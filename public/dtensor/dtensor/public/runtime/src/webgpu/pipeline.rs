use std::{borrow::Cow, collections::HashMap, future::Future};

use tensor::primitives::tensor::{OperationSpec, Tensor, TensorInput, TensorType};
use tensor::primitives::tensorview::TensorView;

use crate::{webgpu::WORKGROUP_SIZE, GraphView};

use super::{generators, ToWebGPUBindGroup, ToWebGPUTensorLayout, WebGPUDevice, WebGPUWorkGroup};

pub trait WebGPUEvaluation {
    fn evaluate_webgpu(&self, wgpu_device: &WebGPUDevice) -> impl Future<Output = Tensor>;
}

#[derive(Debug)]
pub struct WebGPUPipeline<'a> {
    pub shader: &'a str,
    pub inputs: &'a [&'a Tensor],
    pub output: &'a Tensor,
    pub dispatch_workgroups: &'a WebGPUWorkGroup,
}

impl WebGPUEvaluation for Tensor {
    async fn evaluate_webgpu(&self, wgpu_device: &WebGPUDevice) -> Tensor {
        let runtime = self.as_runtime_graph();
        let mut intermediate_results = HashMap::new();

        for tensor in &runtime.graph {
            if let TensorInput::NoOp(input) = tensor.data() {
                let input: &Tensor = intermediate_results.get(&input.id()).unwrap();
                let clone =
                    Tensor::from_raw_bytes(&input.load(), tensor.view().clone(), tensor.datatype());
                intermediate_results.insert(tensor.id(), clone);
            } else if let TensorInput::ExplicitInput(_) = tensor.data() {
                intermediate_results.insert(tensor.id(), tensor.clone());
            } else if let TensorInput::OperationResult(operation) = tensor.data() {
                let (shader, inputs, output) = match operation {
                    OperationSpec::UnaryOp(op) => (
                        generators::unary::build_shader(op.op),
                        vec![op.input.id()],
                        tensor,
                    ),
                    OperationSpec::BinaryOp(op) => (
                        generators::binary::build_shader(op.op),
                        vec![op.lhs.id(), op.rhs.id()],
                        tensor,
                    ),
                    OperationSpec::ReduceOp(op) => (
                        generators::reduce::build_shader(op.op, op.axis),
                        vec![op.input.id()],
                        tensor,
                    ),
                    OperationSpec::ViewOp(op) => (
                        generators::view::build_shader(),
                        vec![op.input.id()],
                        tensor,
                    ),
                    _ => panic!("Unsupported Operation {:?}", operation),
                };

                let dependencies = inputs
                    .iter()
                    .map(|tensor_id| {
                        assert!(
                            intermediate_results.contains_key(tensor_id),
                            "Expected Tensor {} to be computed by Tensor {}",
                            tensor_id,
                            tensor.id()
                        );

                        intermediate_results.get(tensor_id).unwrap()
                    })
                    .collect::<Vec<_>>();

                let result = webgpu_tensor_pipeline(
                    &WebGPUPipeline {
                        shader: &shader,
                        inputs: &dependencies,
                        output,
                        dispatch_workgroups: &tensor.view().into(),
                    },
                    &wgpu_device,
                )
                .await;
                intermediate_results.insert(tensor.id(), result);

                inputs.iter().for_each(|tensor_id| {
                    if let Some(&last_tensor_id) = runtime.dependencies.get(tensor_id) {
                        if tensor.id() == last_tensor_id {
                            intermediate_results.remove(tensor_id);
                        }
                    }
                });
            } else {
                panic!("Found {:?}, which should be impossible", tensor.data());
            }
        }

        intermediate_results.remove(&self.id()).unwrap()
    }
}

pub async fn webgpu_tensor_pipeline<'a>(
    pipeline: &WebGPUPipeline<'a>,
    wgpu_device: &WebGPUDevice,
) -> Tensor {
    let WebGPUDevice { device, queue } = wgpu_device;
    let WebGPUPipeline {
        shader,
        inputs,
        output,
        dispatch_workgroups,
    } = pipeline;

    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(shader)),
    });

    let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: None,
        module: &compiled_shader,
        entry_point: "main",
    });

    let tensor_layouts = inputs
        .iter()
        .chain(std::iter::once(output))
        .map(|tensor| tensor.as_webgpu_tensor(wgpu_device))
        .collect::<Vec<_>>();

    let bind_groups = tensor_layouts
        .iter()
        .enumerate()
        .map(|(index, tensor_layout)| {
            tensor_layout
                .as_webgpu_bind_group(&pipeline.get_bind_group_layout(index as u32), wgpu_device)
        })
        .collect::<Vec<_>>();

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        let mut workload = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: None,
            timestamp_writes: None,
        });
        workload.set_pipeline(&pipeline);

        bind_groups
            .iter()
            .enumerate()
            .for_each(|(index, bind_group)| {
                workload.set_bind_group(index as u32, &bind_group, &[])
            });

        workload.dispatch_workgroups(
            dispatch_workgroups.x / WORKGROUP_SIZE.x + 1,
            dispatch_workgroups.y / WORKGROUP_SIZE.y + 1,
            dispatch_workgroups.z / dispatch_workgroups.z + 1,
        );
    }

    let output_layout = tensor_layouts.last().unwrap();
    let output_buffer = &output_layout.data;
    let size = output_buffer.size();

    let staging_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });

    encoder.copy_buffer_to_buffer(output_buffer, 0, &staging_buffer, 0, size);

    queue.submit(std::iter::once(encoder.finish()));

    // Note that we're not calling `.await` here.
    let buffer_slice = staging_buffer.slice(..);
    // Sets the buffer up for mapping, sending over the result of the mapping back to us when it is finished.
    let (sender, receiver) = futures_intrusive::channel::shared::oneshot_channel();
    buffer_slice.map_async(wgpu::MapMode::Read, move |v| sender.send(v).unwrap());

    // Poll the device in a blocking manner so that our future resolves.
    // In an actual application, `device.poll(...)` should
    // be called in an event loop 1or on another thread.
    device.poll(wgpu::Maintain::Wait);

    // Awaits until `buffer_future` can be read from
    if let Some(Ok(())) = receiver.receive().await {
        // Gets contents of buffer
        let data = buffer_slice.get_mapped_range();

        // Returns data from buffer
        let len_bytes = output.len() as usize * output.datatype().byte_size();
        // NOTE: This can not clone the TensorView, for example if the output is some
        // noncontiguous view, it would not be correct since the pipeline
        // output is ALWAYS contiguous, consider this Tensor as a computed `checkpoint`
        let view = TensorView::from_shape(output.shape());
        let output_tensor = Tensor::from_raw_bytes(&data[..len_bytes], view, output.datatype());

        // With the current interface, we have to make sure all mapped views are
        // dropped before we unmap the buffer.
        drop(data);
        staging_buffer.unmap(); // Unmaps buffer from memory
                                // If you are familiar with C++ these 2 lines can be thought of similarly to:
                                //   delete myPointer;
                                //   myPointer = NULL;
                                // It effectively frees the memory

        output_tensor
    } else {
        panic!("failed to run compute on gpu!")
    }
}
