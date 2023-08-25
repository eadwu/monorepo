use std::{borrow::Cow, collections::HashMap};

use tensor::primitives::tensor::{GatherParams, IndexType, OperationSpec, Tensor, TensorInput};

use crate::GraphView;

use super::{generators, ToWebGPUBindGroup, ToWebGPUTensorLayout, WebGPUDevice};

const MAXIMUM_DISPATCH_GROUP_DIMENSION: usize = 65535;

#[async_trait::async_trait]
pub trait WebGPUEvaluation {
    async fn evaluate_webgpu(&self, wgpu_device: &WebGPUDevice) -> Tensor;
}

#[derive(Debug)]
pub struct WebGPUPipeline<'a> {
    pub shader: &'a str,
    pub inputs: &'a [&'a Tensor],
    pub output: &'a Tensor,
}

#[async_trait::async_trait]
impl WebGPUEvaluation for Tensor {
    async fn evaluate_webgpu(&self, wgpu_device: &WebGPUDevice) -> Tensor {
        let runtime = self.as_runtime_graph();
        let mut intermediate_results = HashMap::new();

        for tensor in &runtime.graph {
            if let TensorInput::NoOp(input) = tensor.data() {
                intermediate_results.insert(tensor.id(), input.clone());
            } else if let TensorInput::ExplicitInput(_) = tensor.data() {
                intermediate_results.insert(tensor.id(), tensor.clone());
            } else if let TensorInput::OperationResult(operation) = tensor.data() {
                let (shader, inputs) = match operation {
                    OperationSpec::UnaryOp(op) => {
                        (generators::unary::build_shader(op.op), vec![op.input.id()])
                    }
                    OperationSpec::BinaryOp(op) => (
                        generators::binary::build_shader(op.op),
                        vec![op.lhs.id(), op.rhs.id()],
                    ),
                    OperationSpec::ViewOp(op) => {
                        (generators::view::build_shader(), vec![op.input.id()])
                    }
                    OperationSpec::IndexOp(op) => match op.op {
                        IndexType::GatherElements => {
                            let params =
                                bincode::deserialize::<GatherParams>(&op.serialized_params[..])
                                    .unwrap();

                            (
                                generators::index::build_gather_shader(params.axis),
                                vec![params.input, params.indices],
                            )
                        }
                        _ => panic!("ScatterElements has not been implemented"),
                    },
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
                        output: tensor,
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
        let mut workload = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor { label: None });
        workload.set_pipeline(&pipeline);

        bind_groups
            .iter()
            .enumerate()
            .for_each(|(index, bind_group)| {
                workload.set_bind_group(index as u32, &bind_group, &[])
            });

        let elements = output.view().len() as usize;
        let z = elements.min(MAXIMUM_DISPATCH_GROUP_DIMENSION);
        let y = elements / MAXIMUM_DISPATCH_GROUP_DIMENSION + 1;
        workload.dispatch_workgroups(1, y as u32, z as u32);
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
        // Since contents are got in bytes, this converts these bytes back to u32
        let result = bytemuck::cast_slice(&data).to_vec();

        // With the current interface, we have to make sure all mapped views are
        // dropped before we unmap the buffer.
        drop(data);
        staging_buffer.unmap(); // Unmaps buffer from memory
                                // If you are familiar with C++ these 2 lines can be thought of similarly to:
                                //   delete myPointer;
                                //   myPointer = NULL;
                                // It effectively frees the memory

        // Returns data from buffer
        let n_tensor_elements = output.view().len() as usize;
        Tensor::from_contiguous(&result[..n_tensor_elements], &output.view().shape)
    } else {
        panic!("failed to run compute on gpu!")
    }
}
