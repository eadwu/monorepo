use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

#[cfg(feature = "dtensor_spirv_passthrough")]
use spirv_tools::assembler::Assembler;
#[cfg(feature = "dtensor_spirv_passthrough")]
use spirv_tools::opt::Optimizer;
#[cfg(feature = "dtensor_spirv_passthrough")]
use spirv_tools::val::Validator;
use tensor::ir::mlir::{ShaderIRBuilder, ShaderIREvaluation, ShaderIROp};
use tensor::primitives::tensor::{OperationSpec, Tensor, TensorInput};
use tensor::topograph::{GraphDependencies, GraphView};

use crate::webgpu::benchmark;
use crate::webgpu::generators::{self, compute_index, wgsl_from_tensortype};
use crate::webgpu::{
    ToWebGPUBindGroup, ToWebGPUTensorLayout, WebGPUDevice, WebGPUTensor, WebGPUWorkGroup,
    WORKGROUP_SIZE,
};

pub trait WebGPUEvaluation {
    fn evaluate_webgpu(&self, wgpu_device: &WebGPUDevice) -> Tensor;
}

#[derive(Debug)]
pub struct WebGPUPipeline<'a> {
    pub shader: &'a wgpu::ShaderModule,
    pub inputs: &'a [&'a Tensor],
    pub output: &'a Tensor,
    pub dispatch_workgroups: &'a WebGPUWorkGroup,
}

impl WebGPUEvaluation for Tensor {
    fn evaluate_webgpu(&self, wgpu_device: &WebGPUDevice) -> Tensor {
        let WebGPUDevice { device, queue: _ } = wgpu_device;
        // Ensure output is a contiguous Tensor
        let output = self.Identity();

        let runtime = output.linearize();
        let mut intermediate_results = HashMap::new();

        let lookup = runtime
            .iter()
            .map(|tensor| (tensor.id(), tensor.clone()))
            .collect::<HashMap<_, _>>();

        let lifetimes = runtime
            .iter()
            .flat_map(|tensor| {
                tensor
                    .dependencies()
                    .iter()
                    .map(|input| (input.id(), tensor.id()))
                    .collect::<Vec<_>>()
            })
            .collect::<HashMap<_, _>>();

        #[cfg(feature = "dtensor_shader_collapse")]
        let runtime = &runtime[runtime.len() - 1..];
        for tensor in &runtime[..] {
            if let TensorInput::NoOp(input) = tensor.data() {
                let precomputed: &Tensor = intermediate_results.get(&input.id()).unwrap();
                let _ = tensor.update(&precomputed.data());
                intermediate_results.insert(tensor.id(), tensor.clone());
            } else if let TensorInput::ExplicitInput(_) = tensor.data() {
                intermediate_results.insert(tensor.id(), tensor.clone());
            } else if let TensorInput::OperationResult(operation) = tensor.data() {
                let workgroups = Into::<WebGPUWorkGroup>::into(tensor.view());

                #[cfg(feature = "dtensor_shader_stitch")]
                let (shader, dependencies) = {
                    let (shader, inputs) = match operation {
                        OperationSpec::UnaryOp(op) => {
                            let input = intermediate_results.get(&op.input.id()).unwrap();

                            (
                                generators::unary::build_shader(op.op, input, tensor, &workgroups),
                                vec![op.input.id()],
                            )
                        }
                        OperationSpec::BinaryOp(op) => {
                            let lhs = intermediate_results.get(&op.lhs.id()).unwrap();
                            let rhs = intermediate_results.get(&op.rhs.id()).unwrap();

                            (
                                generators::binary::build_shader(
                                    op.op,
                                    lhs,
                                    rhs,
                                    tensor,
                                    &workgroups,
                                ),
                                vec![op.lhs.id(), op.rhs.id()],
                            )
                        }
                        OperationSpec::ReduceOp(op) => {
                            let input = intermediate_results.get(&op.input.id()).unwrap();

                            (
                                generators::reduce::build_shader(
                                    op.op,
                                    &op.axes[..],
                                    input,
                                    tensor,
                                    &workgroups,
                                ),
                                vec![op.input.id()],
                            )
                        }
                    };

                    let dependencies = inputs
                        .iter()
                        .map(|tensor_id| {
                            assert!(
                                lookup.contains_key(tensor_id),
                                "Expected Tensor {} to be computed by Tensor {}",
                                tensor_id,
                                tensor.id()
                            );

                            lookup.get(tensor_id).unwrap()
                        })
                        .collect::<Vec<_>>();

                    (shader, dependencies)
                };

                #[cfg(not(feature = "dtensor_shader_stitch"))]
                let (shader, dependencies) = {
                    let shader_ir = tensor.build_shader_ir();
                    let mut dependencies = shader_ir
                        .linearize()
                        .iter()
                        .filter(|ir| match ir.op() {
                            ShaderIROp::Load => true,
                            _ => false,
                        })
                        .map(|ir| match ir.evaltype() {
                            Some(ShaderIREvaluation::I32(tensor_id)) => tensor_id as u32,
                            _ => panic!(),
                        })
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .map(|tensor_id| lookup.get(&tensor_id).unwrap())
                        .collect::<Vec<_>>();

                    let shader = format!(
                    "
                    {input_interface}

                    {output_interface}

                    {workgroup_stride}
                    @compute {workgroup_size}
                    fn main(
                        @builtin(global_invocation_id) global_id: vec3u
                    ) {{
                        {index}

                        // Guard against out-of-bounds work group sizes
                        if index >= {output_length}u {{
                            return;
                        }}

                        // Bypass checks
                        {check_bypass}

                        {shader_body}
                    }}",
                    input_interface = dependencies
                        .iter()
                        .enumerate()
                        .map(
                            |(index, input)| Into::<WebGPUTensor>::into(*input).serialize_type(
                                &wgsl_from_tensortype(input.datatype()),
                                &index.to_string(),
                                "read"
                            )
                        )
                        .collect::<Vec<_>>()
                        .join("\n"),
                    output_interface = Into::<WebGPUTensor>::into(tensor).serialize_type(
                        &wgsl_from_tensortype(tensor.datatype()),
                        &dependencies.len().to_string(),
                        "read_write"
                    ),
                    workgroup_stride = workgroups.serialize_strides("WORKGROUP_STRIDE"),
                    workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
                    index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
                    output_length = tensor.len(),
                    check_bypass = dependencies
                        .iter()
                        .chain(std::iter::once(&tensor))
                        .map(|tensor| tensor.id())
                        .map(|tensor_id| format!(
                            "let _{id} = tensor_{id}[0]; {output_tensor}[index] = {datatype}(_{id});",
                            id = tensor_id,
                            datatype = wgsl_from_tensortype(tensor.datatype()),
                            output_tensor = Into::<WebGPUTensor>::into(tensor).name()
                        ))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    shader_body = shader_ir.gen_wgsl(),
                );

                    (shader, dependencies)
                };

                #[cfg(feature = "dtensor_spirv_passthrough")]
                let compute_shader = {
                    let mut wgsl_module = naga::front::wgsl::parse_str(&shader).unwrap();
                    naga::compact::compact(&mut wgsl_module);

                    let info = naga::valid::Validator::new(
                        naga::valid::ValidationFlags::all(),
                        naga::valid::Capabilities::all(),
                    )
                    .validate(&wgsl_module)
                    .unwrap();

                    let spirv_module = naga::back::spv::write_vec(
                        &wgsl_module,
                        &info,
                        &naga::back::spv::Options::default(),
                        Some(&naga::back::spv::PipelineOptions {
                            shader_stage: naga::ShaderStage::Compute,
                            entry_point: "main".to_string(),
                        }),
                    )
                    .unwrap();

                    let mut opt =
                        spirv_tools::opt::create(Some(spirv_tools::TargetEnv::Vulkan_1_2));
                    opt.register_pass(spirv_tools::opt::Passes::UnifyConstant);
                    opt.register_pass(spirv_tools::opt::Passes::InlineExhaustive);
                    opt.register_pass(spirv_tools::opt::Passes::LoopPeeling);
                    opt.register_pass(spirv_tools::opt::Passes::LoopUnswitch);
                    opt.register_pass(spirv_tools::opt::Passes::EliminateDeadFunctions);
                    opt.register_pass(spirv_tools::opt::Passes::EliminateDeadConstant);
                    opt.register_pass(spirv_tools::opt::Passes::CodeSinking);
                    opt.register_pass(spirv_tools::opt::Passes::DeadVariableElimination);
                    opt.register_pass(spirv_tools::opt::Passes::AggressiveDCE);
                    opt.register_pass(spirv_tools::opt::Passes::FoldSpecConstantOpAndComposite);
                    opt.register_pass(spirv_tools::opt::Passes::Simplification);
                    opt.register_pass(spirv_tools::opt::Passes::StrengthReduction);
                    opt.register_performance_passes();
                    opt.register_pass(spirv_tools::opt::Passes::LocalRedundancyElimination);
                    opt.register_pass(spirv_tools::opt::Passes::RedundancyElimination);
                    opt.register_pass(spirv_tools::opt::Passes::RedundantLineInfoElim);
                    opt.register_pass(spirv_tools::opt::Passes::RemoveDuplicates);
                    #[cfg(feature = "dtensor_spirv_passthrough_f16")]
                    opt.register_pass(spirv_tools::opt::Passes::RelaxFloatOps);
                    #[cfg(feature = "dtensor_spirv_passthrough_f16")]
                    opt.register_pass(spirv_tools::opt::Passes::ConvertRelaxedToHalf);
                    opt.register_size_passes();

                    let spirv_opt = opt
                        .optimize(
                            spirv_module.clone(),
                            &mut crate::webgpu::utils::spirv::Callback {},
                            None,
                        )
                        .unwrap();

                    let validator = spirv_tools::val::create(None);
                    validator
                        .validate(
                            spirv_opt.as_words(),
                            Some(spirv_tools::val::ValidatorOptions::default()),
                        )
                        .unwrap();

                    let assembler = spirv_tools::assembler::create(None);
                    let spirv_text = assembler
                        .disassemble(
                            spirv_opt.as_words(),
                            spirv_tools::assembler::DisassembleOptions::default(),
                        )
                        .unwrap()
                        .unwrap();

                    let bytes: &[u8] = bytemuck::cast_slice(&spirv_module[..]);
                    unsafe {
                        device.create_shader_module_spirv(&wgpu::ShaderModuleDescriptorSpirV {
                            label: None,
                            source: Cow::Borrowed(spirv_opt.as_words()),
                        })
                    }
                };

                #[cfg(not(feature = "dtensor_spirv_passthrough"))]
                let compute_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
                    label: None,
                    source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(&shader)),
                });

                let result = webgpu_tensor_pipeline(
                    &WebGPUPipeline {
                        shader: &compute_shader,
                        inputs: &dependencies,
                        output: tensor,
                        dispatch_workgroups: &workgroups,
                    },
                    &wgpu_device,
                );
                let _ = tensor.update(&result.data());
                intermediate_results.insert(tensor.id(), tensor.clone());

                dependencies
                    .iter()
                    .map(|tensor| tensor.id())
                    .for_each(|tensor_id| {
                        if let Some(&last_tensor_id) = lifetimes.get(&tensor_id) {
                            if tensor.id() == last_tensor_id {
                                intermediate_results.remove(&tensor_id);
                            }
                        }
                    });
            } else {
                panic!("Found {:?}, which should be impossible", tensor.data());
            }
        }

        intermediate_results.remove(&output.id()).unwrap()
    }
}

pub fn webgpu_tensor_pipeline<'a>(
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

    let bind_group_layouts_entries = inputs
        .iter()
        .map(|tensor| wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::COMPUTE,
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Storage { read_only: true },
                has_dynamic_offset: false,
                min_binding_size: None,
            },
            count: None,
        })
        .chain(std::iter::once(wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::COMPUTE,
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Storage { read_only: false },
                has_dynamic_offset: false,
                min_binding_size: None,
            },
            count: None,
        }))
        .collect::<Vec<_>>();

    let bind_group_layouts = bind_group_layouts_entries
        .into_iter()
        .map(|entry| {
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[entry],
            })
        })
        .collect::<Vec<_>>();

    let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: Some(
            &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: None,
                bind_group_layouts: &bind_group_layouts.iter().collect::<Vec<_>>()[..],
                push_constant_ranges: &[],
            }),
        ),
        module: &shader,
        entry_point: "main",
    });

    let tensors = inputs
        .iter()
        .chain(std::iter::once(output))
        .collect::<Vec<_>>();

    let tensor_layouts = tensors
        .iter()
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

    #[cfg(feature = "wgpu_benchmark")]
    let encoder_timestamps =
        benchmark::WebGPUTimestamps::new(benchmark::WebGPUEncoderTimestamps::size(), wgpu_device);
    #[cfg(feature = "wgpu_benchmark")]
    let compute_timestamps = benchmark::WebGPUTimestamps::new(
        benchmark::WebGPUComputePassTimestamps::size(),
        wgpu_device,
    );

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

    #[cfg(feature = "wgpu_benchmark")]
    encoder.write_timestamp(
        &encoder_timestamps.query_set,
        benchmark::WebGPUEncoderTimestamps::Start as _,
    );

    {
        #[cfg(not(feature = "wgpu_benchmark"))]
        let mut workload = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: None,
            timestamp_writes: None,
        });
        #[cfg(feature = "wgpu_benchmark")]
        let mut workload = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: None,
            timestamp_writes: Some(wgpu::ComputePassTimestampWrites {
                query_set: &compute_timestamps.query_set,
                beginning_of_pass_write_index: Some(
                    benchmark::WebGPUComputePassTimestamps::Start as _,
                ),
                end_of_pass_write_index: Some(benchmark::WebGPUComputePassTimestamps::End as _),
            }),
        });

        workload.set_pipeline(&pipeline);

        bind_groups
            .iter()
            .enumerate()
            .for_each(|(index, bind_group)| {
                workload.set_bind_group(index as u32, &bind_group, &[])
            });

        #[cfg(feature = "wgpu_benchmark")]
        workload.write_timestamp(
            &encoder_timestamps.query_set,
            benchmark::WebGPUEncoderTimestamps::ComputePassConfigured as _,
        );

        workload.dispatch_workgroups(
            dispatch_workgroups.x / WORKGROUP_SIZE.x + 1,
            dispatch_workgroups.y / WORKGROUP_SIZE.y + 1,
            dispatch_workgroups.z / WORKGROUP_SIZE.z + 1,
        );
    }

    #[cfg(feature = "wgpu_benchmark")]
    encoder.write_timestamp(
        &encoder_timestamps.query_set,
        benchmark::WebGPUEncoderTimestamps::ComputePassFinished as _,
    );

    #[cfg(feature = "wgpu_benchmark")]
    encoder.write_timestamp(
        &encoder_timestamps.query_set,
        benchmark::WebGPUEncoderTimestamps::OutputCopyToCpuStart as _,
    );

    let output_layout = tensor_layouts.last().unwrap();
    let output_buffer = &output_layout.data;
    let size = output_buffer.size();

    #[cfg(feature = "wgpu_direct_buffer")]
    let staging_buffer = output_buffer;
    #[cfg(not(feature = "wgpu_direct_buffer"))]
    let staging_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });
    #[cfg(not(feature = "wgpu_direct_buffer"))]
    encoder.copy_buffer_to_buffer(output_buffer, 0, &staging_buffer, 0, size);

    #[cfg(feature = "wgpu_benchmark")]
    encoder.write_timestamp(
        &encoder_timestamps.query_set,
        benchmark::WebGPUEncoderTimestamps::OutputCopyToCpuEnd as _,
    );

    #[cfg(feature = "wgpu_benchmark")]
    encoder.write_timestamp(
        &encoder_timestamps.query_set,
        benchmark::WebGPUEncoderTimestamps::End as _,
    );

    #[cfg(feature = "wgpu_benchmark")]
    let resolved_encoder_timestamps =
        encoder_timestamps.resolve_query_set(&mut encoder, wgpu_device);
    #[cfg(feature = "wgpu_benchmark")]
    let resolved_compute_timestamps =
        compute_timestamps.resolve_query_set(&mut encoder, wgpu_device);

    queue.submit(std::iter::once(encoder.finish()));

    let buffer_slice = staging_buffer.slice(..);
    buffer_slice.map_async(wgpu::MapMode::Read, |_| {});
    device.poll(wgpu::Maintain::wait()).panic_on_timeout();

    #[cfg(feature = "wgpu_benchmark")]
    {
        let period = queue.get_timestamp_period();
        let elapsed_us = |start, end: u64| end.wrapping_sub(start) as f64 * period as f64 / 1000.0;

        let encoder_timestamps =
            encoder_timestamps.read_results(&resolved_encoder_timestamps, wgpu_device);
        let compute_timestamps =
            compute_timestamps.read_results(&resolved_compute_timestamps, wgpu_device);

        let encoder_timeline = &encoder_timestamps[1..]
            .iter()
            .map(|&end| elapsed_us(encoder_timestamps[0], end))
            .map(|us| format!("{:.2}", us))
            .collect::<Vec<_>>();

        println!("PIPELINE: {}", encoder_timeline.join(" | ") + " μs");
        println!(
            "COMPUTE: {:.2} μs",
            elapsed_us(compute_timestamps[0], compute_timestamps[1])
        );
    }

    // Gets contents of buffer
    let data = buffer_slice.get_mapped_range();

    // Returns data from buffer
    let data_len_bytes = output.len() as usize * output.datatype().byte_size();
    let output_tensor = Tensor::from_raw_bytes(
        &data[..data_len_bytes],
        output.view().clone(),
        output.datatype(),
    );

    // With the current interface, we have to make sure all mapped views are
    // dropped before we unmap the buffer.
    drop(data);
    staging_buffer.unmap(); // Unmaps buffer from memory
                            // If you are familiar with C++ these 2 lines can be thought of similarly to:
                            //   delete myPointer;
                            //   myPointer = NULL;
                            // It effectively frees the memory

    output_tensor
}
