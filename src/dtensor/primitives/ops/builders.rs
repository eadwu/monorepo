pub mod elementary_arithmetic;
pub mod elementwise_functions;

use crate::dtensor::{self, primitives::Tensor};
use wgpu::{self, util::DeviceExt};

const DUMMY_VAR: &str = "_dummy";

pub struct TensorDescriptor<'a> {
    pub name: &'a str,
    pub tensor: &'a Tensor,
}

pub struct TensorOpDescriptor<'a> {
    pub inputs: &'a [TensorDescriptor<'a>],
    pub output: TensorDescriptor<'a>,
    pub entry_point: &'a str,
}

// Operation Pipeline
pub fn shader_workaround_1976(descriptor: &TensorOpDescriptor) -> String {
    let generated_workarounds = descriptor
        .inputs
        .iter()
        .chain(std::iter::once(&descriptor.output))
        .map(|tensor_descriptor| workaround_1976(DUMMY_VAR, tensor_descriptor))
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "
  // Ensure every binding is used
  // https://github.com/gfx-rs/wgpu/issues/1976
  // https://github.com/gpuweb/gpuweb/issues/1806
  var {var}: f32 = 0.0;
  {generated_workarounds}
",
        var = DUMMY_VAR,
        generated_workarounds = generated_workarounds
    )
}

pub fn build_op_pipeline(
    descriptor: &TensorOpDescriptor,
    compiled_shader: &wgpu::ShaderModule,
    web_gpu: &dtensor::WebGPU,
) {
    let dtensor::WebGPU { device, queue } = web_gpu;

    let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: None,
        module: compiled_shader,
        entry_point: descriptor.entry_point,
    });

    let bind_groups = descriptor
        .inputs
        .iter()
        .chain(std::iter::once(&descriptor.output))
        .enumerate()
        .map(|(index, tensor_descriptor)| {
            tensor_descriptor
                .tensor
                .bind_group(&pipeline.get_bind_group_layout(index as u32))
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

        workload.dispatch_workgroups(descriptor.output.tensor.len() as u32, 1, 1);
    }
    queue.submit(Some(encoder.finish()));
}

// Utility Functions
pub fn define_shader_interface<'a>(
    inputs: &[TensorDescriptor<'a>],
    output: &TensorDescriptor<'a>,
) -> String {
    let input_interface = (0..inputs.len())
        .into_iter()
        .map(|i| self::tensor_interface(&i.to_string(), inputs[i].name, "read"))
        .collect::<Vec<String>>()
        .join("\n");
    let output_interface =
        self::tensor_interface(&inputs.len().to_string(), output.name, "read_write");

    format!(
        "
struct TensorMetadata {{
  rank: u32,
  length: u32,
}}

{input_interface}

{output_interface}
  ",
        input_interface = input_interface,
        output_interface = output_interface,
    )
}

pub fn map_offset(initial_index_var: &str, input_name: &str) -> String {
    format!("
// Map index from the contiguous result offset to the strided input offset
var {input_name}_contiguous_offset: u32 = {index_var};
var {input_name}_mapped_offset: u32 = 0u;
for (var i = 0u; i < {input_name}_metadata.rank; i++) {{
    let index_at_dimension = ({input_name}_contiguous_offset / {input_name}_contiguous_stride[i]) % {input_name}_shape[i];
    {input_name}_mapped_offset = {input_name}_mapped_offset + (index_at_dimension * {input_name}_stride[i]);
    {input_name}_contiguous_offset = {input_name}_contiguous_offset % {input_name}_contiguous_stride[i];
}}
", input_name=input_name, index_var=initial_index_var)
}

// Helper Functions
fn tensor_interface(group: &str, name: &str, permission: &str) -> String {
    format!(
        "
@group({group}) @binding(0) var<storage, {permission}> {name}: array<f32>;
@group({group}) @binding(1) var<storage, read> {name}_shape: array<u32>;
@group({group}) @binding(2) var<storage, read> {name}_stride: array<u32>;
@group({group}) @binding(3) var<storage, read> {name}_contiguous_stride: array<u32>;
@group({group}) @binding(4) var<uniform> {name}_metadata: TensorMetadata;
",
        group = group,
        name = name,
        permission = permission,
    )
}

fn workaround_1976(var: &str, descriptor: &TensorDescriptor) -> String {
    format!(
        "
  {var} = f32({name}[0]);
  {var} = f32({name}_shape[0]);
  {var} = f32({name}_stride[0]);
  {var} = f32({name}_contiguous_stride[0]);
  {var} = f32({name}_metadata.rank);
",
        var = var,
        name = descriptor.name
    )
}
