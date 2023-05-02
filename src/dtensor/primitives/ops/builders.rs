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
            self::create_tensor_bind_group(
                tensor_descriptor.tensor,
                &pipeline.get_bind_group_layout(index as u32),
                device,
            )
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
#[repr(C)]
#[derive(bytemuck::Pod, Copy, Clone, bytemuck::Zeroable)]
struct TensorMetadata {
    rank: u32,
    length: u32,
}

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

pub fn create_tensor_bind_group(
    tensor: &Tensor,
    bind_group_layout: &wgpu::BindGroupLayout,
    device: &wgpu::Device,
) -> wgpu::BindGroup {
    let shape = self::usize_to_u32(tensor.shape());
    let shape_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::cast_slice(&shape),
        usage: wgpu::BufferUsages::STORAGE
            | wgpu::BufferUsages::COPY_DST
            | wgpu::BufferUsages::COPY_SRC,
    });

    let stride = self::usize_to_u32(tensor.stride());
    let stride_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::cast_slice(&stride),
        usage: wgpu::BufferUsages::STORAGE
            | wgpu::BufferUsages::COPY_DST
            | wgpu::BufferUsages::COPY_SRC,
    });

    let contiguous_stride = self::usize_to_u32(tensor.contiguous_stride());
    let contiguous_stride_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::cast_slice(&contiguous_stride),
        usage: wgpu::BufferUsages::STORAGE
            | wgpu::BufferUsages::COPY_DST
            | wgpu::BufferUsages::COPY_SRC,
    });

    let metadata = TensorMetadata {
        rank: tensor.rank() as u32,
        length: tensor.len() as u32,
    };
    let metadata_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::cast_slice(&[metadata]),
        usage: wgpu::BufferUsages::UNIFORM
            | wgpu::BufferUsages::COPY_DST
            | wgpu::BufferUsages::COPY_SRC,
    });

    device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: tensor.buffer().as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: shape_buffer.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: stride_buffer.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 3,
                resource: contiguous_stride_buffer.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 4,
                resource: metadata_buffer.as_entire_binding(),
            },
        ],
    })
}

// Helper Functions
fn usize_to_u32(data: &[usize]) -> Vec<u32> {
    data.iter().map(|&x| x as u32).collect::<Vec<_>>()
}

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
