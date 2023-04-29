use crate::dtensor::{self, primitives::Tensor};
use wgpu;

const WORKGROUP_SIZE: usize = 64;

// Functional implementation
pub async fn elementary_arithmetic_builder<'op>(
    a: &Tensor<'op>,
    b: &Tensor<'op>,
    entry_point: &str,
    op: &str,
) -> Tensor<'op> {
    if !std::ptr::eq(a.device(), b.device()) {
        panic!("Can't perform operations on Tensors on different devices");
    }

    let wgpu_device = a.device();
    let (device, _) = wgpu_device;

    let output_shape = compute_output_shape(a, b);
    let result = Tensor::of_shape(output_shape, wgpu_device).await;

    let shader_source = generate_wgsl_shader(a, b, &result, entry_point, op);
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    elementary_arithmetic_op(a, b, &result, wgpu_device, &compiled_shader, entry_point);
    result
}

fn elementary_arithmetic_op<'op>(
    a: &Tensor<'op>,
    b: &Tensor<'op>,
    result: &Tensor<'op>,
    wgpu_device: &dtensor::WgpuDevice,
    compiled_shader: &wgpu::ShaderModule,
    entry_point: &str,
) {
    let (device, queue) = wgpu_device;

    let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: None,
        module: compiled_shader,
        entry_point: entry_point,
    });

    let a_bind_group_layout = pipeline.get_bind_group_layout(0);
    let a_bind_group = tensor_as_input(a, &a_bind_group_layout, device);

    let b_bind_group_layout = pipeline.get_bind_group_layout(1);
    let b_bind_group = tensor_as_input(b, &b_bind_group_layout, device);

    let result_bind_group_layout = pipeline.get_bind_group_layout(2);
    let result_bind_group = tensor_as_input(result, &result_bind_group_layout, device);

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        let mut cpass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor { label: None });

        cpass.set_pipeline(&pipeline);

        cpass.set_bind_group(0, &a_bind_group, &[]);
        cpass.set_bind_group(1, &b_bind_group, &[]);
        cpass.set_bind_group(2, &result_bind_group, &[]);

        cpass.dispatch_workgroups(result.len() as u32, 1, 1);
    }
    queue.submit(Some(encoder.finish()));
}

// Utility functions
fn compute_output_shape(a: &Tensor, b: &Tensor) -> Vec<usize> {
    a.shape
        .iter()
        .zip(b.shape.iter())
        .map(|(a, b)| std::cmp::max(*a, *b))
        .collect()
}

fn tensor_as_input(
    tensor: &Tensor,
    bind_group_layout: &wgpu::BindGroupLayout,
    device: &wgpu::Device,
) -> wgpu::BindGroup {
    device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: bind_group_layout,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: tensor.data.as_entire_binding(),
        }],
    })
}

fn generate_wgsl_shader(
    _a: &Tensor,
    _b: &Tensor,
    result: &Tensor,
    entry_point: &str,
    op: &str,
) -> String {
    "".to_string()
        + "
@group(0) @binding(0) var<storage, read> left: array<f32>;

@group(1) @binding(0) var<storage, read> right: array<f32>;

@group(2) @binding(0) var<storage, read_write> result: array<f32>;
const result_size: u32 = "
        + &result.len().to_string()
        + "u;

@compute @workgroup_size("
        + &WORKGROUP_SIZE.to_string()
        + ", 1, 1)
fn " + entry_point
        + "(@builtin(global_invocation_id) global_id: vec3u) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= result_size) {
    return;
  }

  let index = global_id.x;
  result[index] = left[index] "
        + op
        + " right[index];
}
"
}
