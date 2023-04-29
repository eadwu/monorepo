use crate::dtensor::primitives::Tensor;
use wgpu;

const ENTRY_POINT: &str = "add";
const WORKGROUP_SIZE: usize = 64;

// Functional implementation
pub async fn add<'add>(a: &Tensor<'add>, b: &Tensor<'add>) -> Tensor<'add> {
    if !std::ptr::eq(a.device(), b.device()) {
        panic!("Can't perform operations on Tensors on different devices");
    }

    let wgpu_device = a.device();
    let (device, queue) = wgpu_device;

    let output_shape = compute_output_shape(a, b);
    let sum = Tensor::of_shape(output_shape, wgpu_device).await;

    let shader_source = generate_wgsl_shader(a, b, &sum);
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: None,
        module: &compiled_shader,
        entry_point: ENTRY_POINT,
    });

    let a_bind_group_layout = pipeline.get_bind_group_layout(0);
    let a_bind_group = tensor_as_input(a, &a_bind_group_layout, device);

    let b_bind_group_layout = pipeline.get_bind_group_layout(1);
    let b_bind_group = tensor_as_input(b, &b_bind_group_layout, device);

    let sum_bind_group_layout = pipeline.get_bind_group_layout(2);
    let sum_bind_group = tensor_as_input(&sum, &sum_bind_group_layout, device);

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        let mut cpass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor { label: None });

        cpass.set_pipeline(&pipeline);

        cpass.set_bind_group(0, &a_bind_group, &[]);
        cpass.set_bind_group(1, &b_bind_group, &[]);
        cpass.set_bind_group(2, &sum_bind_group, &[]);

        cpass.dispatch_workgroups(sum.len() as u32, 1, 1);
    }
    queue.submit(Some(encoder.finish()));

    sum
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

fn generate_wgsl_shader(_a: &Tensor, _b: &Tensor, sum: &Tensor) -> String {
    "".to_string()
        + "
@group(0) @binding(0) var<storage, read> left: array<f32>;

@group(1) @binding(0) var<storage, read> right: array<f32>;

@group(2) @binding(0) var<storage, read_write> sum: array<f32>;
const sum_size: u32 = "
        + &sum.len().to_string()
        + "u;

@compute @workgroup_size("
        + &WORKGROUP_SIZE.to_string()
        + ", 1, 1)
fn " + ENTRY_POINT
        + "(@builtin(global_invocation_id) global_id: vec3u) {
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= sum_size) {
    return;
  }

  let index = global_id.x;
  sum[index] = left[index] + right[index];
}
"
}
