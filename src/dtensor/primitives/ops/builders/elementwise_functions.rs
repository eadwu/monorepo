use crate::dtensor::{self, primitives::Tensor};
use wgpu;

const WORKGROUP_SIZE: usize = 64;

// Functional implementation
pub async fn elementwise_function_builder<'op>(
    a: &Tensor<'op>,
    entry_point: &str,
    elementwise_function: String,
) -> Tensor<'op> {
    let wgpu_device = a.device();
    let (device, _) = wgpu_device;

    let result = Tensor::of_shape(a.shape(), wgpu_device).await;

    let shader_source = generate_wgsl_shader(a, &result, entry_point, elementwise_function);
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    elementwise_function_op(a, &result, wgpu_device, &compiled_shader, entry_point);
    result
}

fn elementwise_function_op<'op>(
    input: &Tensor<'op>,
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

    let input_bind_group_layout = pipeline.get_bind_group_layout(0);
    let input_bind_group = tensor_as_input(input, &input_bind_group_layout, device);

    let result_bind_group_layout = pipeline.get_bind_group_layout(1);
    let result_bind_group = tensor_as_input(result, &result_bind_group_layout, device);

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        let mut cpass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor { label: None });

        cpass.set_pipeline(&pipeline);

        cpass.set_bind_group(0, &input_bind_group, &[]);
        cpass.set_bind_group(1, &result_bind_group, &[]);

        cpass.dispatch_workgroups(result.len() as u32, 1, 1);
    }
    queue.submit(Some(encoder.finish()));
}

// Utility functions
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
    input: &Tensor,
    result: &Tensor,
    entry_point: &str,
    elementwise_function: String,
) -> String {
  // let literal_elementwise_function = format!(elementwise_function, input = "input[index]");
  let literal_elementwise_function = elementwise_function.replace("{input}", "input[index]");

    "".to_string()
        + "
@group(0) @binding(0) var<storage, read> input: array<f32>;

@group(1) @binding(0) var<storage, read_write> result: array<f32>;
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
  result[index] = "
        + &literal_elementwise_function
        + ";
}
"
}
