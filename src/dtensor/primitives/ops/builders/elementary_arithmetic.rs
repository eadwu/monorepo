use crate::dtensor::{self, primitives::ops::builders, primitives::Tensor};
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

    // Strided Tensors are unlikely to be friendly to cache
    let contiguous_a = a.as_contiguous().await;
    let contiguous_b = b.as_contiguous().await;

    let wgpu_device = contiguous_a.device();
    let (device, _) = wgpu_device;

    let output_shape = compute_output_shape(&contiguous_a, &contiguous_b);
    let result = Tensor::of_shape(&output_shape, wgpu_device).await;

    let pipeline_descriptor = builders::TensorOpDescriptor {
        inputs: &[
            builders::TensorDescriptor {
                name: "left",
                tensor: &contiguous_a,
            },
            builders::TensorDescriptor {
                name: "right",
                tensor: &contiguous_b,
            },
        ],
        output: builders::TensorDescriptor {
            name: "result",
            tensor: &result,
        },
        entry_point: entry_point,
    };

    let shader_source = generate_wgsl_shader(&pipeline_descriptor, op);
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    builders::build_op_pipeline(&pipeline_descriptor, &compiled_shader, wgpu_device);
    result
}

// Utility functions
fn compute_output_shape(a: &Tensor, b: &Tensor) -> Vec<usize> {
    a.shape
        .iter()
        .zip(b.shape.iter())
        .map(|(a, b)| std::cmp::max(*a, *b))
        .collect()
}

fn generate_wgsl_shader(pipeline_descriptor: &builders::TensorOpDescriptor, op: &str) -> String {
    format!(
        "
{shader_interface}

@compute @workgroup_size({workgroup_size}, 1, 1)
fn {entry_point}(@builtin(global_invocation_id) global_id: vec3u) {{
  // Guard against out-of-bounds work group sizes
  if (global_id.x >= result_metadata.length) {{
    return;
  }}

  {workarounds}

  let index: u32 = global_id.x;
  result[index] = left[index] {op} right[index];
}}
",
        shader_interface = builders::define_shader_interface(
            pipeline_descriptor.inputs,
            &pipeline_descriptor.output
        ),
        workgroup_size = WORKGROUP_SIZE,
        entry_point = pipeline_descriptor.entry_point,
        workarounds = builders::shader_workaround_1976(pipeline_descriptor),
        op = op,
    )
}
