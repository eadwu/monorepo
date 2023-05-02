use crate::dtensor::{self, primitives::ops::builders, primitives::Tensor};
use wgpu;

const WORKGROUP_SIZE: usize = 64;

// Functional implementation
pub async fn elementwise_function_builder(
    a: &Tensor,
    entry_point: &str,
    elementwise_function: String,
) -> Tensor {
    let web_gpu = a.device();
    let dtensor::WebGPU { device, queue } = web_gpu;
    let result = Tensor::of_shape(a.shape(), a.wgpu_device()).await;

    let pipeline_descriptor = builders::TensorOpDescriptor {
        inputs: &[builders::TensorDescriptor {
            name: "input",
            tensor: a,
        }],
        output: builders::TensorDescriptor {
            name: "result",
            tensor: &result,
        },
        entry_point: entry_point,
    };

    let shader_source = generate_wgsl_shader(&pipeline_descriptor, elementwise_function);
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    builders::build_op_pipeline(&pipeline_descriptor, &compiled_shader, web_gpu);
    result
}

// Utility functions
fn generate_wgsl_shader(
    pipeline_descriptor: &builders::TensorOpDescriptor,
    elementwise_function: String,
) -> String {
    // let literal_elementwise_function = format!(elementwise_function, input = "input[index]");
    let literal_elementwise_function = elementwise_function.replace("{input}", "input[index]");

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
    result[index] = {literal_elementwise_function};
}}
",
        shader_interface = builders::define_shader_interface(
            pipeline_descriptor.inputs,
            &pipeline_descriptor.output
        ),
        workgroup_size = WORKGROUP_SIZE,
        entry_point = pipeline_descriptor.entry_point,
        workarounds = builders::shader_workaround_1976(pipeline_descriptor),
        literal_elementwise_function = literal_elementwise_function,
    )
}
