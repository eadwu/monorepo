use crate::dtensor::{
    self, primitives::ops::builders, primitives::ops::impl_tensor_op, primitives::Tensor,
};
use wgpu;

const ENTRY_POINT: &str = "reshape";
const WORKGROUP_SIZE: usize = 64;

impl_tensor_op!(Reshape reshape |input: &Tensor, shape: &[usize]| -> Tensor {
    let web_gpu = input.device();
    let dtensor::WebGPU { device, queue: _ } = web_gpu;
    let result = Tensor::of_shape(shape, input.wgpu_device()).await;

    let pipeline_descriptor = builders::TensorOpDescriptor {
        inputs: &[builders::TensorDescriptor {
            name: "input",
            tensor: input,
        }],
        output: builders::TensorDescriptor {
            name: "result",
            tensor: &result,
        },
        entry_point: ENTRY_POINT,
    };

    let shader_source = generate_wgsl_shader(&pipeline_descriptor);
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    builders::build_op_pipeline(&pipeline_descriptor, &compiled_shader, web_gpu);
    result
});

fn generate_wgsl_shader(pipeline_descriptor: &builders::TensorOpDescriptor) -> String {
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

  // Unmapped index
  let index: u32 = global_id.x;

  // Map index from the contiguous result offset to the strided input offset
  var contiguous_offset: u32 = index;
  var mapped_offset: u32 = 0u;
  for (var i = 0u; i < input_metadata.rank; i++) {{
    let input_index = (contiguous_offset / input_contiguous_stride[i]) % input_shape[i];
    mapped_offset = mapped_offset + (input_index * input_stride[i]);
    contiguous_offset = contiguous_offset % input_contiguous_stride[i];
  }}

  result[index] = input[mapped_offset];
}}
",
        shader_interface = builders::define_shader_interface(
            pipeline_descriptor.inputs,
            &pipeline_descriptor.output
        ),
        workgroup_size = WORKGROUP_SIZE,
        entry_point = pipeline_descriptor.entry_point,
        workarounds = builders::shader_workaround_1976(pipeline_descriptor),
    )
}
