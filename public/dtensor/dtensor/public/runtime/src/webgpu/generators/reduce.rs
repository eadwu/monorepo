use tensor::primitives::tensor::ReduceType;
use tensor::primitives::tensor::ViewType;

use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

fn build_webgpu_operation<'a>(op: ReduceType) -> impl Fn(&'a str, &'a str) -> String {
    match op {
        ReduceType::MAX => |accumulator, current| format!("max({}, {})", accumulator, current),
        ReduceType::SUM => |accumulator, current| format!("{} + {}", accumulator, current),
    }
}

pub fn build_shader(op: ReduceType, axis: ViewType) -> String {
    let operation = build_webgpu_operation(op);

    format!(
        "
{header}

{workgroup_stride}

const AXIS: u32 = {axis}u;

{input_interface}

{output_interface}

@compute {workgroup_size}
fn {entry_point}(
    @builtin(global_invocation_id) global_id: vec3u
) {{
    {index}

    // Guard against out-of-bounds work group sizes
    if index >= output_metadata.length {{
        return;
    }}

    // Essentially map indices without AXIS
    var mapped_index_temp = index;
    var mapped_index = 0u;
    for (var i = 0u; i < AXIS; i++) {{
        let output_contiguous_stride = output_metadata.metadata[output_metadata.contiguous_stride_offset + i];
        let output_shape = output_metadata.metadata[output_metadata.shape_offset + i];

        let input_stride = input_metadata.metadata[input_metadata.stride_offset + i];
        let input_shape = input_metadata.metadata[input_metadata.shape_offset + i];

        let index_at_dimension = mapped_index_temp / output_contiguous_stride;
        mapped_index_temp -= (index_at_dimension % output_shape) * output_contiguous_stride;
        mapped_index += (index_at_dimension % input_shape) * input_stride;
    }}

    // Assumption is keep_dims is always true for computation, then squeezed out later if needed
    for (var i = AXIS + 1u; i < output_metadata.dimension; i++) {{
        let output_contiguous_stride = output_metadata.metadata[output_metadata.contiguous_stride_offset + i];
        let output_shape = output_metadata.metadata[output_metadata.shape_offset + i];

        let input_stride = input_metadata.metadata[input_metadata.stride_offset + i];
        let input_shape = input_metadata.metadata[input_metadata.shape_offset + i];

        let index_at_dimension = mapped_index_temp / output_contiguous_stride;
        mapped_index_temp -= (index_at_dimension % output_shape) * output_contiguous_stride;
        mapped_index += (index_at_dimension % input_shape) * input_stride;
    }}

    let axis_rank = input_metadata.metadata[input_metadata.shape_offset + AXIS];
    let axis_stride = input_metadata.metadata[input_metadata.stride_offset + AXIS];
    var reduction: f32 = f32(0);
    for (var i = 0u; i < axis_rank; i++) {{
        let mapped_axis_index = mapped_index + i * axis_stride;
        reduction = {operation};
    }}

    output[index] = reduction;
}}
",
        header = shader_header(),
        workgroup_stride = WORKGROUP_SIZE.serialize_strides("WORKGROUP_STRIDE"),
        input_interface = tensor_interface("0", "read", "input", "array<f32>", "input_metadata"),
        output_interface =
            tensor_interface("1", "read_write", "output", "array<f32>", "output_metadata"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        operation = operation("reduction", "input[mapped_axis_index]"),
    )
}
