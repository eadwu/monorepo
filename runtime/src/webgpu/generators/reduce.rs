use tensor::primitives::tensor::ReduceType;
use tensor::primitives::tensorview::ViewType;

use crate::webgpu::generators::*;
use crate::webgpu::WebGPUWorkGroup;
use crate::webgpu::WORKGROUP_SIZE;

fn build_webgpu_operation<'a>(op: ReduceType) -> impl Fn(&'a str, &'a str) -> String {
    match op {
        ReduceType::MAX => |accumulator, current| format!("max({}, {})", accumulator, current),
        ReduceType::SUM => |accumulator, current| format!("{} + {}", accumulator, current),
    }
}

pub fn build_shader(
    op: ReduceType,
    axis: ViewType,
    datatype: TensorType,
    workgroups: &WebGPUWorkGroup,
) -> String {
    let container_type = format!(
        "array<{datatype}>",
        datatype = wgsl_from_tensortype(datatype)
    );

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
    // Coordinates are the same from output to input, except with one dimension flattened
    // Meaning [..AXIS] strides are different
    for (var i = 0u; i < AXIS; i++) {{
        let output_continguous_stride = output_metadata.metadata[output_metadata.contiguous_stride_offset + i];
        let input_continguous_stride = input_metadata.metadata[input_metadata.contiguous_stride_offset + i];

        let index_at_dimension = mapped_index_temp / output_continguous_stride;
        mapped_index_temp %= output_continguous_stride;
        mapped_index += index_at_dimension * input_continguous_stride;
    }}
    // While [AXIS+1..] are the same
    mapped_index += mapped_index_temp;

    let axis_rank = input_metadata.metadata[input_metadata.shape_offset + AXIS];
    let axis_stride = input_metadata.metadata[input_metadata.contiguous_stride_offset + AXIS];

    var mapped_axis_index = mapped_index;
    {map_axis_index}

    var reduction = input[mapped_index];
    for (var i = 1u; i < axis_rank; i++) {{
        mapped_axis_index = mapped_index + i * axis_stride;
        {map_axis_index}

        reduction = {operation};
    }}

    output[index] = reduction;
}}
",
        header = shader_header(),
        workgroup_stride = workgroups.serialize_strides("WORKGROUP_STRIDE"),
        input_interface = tensor_interface("0", "read", "input", &container_type, "input_metadata"),
        output_interface = tensor_interface(
            "1",
            "read_write",
            "output",
            &container_type,
            "output_metadata"
        ),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        map_axis_index = map_index(
            "mapped_axis_index",
            "input_metadata"
        ),
        operation = build_webgpu_operation(op)("reduction", "input[mapped_axis_index]"),
    )
}
