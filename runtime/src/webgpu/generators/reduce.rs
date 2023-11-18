use tensor::primitives::tensor::{ReduceType, Tensor};
use tensor::primitives::tensorview::ViewType;

use crate::webgpu::WebGPUWorkGroup;
use crate::webgpu::WORKGROUP_SIZE;
use crate::webgpu::{generators::*, WebGPUTensor};

fn build_webgpu_operation<'a>(op: ReduceType) -> impl Fn(&'a str, &'a str) -> String {
    match op {
        ReduceType::MAX => |accumulator, current| format!("max({}, {})", accumulator, current),
        ReduceType::SUM => |accumulator, current| format!("{} + {}", accumulator, current),
    }
}

pub fn build_shader(
    op: ReduceType,
    axis: ViewType,
    input: &Tensor,
    output: &Tensor,
    workgroups: &WebGPUWorkGroup,
) -> String {
    let input_wgpu = Into::<WebGPUTensor>::into(input);
    let output_wgpu = Into::<WebGPUTensor>::into(output);

    format!(
        "
const AXIS: u32 = {axis}u;

{input_interface}

{output_interface}

{workgroup_stride}
@compute {workgroup_size}
fn {entry_point}(
    @builtin(global_invocation_id) global_id: vec3u
) {{
    {index}

    // Guard against out-of-bounds work group sizes
    if index >= {output_tensor_name}.length {{
        return;
    }}

    // Essentially map indices without AXIS
    var mapped_index_temp = index;
    var mapped_index = 0u;
    // Coordinates are the same from output to input, except with one dimension flattened
    // Meaning [..AXIS] strides are different
    for (var i = 0u; i < AXIS; i++) {{
        let output_continguous_stride = {output_tensor_name}.metadata[{output_tensor_name}.contiguous_stride_offset + i];
        let input_continguous_stride = {input_tensor_name}.metadata[{input_tensor_name}.contiguous_stride_offset + i];

        let index_at_dimension = mapped_index_temp / output_continguous_stride;
        mapped_index_temp %= output_continguous_stride;
        mapped_index += index_at_dimension * input_continguous_stride;
    }}
    // While [AXIS+1..] are the same
    mapped_index += mapped_index_temp;

    let axis_rank = {input_tensor_name}.metadata[{input_tensor_name}.shape_offset + AXIS];
    let axis_stride = {input_tensor_name}.metadata[{input_tensor_name}.contiguous_stride_offset + AXIS];

    var mapped_axis_index = mapped_index;
    {map_axis_index}

    var reduction = {input_tensor_name}.data[mapped_index];
    for (var i = 1u; i < axis_rank; i++) {{
        mapped_axis_index = mapped_index + i * axis_stride;
        {map_axis_index}

        reduction = {operation};
    }}

    {output_tensor_name}.data[index] = reduction;
}}
",
        workgroup_stride = workgroups.serialize_strides("WORKGROUP_STRIDE"),
        input_interface = input_wgpu.serialize_type(&wgsl_from_tensortype(input.datatype()), "0", "read"),
        output_interface = output_wgpu.serialize_type(&wgsl_from_tensortype(output.datatype()), "1", "read_write"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        output_tensor_name = output_wgpu.name(),
        input_tensor_name = input_wgpu.name(),
        map_axis_index = map_index("mapped_axis_index", input.viewtracker()),
        operation = {
            let input_data = format!("{}.data[mapped_axis_index]", input_wgpu.name());
            let output = build_webgpu_operation(op)("reduction", &input_data);
            output
        },
    )
}
