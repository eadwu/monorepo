use tensor::primitives::tensor::BinaryType;

use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

fn build_webgpu_operation<'a>(op: BinaryType) -> impl Fn(&'a str, &'a str) -> String {
    match op {
        BinaryType::ADD => |lhs, rhs| format!("{} + {}", lhs, rhs),
        BinaryType::DIVIDE => |lhs, rhs| format!("{} / {}", lhs, rhs),
        BinaryType::SUB => |lhs, rhs| format!("{} - {}", lhs, rhs),
        BinaryType::MULTIPLY => |lhs, rhs| format!("{} * {}", lhs, rhs),
        BinaryType::MAX => |lhs, rhs| format!("max({}, {})", lhs, rhs),
        BinaryType::MOD => |lhs, rhs| format!("{} % {}", lhs, rhs),
        BinaryType::EQUAL => |lhs, rhs| format!("{} == {}", lhs, rhs),
        BinaryType::LESSTHAN => |lhs, rhs| format!("{} < {}", lhs, rhs),
    }
}

pub fn build_shader(op: BinaryType) -> String {
    format!(
        "
{header}

{workgroup_stride}

{lhs_interface}

{rhs_interface}

{output_interface}

@compute {workgroup_size}
fn {entry_point}(
    @builtin(global_invocation_id) global_id: vec3u
) {{
    {index}

    // Guard against out-of-bounds work group sizes
    if index >= input_metadata.length {{
        return;
    }}

    {lhs_mapped_index}

    {rhs_mapped_index}

    output[index] = {output};
}}
",
        header = shader_header(),
        workgroup_stride = WORKGROUP_SIZE.serialize_strides("WORKGROUP_STRIDE"),
        lhs_interface = tensor_interface("0", "read", "lhs", "array<vec4<f32>>", "lhs_metadata"),
        rhs_interface = tensor_interface("1", "read", "rhs", "array<vec4<f32>>", "rhs_metadata"),
        output_interface = tensor_interface(
            "2",
            "read_write",
            "output",
            "array<vec4<f32>>",
            "output_metadata"
        ),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        lhs_mapped_index = compute_strided_offset(
            "lhs_mapped_index",
            "index",
            "output_metadata",
            "lhs_metadata"
        ),
        rhs_mapped_index = compute_strided_offset(
            "rhs_mapped_index",
            "index",
            "output_metadata",
            "rhs_metadata"
        ),
        output = build_webgpu_operation(op)("lhs[lhs_mapped_index]", "rhs[rhs_mapped_index]"),
    )
}
