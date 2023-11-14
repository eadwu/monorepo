use tensor::primitives::tensor::BinaryType;

use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

fn build_webgpu_operation<'a>(op: BinaryType) -> impl Fn(&'a str, &'a str, &'a str) -> String {
    match op {
        BinaryType::ADD => |lhs, rhs, _| format!("{} + {}", lhs, rhs),
        BinaryType::DIVIDE => |lhs, rhs, _| format!("{} / {}", lhs, rhs),
        BinaryType::SUB => |lhs, rhs, _| format!("{} - {}", lhs, rhs),
        BinaryType::MULTIPLY => |lhs, rhs, _| format!("{} * {}", lhs, rhs),
        BinaryType::MAX => |lhs, rhs, _| format!("max({}, {})", lhs, rhs),
        BinaryType::MOD => |lhs, rhs, _| format!("{} % {}", lhs, rhs),
        BinaryType::EQUAL => |lhs, rhs, output_type| format!("{}({} == {})", output_type, lhs, rhs),
        BinaryType::LESSTHAN => {
            |lhs, rhs, output_type| format!("{}({} < {})", output_type, lhs, rhs)
        }
    }
}

pub fn build_shader(
    op: BinaryType,
    lhs_type: TensorType,
    rhs_type: TensorType,
    output_type: TensorType,
) -> String {
    let lhs_type = format!(
        "array<{datatype}>",
        datatype = wgsl_from_tensortype(lhs_type)
    );
    let rhs_type = format!(
        "array<{datatype}>",
        datatype = wgsl_from_tensortype(rhs_type)
    );
    let output_element_type = wgsl_from_tensortype(output_type);
    let output_type = format!("array<{datatype}>", datatype = output_element_type);

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
    if index >= output_metadata.length {{
        return;
    }}

    var lhs_mapped_index = index;
    {map_lhs_index}

    var rhs_mapped_index = index;
    {map_rhs_index}

    output[index] = {output};
}}
",
        header = shader_header(),
        workgroup_stride = WORKGROUP_SIZE.serialize_strides("WORKGROUP_STRIDE"),
        lhs_interface = tensor_interface("0", "read", "lhs", &lhs_type, "lhs_metadata"),
        rhs_interface = tensor_interface("1", "read", "rhs", &rhs_type, "rhs_metadata"),
        output_interface =
            tensor_interface("2", "read_write", "output", &output_type, "output_metadata"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        map_lhs_index = map_index("lhs_mapped_index", "lhs_metadata"),
        map_rhs_index = map_index("rhs_mapped_index", "rhs_metadata"),
        output = build_webgpu_operation(op)(
            "lhs[lhs_mapped_index]",
            "rhs[rhs_mapped_index]",
            &output_element_type
        ),
    )
}
