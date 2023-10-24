use tensor::primitives::tensor::UnaryType;

use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

fn build_webgpu_operation<'a>(op: UnaryType) -> impl Fn(&'a str) -> String {
    match op {
        UnaryType::EXP2 => |input| format!("exp2({input})", input = input),
        UnaryType::IDENTITY => |input| format!("{input}", input = input),
        UnaryType::LOG2 => |input| format!("log2({input})", input = input),
        UnaryType::RECIP => |input| format!("(1 / ({input}))", input = input),
        UnaryType::SIN => |input| format!("sin({input})", input = input),
        UnaryType::SQRT => |input| format!("sqrt({input})", input = input),
        UnaryType::ABS => |input| format!("abs({input})", input = input),
        UnaryType::FLOOR => |input| format!("floor({input})", input = input),
        UnaryType::CEIL => |input| format!("ceil({input})", input = input),
    }
}

pub fn build_shader(op: UnaryType) -> String {
    format!(
        "
{header}

{workgroup_stride}

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

    var mapped_index_temp = index;
    var mapped_index = 0u;
    {map_index}

    output[index] = {output};
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
        map_index = compute_strided_offset(
            "mapped_index_temp",
            "mapped_index",
            "0u",
            "output_metadata.dimension",
            "output_metadata",
            "input_metadata"
        ),
        output = build_webgpu_operation(op)("input[mapped_index]"),
    )
}
