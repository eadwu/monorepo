use tensor::primitives::tensor::{TensorType, UnaryType};

use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

fn build_webgpu_operation<'a>(
    op: UnaryType,
    output_datatype: TensorType,
) -> impl Fn(&'a str) -> String {
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
        UnaryType::CAST => match output_datatype {
            TensorType::F16 => |input| format!("f16({input})", input = input),
            TensorType::F32 => |input| format!("f32({input})", input = input),
            TensorType::U32 => |input| format!("u32({input})", input = input),
            TensorType::I32 => |input| format!("i32({input})", input = input),
        },
    }
}

pub fn build_shader(op: UnaryType, input_type: TensorType, output_datatype: TensorType) -> String {
    let input_type = format!(
        "array<{datatype}>",
        datatype = wgsl_from_tensortype(input_type)
    );
    let output_type = format!(
        "array<{datatype}>",
        datatype = wgsl_from_tensortype(output_datatype)
    );

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

    var mapped_index = index;
    {map_index}

    output[index] = {output};
}}
",
        header = shader_header(),
        workgroup_stride = WORKGROUP_SIZE.serialize_strides("WORKGROUP_STRIDE"),
        input_interface = tensor_interface("0", "read", "input", &input_type, "input_metadata"),
        output_interface =
            tensor_interface("1", "read_write", "output", &output_type, "output_metadata"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        map_index = map_index("mapped_index", "input_metadata"),
        output = build_webgpu_operation(op, output_datatype)("input[mapped_index]"),
    )
}
