use tensor::primitives::tensor::{Tensor, TensorType, UnaryType};

use crate::webgpu::generators::*;
use crate::webgpu::WebGPUTensor;
use crate::webgpu::WebGPUWorkGroup;
use crate::webgpu::WORKGROUP_SIZE;

fn build_webgpu_operation<'a>(
    op: UnaryType,
    output_datatype: TensorType,
) -> impl Fn(&'a str) -> String {
    match op {
        UnaryType::EXP2 => |input| format!("exp2({input})", input = input),
        UnaryType::IDENTITY => |input| format!("{input}", input = input),
        UnaryType::LOG2 => |input| format!("log2({input})", input = input),
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

pub fn build_shader(
    op: UnaryType,
    input: &Tensor,
    output: &Tensor,
    workgroups: &WebGPUWorkGroup,
) -> String {
    let input_wgpu = Into::<WebGPUTensor>::into(input);
    let output_wgpu = Into::<WebGPUTensor>::into(output);

    format!(
        "
{input_interface}

{output_interface}

{workgroup_stride}
@compute {workgroup_size}
fn {entry_point}(
    @builtin(global_invocation_id) global_id: vec3u
) {{
    {index}

    // Guard against out-of-bounds work group sizes
    if index >= {output_length}u {{
        return;
    }}

    var mapped_index = index;
    {map_index}

    {output_tensor_name}[index] = {output};
}}
",
        workgroup_stride = workgroups.serialize_strides("WORKGROUP_STRIDE"),
        input_interface =
            input_wgpu.serialize_type(&wgsl_from_tensortype(input.datatype()), "0", "read"),
        output_interface =
            output_wgpu.serialize_type(&wgsl_from_tensortype(output.datatype()), "1", "read_write"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        output_length = output.len(),
        output_tensor_name = output_wgpu.name(),
        map_index = map_index("mapped_index", input.viewtracker()),
        output = {
            let input_data = format!("{}[mapped_index]", input_wgpu.name());
            let output = build_webgpu_operation(op, output.datatype())(&input_data);
            output
        }
    )
}
