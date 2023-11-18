use tensor::primitives::tensor::{BinaryType, Tensor};

use crate::webgpu::generators::*;
use crate::webgpu::WebGPUTensor;
use crate::webgpu::WebGPUWorkGroup;
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
    lhs: &Tensor,
    rhs: &Tensor,
    output: &Tensor,
    workgroups: &WebGPUWorkGroup,
) -> String {
    let lhs_wgpu = Into::<WebGPUTensor>::into(lhs);
    let rhs_wgpu = Into::<WebGPUTensor>::into(rhs);
    let output_wgpu = Into::<WebGPUTensor>::into(output);
    let output_datatype = wgsl_from_tensortype(output.datatype());

    format!(
        "
{lhs_interface}

{rhs_interface}

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

    var lhs_mapped_index = index;
    {map_lhs_index}

    var rhs_mapped_index = index;
    {map_rhs_index}

    {output_tensor_name}.data[index] = {output};
}}
",
        workgroup_stride = workgroups.serialize_strides("WORKGROUP_STRIDE"),
        lhs_interface = lhs_wgpu.serialize_type(&wgsl_from_tensortype(lhs.datatype()), "0", "read"),
        rhs_interface = rhs_wgpu.serialize_type(&wgsl_from_tensortype(rhs.datatype()), "1", "read"),
        output_interface =
            output_wgpu.serialize_type(&wgsl_from_tensortype(output.datatype()), "2", "read_write"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        output_tensor_name = output_wgpu.name(),
        map_lhs_index = map_index("lhs_mapped_index", &lhs_wgpu.name()),
        map_rhs_index = map_index("rhs_mapped_index", &rhs_wgpu.name()),
        output = {
            let mapped_lhs_data = format!("{}.data[lhs_mapped_index]", lhs_wgpu.name());
            let mapped_rhs_data = format!("{}.data[rhs_mapped_index]", rhs_wgpu.name());
            let output =
                build_webgpu_operation(op)(&mapped_lhs_data, &mapped_rhs_data, &output_datatype);
            output
        },
    )
}
