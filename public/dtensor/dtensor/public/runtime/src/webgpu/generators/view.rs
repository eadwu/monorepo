use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

pub fn build_shader() -> String {
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

    {mapped_index}

    output[index] = input[mapped_index];
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
        mapped_index =
            compute_strided_offset("mapped_index", "index", "output_metadata", "input_metadata"),
    )
}
