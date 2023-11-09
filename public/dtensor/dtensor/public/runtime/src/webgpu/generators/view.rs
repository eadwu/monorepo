use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

pub fn build_shader(datatype: TensorType) -> String {
    let datatype = format!(
        "array<{datatype}>",
        datatype = wgsl_from_tensortype(datatype)
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

    var mapped_index_temp = index;
    var mapped_index = 0u;
    {map_index}

    output[index] = input[mapped_index];
}}
",
        header = shader_header(),
        workgroup_stride = WORKGROUP_SIZE.serialize_strides("WORKGROUP_STRIDE"),
        input_interface = tensor_interface("0", "read", "input", &datatype, "input_metadata"),
        output_interface =
            tensor_interface("1", "read_write", "output", &datatype, "output_metadata"),
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
    )
}
