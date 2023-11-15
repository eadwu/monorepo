use tensor::primitives::tensor::TensorType;

use super::TensorMetadata;

pub mod binary;
pub mod reduce;
pub mod unary;

pub fn shader_header() -> String {
    format!(
        "
{TensorMetadata}
",
        TensorMetadata = TensorMetadata::serialize_definition(),
    )
}

pub fn wgsl_from_tensortype(datatype: TensorType) -> String {
    match datatype {
        TensorType::F16 => "f16",
        TensorType::F32 => "f32",
        TensorType::I32 => "i32",
        TensorType::U32 => "u32",
    }
    .to_string()
}

pub fn tensor_interface(
    group: &str,
    permission: &str,
    name: &str,
    container_type: &str,
    metadata_name: &str,
) -> String {
    format!(
        "
@group({group}) @binding(0) var<storage, {permission}> {name}: {container_type};
@group({group}) @binding(1) var<storage, read> {metadata_name}: TensorMetadata;
",
        group = group,
        permission = permission,
        name = name,
        container_type = container_type,
        metadata_name = metadata_name,
    )
}

pub fn compute_index(
    output_variable: &str,
    global_index_variable: &str,
    workgroup_stride_variable: &str,
) -> String {
    format!(
        "let {output_variable} = dot({global_index_variable}, {workgroup_stride_variable});",
        output_variable = output_variable,
        global_index_variable = global_index_variable,
        workgroup_stride_variable = workgroup_stride_variable,
    )
}

pub fn translate_index_as_strided(
    index_variable: &str,
    metadata_variable: &str,
    strided_field: &str,
    view_offset_variable: &str,
) -> String {
    format!(
        "
{{
    var translate_index_as_strided__index = {index_variable};
    var translate_index_as_strided__mapped = 0u;

    for (var i = 0u; i < {metadata_variable}.ndim; i++) {{
        let shape_offset = {view_offset_variable} + {metadata_variable}.shape_offset + i;
        let stride_offset = {view_offset_variable} + {metadata_variable}.{strided_field} + i;
        let contiguous_stride_offset = {view_offset_variable} + {metadata_variable}.contiguous_stride_offset + i;

        let shape = {metadata_variable}.metadata[shape_offset];
        let stride = {metadata_variable}.metadata[stride_offset];
        let contiguous_stride = {metadata_variable}.metadata[contiguous_stride_offset];

        let data_index = translate_index_as_strided__index / contiguous_stride;
        let mapped_index = data_index % shape;

        translate_index_as_strided__index %= contiguous_stride;
        translate_index_as_strided__mapped += mapped_index * stride;
    }}

    {index_variable} = translate_index_as_strided__mapped;
}}
",
        index_variable = index_variable,
        metadata_variable = metadata_variable,
        strided_field = strided_field,
        view_offset_variable = view_offset_variable,
    )
}

pub fn map_index(index_variable: &str, metadata_variable: &str) -> String {
    format!(
        "
{{
    var map_index__index = {index_variable};

    for (var view = 0u; view < {metadata_variable}.nviews; view++) {{
        let base_view_offset = view * {metadata_variable}.view_size;
        {map_as_strided}
    }}

    {index_variable} = map_index__index;
}}
",
        index_variable = index_variable,
        map_as_strided = translate_index_as_strided(
            "map_index__index",
            metadata_variable,
            "stride_offset",
            "base_view_offset"
        ),
    )
}
