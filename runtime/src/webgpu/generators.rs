use super::TensorMetadata;

pub mod binary;
pub mod index;
pub mod reduce;
pub mod unary;
pub mod view;

pub fn shader_header() -> String {
    format!(
        "
{TensorMetadata}
",
        TensorMetadata = TensorMetadata::serialize_definition(),
    )
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

pub fn compute_strided_offset(
    origin_index_var: &str,
    mapped_index_var: &str,
    start_axis_var: &str,
    end_axis_var: &str,
    origin_metadata_variable: &str,
    mapped_metadata_variable: &str,
) -> String {
    format!(
        "
for (var i = {start_axis_var}; i < {end_axis_var}; i++) {{
    let origin_stride = {origin_metadata_variable}.metadata[{origin_metadata_variable}.contiguous_stride_offset + i];
    let origin_shape = {origin_metadata_variable}.metadata[{origin_metadata_variable}.shape_offset + i];
    let origin_offset = {origin_metadata_variable}.metadata[{origin_metadata_variable}.offset_offset + i];
    let origin_end_offset = origin_offset + origin_shape;

    let mapped_stride = {mapped_metadata_variable}.metadata[{mapped_metadata_variable}.stride_offset + i];
    let mapped_shape = {mapped_metadata_variable}.metadata[{mapped_metadata_variable}.shape_offset + i];
    let mapped_offset = {mapped_metadata_variable}.metadata[{mapped_metadata_variable}.offset_offset + i];

    let index_at_dimension = {origin_index_var} / origin_stride;

    // Skip if not within bounds of View
    if index_at_dimension < origin_offset || index_at_dimension >= origin_end_offset {{
        return;
    }}

    let origin_index_at_dimension = index_at_dimension;
    let mapped_index_at_dimension = ((index_at_dimension - origin_offset) % mapped_shape) + mapped_offset;
    {origin_index_var} -= origin_index_at_dimension * origin_stride;
    {mapped_index_var} += mapped_index_at_dimension * mapped_stride;
}}
",
        origin_index_var = origin_index_var,
        mapped_index_var = mapped_index_var,
        start_axis_var = start_axis_var,
        end_axis_var = end_axis_var,
        origin_metadata_variable = origin_metadata_variable,
        mapped_metadata_variable = mapped_metadata_variable,
    )
}
