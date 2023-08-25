use super::TensorMetadata;

pub mod binary;
pub mod unary;
pub mod view;
pub mod index;

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
    output_variable: &str,
    index_variable: &str,
    input_metadata_variable: &str,
    mapped_metadata_variable: &str,
) -> String {
    format!(
        "
var {output_variable}_temp: u32 = {index_variable};
var {output_variable}: u32 = 0u;

for (var i = 0u; i < {input_metadata_variable}.dimension; i++) {{
    let input_contiguous_stride = {input_metadata_variable}.metadata[{input_metadata_variable}.contiguous_stride_offset + i];

    let input_shape = {input_metadata_variable}.metadata[{input_metadata_variable}.shape_offset + i];
    let mapped_shape = {mapped_metadata_variable}.metadata[{mapped_metadata_variable}.shape_offset + i];
    let mapped_stride = {mapped_metadata_variable}.metadata[{mapped_metadata_variable}.stride_offset + i];

    let index_at_dimension = {output_variable}_temp / input_contiguous_stride;
    {output_variable} += (index_at_dimension % mapped_shape) * mapped_stride;
    {output_variable}_temp -= (index_at_dimension % input_shape) * input_contiguous_stride;
}}
",
        output_variable = output_variable,
        index_variable = index_variable,
        input_metadata_variable = input_metadata_variable,
        mapped_metadata_variable = mapped_metadata_variable,
    )
}
