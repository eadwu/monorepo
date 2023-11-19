use tensor::primitives::tensor::{ReduceType, Tensor};
use tensor::primitives::tensorview::ViewType;

use crate::webgpu::WebGPUWorkGroup;
use crate::webgpu::WORKGROUP_SIZE;
use crate::webgpu::{generators::*, WebGPUTensor};

fn build_webgpu_operation<'a>(op: ReduceType) -> impl Fn(&'a str, &'a str) -> String {
    match op {
        ReduceType::MAX => |accumulator, current| format!("max({}, {})", accumulator, current),
        ReduceType::SUM => |accumulator, current| format!("{} + {}", accumulator, current),
    }
}

pub fn build_shader(
    op: ReduceType,
    axis: ViewType,
    input: &Tensor,
    output: &Tensor,
    workgroups: &WebGPUWorkGroup,
) -> String {
    let input_wgpu = Into::<WebGPUTensor>::into(input);
    let output_wgpu = Into::<WebGPUTensor>::into(output);

    let index_normalization = |index_variable| {
        let normalized_index = if axis == 0 {
            // Nothing to normalize
            "0u".to_string()
        } else {
            input.view().shape[..axis as usize]
                .iter()
                .zip(output.view().contiguous_stride.iter())
                .zip(input.view().stride.iter())
                .map(
                    |((&output_shape, &output_contiguous_stride), &input_stride)| {
                        // Strictly speaking, shape isn't needed
                        format!(
                            "((({index_variable} / {contiguous_stride}u) % {shape}u) * {stride}u)",
                            index_variable = index_variable,
                            contiguous_stride = output_contiguous_stride,
                            shape = output_shape,
                            stride = input_stride
                        )
                    },
                )
                .collect::<Vec<_>>()
                .join("+")
        };

        let leftover_index = format!(
            "({index_variable} % {contiguous_stride_at_axis}u)",
            index_variable = index_variable,
            contiguous_stride_at_axis = output.view().contiguous_stride[axis as usize]
        );

        format!("(({}) + ({}))", normalized_index, leftover_index)
    };

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

    let mapped_index = {normalize_index};

    var mapped_axis_index = mapped_index;
    {map_axis_index}

    var reduction = {input_tensor_name}[mapped_index];
    for (var i = 1u; i < {axis_rank}u; i++) {{
        mapped_axis_index = mapped_index + i * {axis_stride}u;
        {map_axis_index}

        reduction = {operation};
    }}

    {output_tensor_name}[index] = reduction;
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
        input_tensor_name = input_wgpu.name(),
        normalize_index = index_normalization("index"),
        map_axis_index = map_index("mapped_axis_index", input.viewtracker()),
        axis_rank = input.view().shape[axis as usize],
        axis_stride = input.view().stride[axis as usize],
        operation = {
            let input_data = format!("{}[mapped_axis_index]", input_wgpu.name());
            let output = build_webgpu_operation(op)("reduction", &input_data);
            output
        },
    )
}
