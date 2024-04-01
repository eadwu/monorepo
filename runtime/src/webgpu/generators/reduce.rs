use tensor::primitives::tensor::{ReduceType, Tensor};
use tensor::primitives::tensorview::{TensorView, ViewType};

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
    axes: &[ViewType],
    input: &Tensor,
    output: &Tensor,
    workgroups: &WebGPUWorkGroup,
) -> String {
    let input_wgpu = Into::<WebGPUTensor>::into(input);
    let output_wgpu = Into::<WebGPUTensor>::into(output);

    let index_normalization = |index_variable| {
        let normalized_mapper = Into::<TensorViewTracker>::into(TensorView::as_defined(
            false,
            output.view().shape.clone(),
            input.view().stride.clone(),
        ));
        map_index(index_variable, &normalized_mapper)
    };

    let reduce_shape = axes
        .iter()
        .map(|&axis| input.view().shape[axis as usize])
        .collect::<Vec<_>>();
    let reduce_strides = axes
        .iter()
        .map(|&axis| input.view().stride[axis as usize])
        .collect::<Vec<_>>();
    let iteration_mapper = Into::<TensorViewTracker>::into(TensorView::as_defined(
        false,
        reduce_shape.into_boxed_slice(),
        reduce_strides.into_boxed_slice(),
    ));
    let reduce_iterations = iteration_mapper.len();

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
    {normalize_index}

    var mapped_axis_index = mapped_index;
    {map_axis_index}

    var reduction = {input_tensor_name}[mapped_index];
    for (var i = 1u; i < {reduce_iterations}u; i++) {{
        var offset_at_index = i;
        {map_index_to_offset}
        mapped_axis_index = mapped_index + offset_at_index;
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
        normalize_index = index_normalization("mapped_index"),
        map_axis_index = map_index("mapped_axis_index", input.viewtracker()),
        reduce_iterations = reduce_iterations,
        map_index_to_offset = map_index("offset_at_index", &iteration_mapper),
        operation = {
            let input_data = format!("{}[mapped_axis_index]", input_wgpu.name());
            let output = build_webgpu_operation(op)("reduction", &input_data);
            output
        },
    )
}
