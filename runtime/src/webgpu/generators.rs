use tensor::primitives::tensor::TensorType;
use tensor::primitives::tensorview::TensorViewTracker;

pub mod binary;
pub mod reduce;
pub mod unary;

pub fn wgsl_from_tensortype(datatype: TensorType) -> String {
    match datatype {
        TensorType::F16 => "f16",
        TensorType::F32 => "f32",
        TensorType::I32 => "i32",
        TensorType::U32 => "u32",
    }
    .to_string()
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

pub fn map_index(index_variable: &str, viewtracker: &TensorViewTracker) -> String {
    let index_transformation = {
        viewtracker
            .serialized_history_fifo()
            .iter()
            .fold(index_variable.to_string(), |previous_index, view| {
                let mapped_index = if view.ndim() == 0 {
                    format!("0u")
                } else {
                    view.shape
                        .iter()
                        .zip(view.stride.iter().zip(view.contiguous_stride.iter()))
                        .map(|(&shape, (&stride, &contiguous_stride))| {
                            format!(
                                "((({previous_index} / {contiguous_stride}u) % {shape}u) * {stride}u)",
                                previous_index = previous_index,
                                contiguous_stride = contiguous_stride,
                                shape = shape,
                                stride = stride,
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("+")
                };

                format!("({})", mapped_index)
            })
    };

    format!(
        "
{{
    {index_variable} = {singular_map};
}}
",
        index_variable = index_variable,
        singular_map = index_transformation,
    )
}
