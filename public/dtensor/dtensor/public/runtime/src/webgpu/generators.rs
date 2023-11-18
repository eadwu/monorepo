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
    let view_history = viewtracker.serialized_history_fifo();
    let index_transformations = {
        view_history
            .iter()
            .enumerate()
            .map(|(i, view)| {
                let last_index = format!("index{}", i);
                let this_index = format!("index{}", i + 1);
                if view.ndim() == 0 {
                    format!("let {} = 0u;", this_index)
                } else {
                    let assignment = view
                        .shape
                        .iter()
                        .zip(view.stride.iter().zip(view.contiguous_stride.iter()))
                        .map(|(&shape, (&stride, &contiguous_stride))| {
                            format!(
                                "((({index} / {contiguous_stride}u) % {shape}u) * {stride}u)",
                                index = last_index,
                                contiguous_stride = contiguous_stride,
                                shape = shape,
                                stride = stride,
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("+");
                    format!("let {} = {};", this_index, assignment)
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    };

    format!(
        "
{{
    let index0 = {index_variable};
    {index_transformations}
    {index_variable} = index{view_len};
}}
",
        index_variable = index_variable,
        index_transformations = index_transformations,
        view_len = view_history.len(),
    )
}
