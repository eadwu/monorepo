use crate::dtensor::{
    self, primitives::ops::builders, primitives::ops::impl_tensor_op, primitives::Tensor,
};
use itertools::{
    EitherOrBoth::{Both, Left, Right},
    Itertools,
};
use wgpu;

const ENTRY_POINT: &str = "gemm";
const WORKGROUP_SIZE: usize = 64;

impl_tensor_op!(MM mm |a: &Tensor, b: &Tensor| -> Tensor {
    if !std::rc::Rc::ptr_eq(a.wgpu_device(), b.wgpu_device()) {
        panic!("Can't perform operations on Tensors on different devices");
    }

    // Perform some reshaping for broadcasting rules
    let a_shape = a.shape();
    let b_shape = b.shape();

    // https://pytorch.org/docs/stable/generated/torch.matmul.html
    let (a_matrix, b_matrix) = if a_shape.len() == 1 && b_shape.len() == 1 {
        let a_matrix = a.reshape(&[1, a_shape[0]]).await;
        let b_matrix = b.reshape(&[b_shape[0], 1]).await;
        (a_matrix, b_matrix)
    } else if a_shape.len() == 1 {
        let a_matrix = a.reshape(&[1, a_shape[0]]).await;
        (a_matrix, b * 1.0)
    } else if b_shape.len() == 1 {
        let b_matrix = b.reshape(&[b_shape[0], 1]).await;
        (a * 1.0, b_matrix)
    } else {
        (a * 1.0, b * 1.0)
    };

    let a_shape = a_matrix.shape();
    let b_shape = b_matrix.shape();

    let a_batch_shape = &a_shape[..a_shape.len() - 2];
    let a_mat_shape = &a_shape[a_shape.len() - 2..];

    let b_batch_shape = &b_shape[..b_shape.len() - 2];
    let b_mat_shape = &b_shape[b_shape.len() - 2..];

    let a_m = a_mat_shape[0];
    let a_k = a_mat_shape[1];
    let b_k = b_mat_shape[0];
    let b_n = b_mat_shape[1];

    if a_k != b_k {
        panic!(
            "Unable to perform matrix multiplication between {}x{} and {}x{} matrices",
            a_m, a_k, b_k, b_n
        )
    }

    let broadcasted_batch_shape: Vec<usize> = a_batch_shape
        .iter()
        .rev()
        .zip_longest(b_batch_shape.iter().rev())
        .map(|element| match element {
            Both(&l, &r) => {
                if l == r || l == 1 || r == 1 {
                    std::cmp::max(l, r)
                } else {
                    panic!(
                        "Unable to broadcast batch shapes {:?} and {:?}",
                        a_batch_shape, b_batch_shape
                    );
                }
            }
            Left(&l) => l,
            Right(&r) => r,
        })
        .rev()
        .collect();
    let broadcasted_output_shape: Vec<usize> = broadcasted_batch_shape
        .iter()
        .chain(std::iter::once(&a_m))
        .chain(std::iter::once(&b_n))
        .map(|&x| x)
        .collect();

    let web_gpu = a.device();
    let dtensor::WebGPU { device, queue: _ } = web_gpu;

    let a_contiguous = a.as_contiguous().await;
    let b_contiguous = b.as_contiguous().await;
    let result = Tensor::of_shape(&broadcasted_output_shape, a.wgpu_device()).await;

    let pipeline_descriptor = builders::TensorOpDescriptor {
        inputs: &[
            builders::TensorDescriptor {
                name: "left",
                tensor: &a_contiguous,
            },
            builders::TensorDescriptor {
                name: "right",
                tensor: &b_contiguous,
            },
        ],
        output: builders::TensorDescriptor {
            name: "result",
            tensor: &result,
        },
        entry_point: ENTRY_POINT,
    };

    let shader_source =
        generate_wgsl_shader(a_m as u32, b_k as u32, b_n as u32, &pipeline_descriptor);
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    builders::build_op_pipeline(&pipeline_descriptor, &compiled_shader, web_gpu);
    result
});

fn generate_wgsl_shader(
    m: u32,
    k: u32,
    n: u32,
    pipeline_descriptor: &builders::TensorOpDescriptor,
) -> String {
    format!(
        "
{shader_interface}

const M: u32 = {M}u;
const K: u32 = {K}u;
const N: u32 = {N}u;

@compute @workgroup_size({workgroup_size}, 1, 1)
fn {entry_point}(@builtin(global_invocation_id) global_id: vec3u) {{
    // Guard against out-of-bounds work group sizes
    if (global_id.x >= result_metadata.length) {{
      return;
    }}

    {workarounds}

    // Unmapped index, M[..][i][j]
    let index: u32 = global_id.x;

    // M[..], every ..xBxC can be deconstructed into a series of BxC matrices
    let index_mn = index / (M * N);

    // A[..][K][0], map M[..][i][j] to get the A[..][i][0] base offset
    let left_base_offset = (index_mn * M * K) + ((index / N) * K);
    // B[..][0][K], map M[..][i][j] to get the B[..][0][j] base offset
    let right_base_offset = (index_mn * K * N) + (index % N);

    var sum_of_products: f32 = 0.0;
    for (var i = 0u; i < K; i++) {{
        // A[..][K][i], each offset increases by 1 when contiguous
        let left_row_element = left[left_base_offset + i];
        // B[..][i][K], each offset increases by a row when contiguous
        let right_column_element = right[right_base_offset + i*N];
        // A[..][K][i] * B[..][i][K]
        sum_of_products = sum_of_products + left_row_element * right_column_element;
    }}

    result[index] = sum_of_products;
}}
",
        shader_interface = builders::define_shader_interface(
            pipeline_descriptor.inputs,
            &pipeline_descriptor.output
        ),
        M = m,
        K = k,
        N = n,
        workgroup_size = WORKGROUP_SIZE,
        entry_point = pipeline_descriptor.entry_point,
        workarounds = builders::shader_workaround_1976(pipeline_descriptor),
    )
}
