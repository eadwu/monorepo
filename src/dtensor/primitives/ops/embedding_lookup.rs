use crate::dtensor::{
    self, primitives::ops::builders, primitives::ops::impl_tensor_op, primitives::Tensor,
};
use wgpu;

const ENTRY_POINT: &str = "embedding_lookup";
const WORKGROUP_SIZE: usize = 64;

impl_tensor_op!(EmbeddingLookup embedding_lookup |embeddings: &Tensor, indices: &Tensor| -> Tensor {
    if !std::rc::Rc::ptr_eq(embeddings.wgpu_device(), indices.wgpu_device()) {
        panic!("Can't perform operations on Tensors on different devices");
    }

    if embeddings.shape().len() != 2 {
        panic!(
            "Expected Embedding to be of 2-dimensions, got {:?}",
            embeddings.shape()
        );
    }

    if indices.shape().len() != 1 {
        panic!(
            "Expected indices to be of 1-dimension, got {:?}",
            indices.shape()
        );
    }

    let (embedding_shape, indices_shape) = (embeddings.shape(), indices.shape());
    let n_embeddings = embedding_shape[0];
    let embedding_dim = embedding_shape[1];
    let batch_size = indices_shape[0];
    let output_shape = [batch_size, embedding_dim];

    let web_gpu = embeddings.device();
    let dtensor::WebGPU { device, queue: _ } = web_gpu;
    let result = Tensor::of_shape(&output_shape, embeddings.wgpu_device()).await;

    let pipeline_descriptor = builders::TensorOpDescriptor {
        inputs: &[
            builders::TensorDescriptor {
                name: "embeddings",
                tensor: embeddings,
            },
            builders::TensorDescriptor {
                name: "indices",
                tensor: indices,
            },
        ],
        output: builders::TensorDescriptor {
            name: "result",
            tensor: &result,
        },
        entry_point: ENTRY_POINT,
    };

    let shader_source = generate_wgsl_shader(
        &pipeline_descriptor,
        n_embeddings,
        embedding_dim,
        batch_size,
    );
    let compiled_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(shader_source.into()),
    });

    builders::build_op_pipeline(&pipeline_descriptor, &compiled_shader, web_gpu);
    result
});

fn generate_wgsl_shader(
    pipeline_descriptor: &builders::TensorOpDescriptor,
    n_embeddings: usize,
    embedding_dim: usize,
    batch_size: usize,
) -> String {
    format!(
        "
{shader_interface}

@compute @workgroup_size({workgroup_size}, 1, 1)
fn {entry_point}(@builtin(global_invocation_id) global_id: vec3u) {{
    // Guard against out-of-bounds work group sizes
    if (global_id.x >= result_metadata.length) {{
      return;
    }}

    {workarounds}

    // Unmapped index
    let index: u32 = global_id.x;

    let n_embeddings = embeddings_shape[0];
    let embeddings_dim = embeddings_shape[1];
    let batch_size = indices_shape[0];

    // Embeddings is of shape n_embeddings x embeddings_dim
    // Output is of shape batch_size x embeddings_dim
    let embedding_n = u32(indices[index / embeddings_dim]);
    let embedding_rank_n = index % embeddings_dim;

    result[index] = embeddings[embedding_n * embeddings_dim + embedding_rank_n];
}}
",
        shader_interface = builders::define_shader_interface(
            pipeline_descriptor.inputs,
            &pipeline_descriptor.output
        ),
        workgroup_size = WORKGROUP_SIZE,
        entry_point = pipeline_descriptor.entry_point,
        workarounds = builders::shader_workaround_1976(pipeline_descriptor),
    )
}
