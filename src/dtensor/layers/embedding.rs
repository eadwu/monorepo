use crate::dtensor::{self, primitives::ops::*, primitives::Tensor};

pub struct Embedding {
    embeddings: Tensor,
}

impl Embedding {
    pub async fn new(
        num_embeddings: usize,
        embedding_dim: usize,
        wgpu_device: &dtensor::WgpuDevice,
    ) -> Embedding {
        Embedding {
            embeddings: Tensor::of_shape(&[num_embeddings, embedding_dim], wgpu_device).await,
        }
    }

    pub async fn forward(&self, x: &Tensor) -> Tensor {
        embedding_lookup(&self.embeddings, x).await
    }
}
