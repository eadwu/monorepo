use crate::dtensor::{self, primitives::ops::*, primitives::Tensor};

pub struct Linear {
    weights: Tensor,
    bias: Tensor,
}

impl Linear {
    pub async fn new(
        in_features: usize,
        out_features: usize,
        bias: bool,
        wgpu_device: &dtensor::WgpuDevice,
    ) -> Linear {
        // let weights = Tensor::of_shape(&[out_features, in_features], wgpu_device);
        let w = Tensor::of_shape(&[in_features, out_features], wgpu_device).await;
        let b = Tensor::of_shape(&[out_features], wgpu_device).await;

        Linear {
            weights: w,
            bias: if bias { b } else { b * 0.0 },
        }
    }

    pub async fn forward(&self, x: &Tensor) -> Tensor {
        mm(x, &self.weights).await + &self.bias
    }
}
