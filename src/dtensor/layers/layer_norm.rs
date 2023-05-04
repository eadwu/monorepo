use crate::dtensor::{self, primitives::ops::*, primitives::Tensor};

pub struct LayerNorm {
    n: usize,

    mean: Tensor,
    power_sum_average: Tensor,
    variance: Tensor,
    epsilon: Tensor,

    elementwise_affine: bool,
    gamma: Tensor,
    beta: Tensor,
}

impl LayerNorm {
    pub async fn new(
        normalized_shape: &[usize],
        eps: f32,
        elementwise_affine: bool,
        wgpu_device: &dtensor::WgpuDevice,
    ) -> LayerNorm {
        LayerNorm {
            n: 0,
            mean: Tensor::of_shape(normalized_shape, wgpu_device).await,
            power_sum_average: Tensor::of_shape(normalized_shape, wgpu_device).await,
            variance: Tensor::of_shape(normalized_shape, wgpu_device).await,
            epsilon: Tensor::literal(eps, wgpu_device).await,
            elementwise_affine: elementwise_affine,
            gamma: Tensor::of_shape(&normalized_shape, wgpu_device).await + 1.0,
            beta: Tensor::of_shape(&normalized_shape, wgpu_device).await,
        }
    }

    pub async fn forward(&mut self, x: &Tensor) -> Tensor {
        // https://invisibleblocks.wordpress.com/2008/07/30/long-running-averages-without-the-sum-of-preceding-values/
        let mean = &self.mean;

        self.n = self.n + 1;
        let n = self.n as f32;

        self.mean = mean + (x - mean) / n;
        let mean = &self.mean;

        // https://subluminal.wordpress.com/2008/07/31/running-standard-deviations/#more-15
        let power_sum_average = &self.power_sum_average;
        self.power_sum_average = power_sum_average + (x * x - power_sum_average) / n;
        let power_sum_average = &self.power_sum_average;

        self.variance = (power_sum_average * n - n * mean * mean) / (n - 1.0);
        let variance = &self.variance;

        let variance_eps = variance + &self.epsilon;
        let layer_norm = (x - mean) / sqrt(&variance_eps).await;

        if self.elementwise_affine {
            layer_norm * &self.gamma + &self.beta
        } else {
            layer_norm
        }
    }
}
