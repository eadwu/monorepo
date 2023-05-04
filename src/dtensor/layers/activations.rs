// https://pytorch.org/docs/stable/generated/torch.nn.GELU.html
use crate::dtensor::{primitives::ops::*, primitives::Tensor};

const C1: f32 = 0.5;
const C2: f32 = 1.0;
const C3: f32 = (2.0 / std::f64::consts::PI) as f32;
const C4: f32 = 0.044715;

pub struct GeLU {}
impl GeLU {
    pub async fn new() -> GeLU {
      GeLU {  }
    }

    pub async fn forward(&self, x: &Tensor) -> Tensor {
        // GELU(x) = 0.5*x*(1+tanh(sqrt(2/pi) * (x+0.044715+x^3)))
        C1 * x * (C2 + tanh(&(C3 * (x + C4 + pow(x, 3.0).await))).await)
    }
}
