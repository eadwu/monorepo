use std::collections::HashMap;

use ::tensor::primitives::tensor::Tensor;

mod tensor;
pub use tensor::*;

type Gradients = HashMap<u32, Option<Tensor>>;
pub struct TensorGradient {
    _wrt: Tensor,
    _gradient: Tensor,
}

impl TensorGradient {
    pub fn new(tensor: Tensor, gradient: Tensor) -> TensorGradient {
        TensorGradient {
            _wrt: tensor,
            _gradient: gradient,
        }
    }

    pub fn gradient_of(&self) -> &Tensor {
        &self._wrt
    }

    pub fn gradient(&self) -> &Tensor {
        &self._gradient
    }
}

pub trait BackPropable {
    fn backward(&self, output: &Tensor) -> Vec<TensorGradient>;
}

pub trait BackPropagation {
    fn backward(&self) -> Vec<TensorGradient>;
    fn backprop(&self, tensors: &[&Tensor]) -> Gradients;
}
