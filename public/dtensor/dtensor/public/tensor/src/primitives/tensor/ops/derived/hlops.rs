use crate::primitives::tensor::Tensor;

impl Tensor {
    pub fn Clip(&self, min: &Tensor, max: &Tensor) -> Tensor {
        self.Minimum(min).Maximum(max)
    }
}
