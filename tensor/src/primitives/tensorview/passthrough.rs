use crate::primitives::tensor::Tensor;
use crate::primitives::tensorview::ViewType;

impl Tensor {
    pub fn len(&self) -> ViewType {
        self.view().len()
    }

    pub fn ndim(&self) -> ViewType {
        self.view().ndim()
    }

    pub fn shape(&self) -> &[ViewType] {
        &self.view().shape[..]
    }
}
