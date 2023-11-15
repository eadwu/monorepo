use crate::primitives::tensor::Tensor;
use crate::primitives::tensorview::{TensorView, ViewType};

pub trait ArangeLoader {
    fn load(self, shape: &[ViewType]) -> Vec<u8>;
}

impl ArangeLoader for &Tensor {
    fn load(self, shape: &[ViewType]) -> Vec<u8> {
        let view = TensorView::from_contiguous_shape(shape);
        let n = view.len();
        let data = (0..n)
            .map(TryInto::<i32>::try_into)
            .map(Result::unwrap)
            .collect::<Vec<_>>();
        bytemuck::cast_slice(&data[..]).to_vec()
    }
}
