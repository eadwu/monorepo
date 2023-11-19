use crate::primitives::tensor::Tensor;
use crate::primitives::tensorview::{TensorView, ViewType};

pub trait ArangeLoader {
    fn load(self, n: ViewType) -> Vec<u8>;
}

impl ArangeLoader for &Tensor {
    fn load(self, n: ViewType) -> Vec<u8> {
        let data = (0..n)
            .map(TryInto::<i32>::try_into)
            .map(Result::unwrap)
            .collect::<Vec<_>>();
        bytemuck::cast_slice(&data[..]).to_vec()
    }
}
