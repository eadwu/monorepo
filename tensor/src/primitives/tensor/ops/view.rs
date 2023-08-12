use crate::primitives::tensor::{Tensor, TensorView};

pub struct ViewSpec {
    pub view: TensorView,
    pub input: Tensor,
}
