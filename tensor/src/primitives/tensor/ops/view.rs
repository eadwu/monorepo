use crate::primitives::tensor::{Tensor, TensorView};

#[derive(Clone, Debug)]
pub struct ViewSpec {
    pub view: TensorView,
    pub input: Tensor,
}
