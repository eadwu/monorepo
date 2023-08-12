use crate::primitives::tensor::{Tensor, TensorView};

use super::{OperationSpec, TensorInput};

#[derive(Clone, Debug)]
pub struct ViewSpec {
    pub view: TensorView,
    pub input: Tensor,
}

impl TensorInput {
    pub fn view(view: TensorView, input: Tensor) -> TensorInput {
        TensorInput::OperationResult(OperationSpec::ViewOp(ViewSpec { view, input }))
    }
}

impl Tensor {
    fn view_op(&self, view: TensorView) -> Tensor {
        Tensor::new(view.clone(), TensorInput::view(view, self.clone()))
    }

    pub fn contiguous(&self) -> Tensor {
        let view = TensorView::from_shape(&self.view().shape);
        self.view_op(view)
    }

    pub fn broadcast(&self, other: &Tensor) -> Tensor {
        let view = self.view().broadcast(other.view());
        self.view_op(view).contiguous()
    }
}
