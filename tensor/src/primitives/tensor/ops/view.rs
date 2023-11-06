use crate::primitives::tensor::{Tensor, TensorView, ViewType};

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
    fn view_op(&self, view: &TensorView) -> Tensor {
        if self.view() == view {
            self.clone()
        } else {
            Tensor::new(view.clone(), TensorInput::no_op(self.clone()))
        }
    }

    fn eager_view_op(&self, view: &TensorView) -> Tensor {
        if self.view() == view {
            self.clone()
        } else {
            Tensor::new(view.clone(), TensorInput::view(view.clone(), self.clone()))
        }
    }

    pub fn contiguous(&self) -> Tensor {
        let view = TensorView::from_shape(&self.shape());
        self.eager_view_op(&view)
    }

    pub fn broadcast(&self, other: &Tensor) -> Tensor {
        self.broadcast_to(other.view())
    }

    pub fn broadcast_to(&self, view: &TensorView) -> Tensor {
        self.reshape(&self.view().broadcast(view))
    }

    pub fn pad(&self, padding: &[(ViewType, ViewType)]) -> Tensor {
        let dimension = self.ndim() as usize;
        assert!(
            dimension == padding.len(),
            "Padding must be specified for every dimension"
        );

        let padded_view = self.view().pad(padding);
        let copy_view = padded_view.offset(padding);
        self.eager_view_op(&copy_view).reshape_unsafe(&padded_view)
    }

    pub fn squeeze(&self, axis: ViewType) -> Tensor {
        self.reshape(&self.view().squeeze(axis))
    }

    pub fn transpose(&self, axes: &[ViewType]) -> Tensor {
        self.reshape(&self.view().transpose(axes))
    }

    pub fn unsqueeze(&self, axis: ViewType) -> Tensor {
        self.reshape(&self.view().unsqueeze(axis))
    }

    pub fn reshape(&self, view: &TensorView) -> Tensor {
        assert!(
            view.len() % self.len() == 0,
            "Expected shapes to be multiples, got {} -> {} elements",
            self.len(),
            view.len()
        );

        self.view_op(view)
    }

    pub fn reshape_unsafe(&self, view: &TensorView) -> Tensor {
        self.view_op(view)
    }
}
