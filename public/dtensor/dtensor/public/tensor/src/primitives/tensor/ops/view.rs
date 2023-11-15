use crate::primitives::tensor::{Tensor, TensorInput};
use crate::primitives::tensorview::{TensorView, ViewType};

impl Tensor {
    fn view_op(&self, view: &TensorView) -> Tensor {
        Tensor::new(
            self.viewtracker().track_view(view),
            TensorInput::no_op(self.clone()),
            self.datatype(),
        )
    }

    fn transparent_view_op(&self, view: &TensorView) -> Tensor {
        Tensor::new(
            self.viewtracker().swap_view(view),
            TensorInput::no_op(self.clone()),
            self.datatype(),
        )
    }

    pub fn contiguous(&self) -> Tensor {
        let view = TensorView::from_contiguous_shape(&self.shape());
        self.reshape(&view)
    }

    pub fn broadcast(&self, other: &Tensor) -> Tensor {
        self.broadcast_to(other.view())
    }

    pub fn broadcast_to(&self, view: &TensorView) -> Tensor {
        self.reshape(&self.view().broadcast(view))
    }

    pub fn squeeze(&self, axis: ViewType) -> Tensor {
        self.transparent_view_op(&self.view().squeeze(axis))
    }

    pub fn transpose(&self, axes: &[ViewType]) -> Tensor {
        self.transparent_view_op(&self.view().transpose(axes))
    }

    pub fn unsqueeze(&self, axis: ViewType) -> Tensor {
        self.transparent_view_op(&self.view().unsqueeze(axis))
    }

    pub fn reshape(&self, view: &TensorView) -> Tensor {
        self.view_op(view)
    }
}
