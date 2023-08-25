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

    pub fn contiguous(&self) -> Tensor {
        let view = TensorView::from_shape(&self.view().shape);

        if self.view() == &view {
            self.clone()
        } else {
            Tensor::new(view.clone(), TensorInput::view(view.clone(), self.clone()))
        }
    }

    pub fn broadcast(&self, other: &Tensor) -> Tensor {
        self.broadcast_to(other.view())
    }

    pub fn broadcast_to(&self, view: &TensorView) -> Tensor {
        self.reshape(&self.view().broadcast(view))
    }

    pub fn squeeze(&self, axis: ViewType) -> Tensor {
        let axis = axis as usize;
        let n_dimension = self.view().shape.len();
        let axis_rank = self.view().shape[axis];

        assert!(
            axis < n_dimension,
            "Axis {} must be within 0 <= axis < {}",
            axis,
            n_dimension
        );
        assert!(
            axis_rank == 1,
            "Cannot remove axis {} with rank {} != 1",
            axis,
            axis_rank
        );

        let (exclusive_left, inclusive_right) = self.view().shape.split_at(axis);
        let new_shape = exclusive_left
            .iter()
            .chain(inclusive_right.iter().skip(1))
            .map(|&n| n)
            .collect::<Vec<_>>();

        self.reshape(&TensorView::from_shape(&new_shape))
    }

    pub fn unsqueeze(&self, axis: ViewType) -> Tensor {
        let axis = axis as usize;
        let n_dimension = self.view().shape.len();
        assert!(
            axis <= n_dimension,
            "Axis {} must be within 0 <= axis <= {}",
            axis,
            n_dimension
        );

        let (exclusive_left, inclusive_right) = self.view().shape.split_at(axis);
        let new_shape = exclusive_left
            .iter()
            .chain(std::iter::once(&1))
            .chain(inclusive_right.iter())
            .map(|&n| n)
            .collect::<Vec<_>>();

        self.reshape(&TensorView::from_shape(&new_shape))
    }

    pub fn reshape(&self, view: &TensorView) -> Tensor {
        assert!(
            view.len() % self.view().len() == 0,
            "Expected shapes to be multiples, got {} -> {} elements",
            self.view().len(),
            view.len()
        );

        self.view_op(&view)
    }
}
