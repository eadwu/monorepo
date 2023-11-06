use crate::primitives::tensor::{Tensor, TensorView, ViewType};

use super::{OperationSpec, TensorInput};

#[derive(Clone, Copy, Debug)]
pub enum ReduceType {
    SUM,
    MAX,
}

#[derive(Clone, Debug)]
pub struct ReduceSpec {
    pub op: ReduceType,
    pub input: Tensor,
    pub axis: ViewType,
}

impl TensorInput {
    pub fn reduce(op: ReduceType, input: Tensor, axis: ViewType) -> TensorInput {
        TensorInput::OperationResult(OperationSpec::ReduceOp(ReduceSpec { op, input, axis }))
    }
}

impl Tensor {
    fn _reduce(&self, op: ReduceType, axis: ViewType, keep_dim: bool) -> Tensor {
        let output_shape = self
            .view()
            .shape
            .iter()
            .enumerate()
            .map(
                |(idx, &dimension)| {
                    if idx == axis as usize {
                        1
                    } else {
                        dimension
                    }
                },
            )
            .collect::<Vec<_>>();

        let result = Tensor::new(
            TensorView::from_shape(&output_shape),
            TensorInput::reduce(op, self.clone(), axis),
        );

        if keep_dim {
            result
        } else {
            result.squeeze(axis)
        }
    }

    fn reduce_op(&self, op: ReduceType, axes: &[ViewType], keep_dims: bool) -> Tensor {
        // If &[] is given, assume it is a reduction along all axes
        let axes = if axes.len() == 0 {
            self.view()
                .shape
                .iter()
                .enumerate()
                .map(|(idx, _)| idx as ViewType)
                .collect::<Vec<_>>()
        } else {
            let mut axes = axes.to_vec();
            axes.sort();
            axes
        };

        assert!(
            axes.windows(2).all(|x| x[0] <= x[1]),
            "Axes must be sorted for ReduceOp due to underlying assumptions"
        );

        // Make sure axes are within 0 <= axis < rank
        axes.iter().for_each(|&dimension| {
            assert!(
                dimension < self.view().dimension(),
                "Unexpected axis {} >= {}",
                dimension,
                self.view().dimension()
            )
        });

        // Start from the back so that indices are accurate if keep_dims is false
        axes.iter().rev().fold(self.clone(), |accumulator, &axis| {
            accumulator._reduce(op, axis, keep_dims)
        })
    }

    pub fn Sum(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.reduce_op(ReduceType::SUM, axes, keep_dims)
    }

    pub fn Max(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.reduce_op(ReduceType::MAX, axes, keep_dims)
    }
}
