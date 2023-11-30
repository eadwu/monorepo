use crate::primitives::tensor::Tensor;
use crate::primitives::tensorview::{TensorView, ViewType};

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
    pub axes: Vec<ViewType>,
}

impl TensorInput {
    pub fn reduce(op: ReduceType, input: Tensor, axes: Vec<ViewType>) -> TensorInput {
        TensorInput::OperationResult(OperationSpec::ReduceOp(ReduceSpec { op, input, axes }))
    }
}

impl Tensor {
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
                dimension < self.ndim(),
                "Unexpected axis {} >= {}",
                dimension,
                self.ndim()
            )
        });

        let mut output_shape = self.shape().to_vec();
        for &axis in &axes {
            output_shape[axis as usize] = 1;
        }
        let output_view = TensorView::from_contiguous_shape(&output_shape[..]);

        let result = Tensor::new(
            output_view,
            TensorInput::reduce(op, self.clone(), axes.clone()),
            self.datatype(),
        );

        if keep_dims {
            result
        } else {
            // Start from the back so that indices are accurate if keep_dims is false
            axes.iter()
                .rev()
                .fold(result, |accumulator, &axis| accumulator.squeeze(axis))
        }
    }

    pub fn Sum(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.reduce_op(ReduceType::SUM, axes, keep_dims)
    }

    pub fn Max(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.reduce_op(ReduceType::MAX, axes, keep_dims)
    }
}
