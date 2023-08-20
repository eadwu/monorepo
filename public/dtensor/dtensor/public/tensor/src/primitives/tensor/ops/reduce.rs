use std::collections::HashSet;

use crate::primitives::tensor::{Tensor, TensorView, ViewType};

use super::{OperationSpec, TensorInput};

#[derive(Clone, Debug)]
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
    pub fn reduce(op: ReduceType, input: Tensor, axes: &[ViewType]) -> TensorInput {
        TensorInput::OperationResult(OperationSpec::ReduceOp(ReduceSpec {
            op,
            input,
            axes: axes.to_vec(),
        }))
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
            axes.to_vec()
        };

        // Make sure axes are within 0 <= axis < rank
        axes.iter().for_each(|&dimension| {
            assert!(
                (dimension as usize) < self.view().shape.len(),
                "Unexpected axis {} >= {}",
                dimension,
                self.view().shape.len()
            )
        });

        let axes_lookup = axes.iter().collect::<HashSet<_>>();
        let output_shape = self
            .view()
            .shape
            .iter()
            .enumerate()
            .filter_map(|(idx, &rank)| {
                let dimension = idx as ViewType;
                if axes_lookup.contains(&dimension) {
                    if keep_dims {
                        Some(1)
                    } else {
                        None
                    }
                } else {
                    Some(rank)
                }
            })
            .collect::<Vec<_>>();

        Tensor::new(
            TensorView::from_shape(&output_shape),
            TensorInput::reduce(op, self.clone(), &axes),
        )
    }

    pub fn Sum(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.reduce_op(ReduceType::SUM, axes, keep_dims)
    }

    pub fn Max(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.reduce_op(ReduceType::MAX, axes, keep_dims)
    }
}
