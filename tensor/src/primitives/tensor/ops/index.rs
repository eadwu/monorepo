use std::collections::HashMap;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::primitives::tensor::{Tensor, TensorView, ViewType};

use super::{OperationSpec, TensorInput};

#[derive(Clone, Debug)]
pub enum IndexType {
    GatherElements,
    ScatterElements,
}

#[derive(Clone, Debug)]
pub struct IndexSpec {
    pub op: IndexType,
    pub dependencies: HashMap<u32, Tensor>,
    pub serialized_params: Vec<u8>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GatherParams {
    pub axis: ViewType,
    pub input: u32,
    pub indices: u32,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ScatterParams {
    pub axis: ViewType,
    pub reduction: ScatterReduction,
    pub input: u32,
    pub indices: u32,
    pub updates: u32,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ScatterReduction {
    None,
    Add,
    Mul,
    Max,
    Min,
}

impl TensorInput {
    pub fn index(
        op: IndexType,
        dependencies: HashMap<u32, Tensor>,
        serialized_params: Vec<u8>,
    ) -> TensorInput {
        TensorInput::OperationResult(OperationSpec::IndexOp(IndexSpec {
            op,
            dependencies,
            serialized_params,
        }))
    }
}

impl Tensor {
    fn gather_op(&self, axis: ViewType, indices: &Tensor) -> Tensor {
        let mut dependencies = HashMap::new();
        dependencies.insert(self.id(), self.clone());
        dependencies.insert(indices.id(), indices.clone());

        let serialized_params = bincode::serialize(&GatherParams {
            axis: axis,
            input: self.id(),
            indices: indices.id(),
        })
        .unwrap();

        Tensor::new(
            indices.view().clone(),
            TensorInput::index(IndexType::GatherElements, dependencies, serialized_params),
        )
    }

    fn scatter_op(
        &self,
        axis: ViewType,
        reduction: ScatterReduction,
        indices: &Tensor,
        updates: &Tensor,
    ) -> Tensor {
        let mut dependencies = HashMap::new();
        dependencies.insert(self.id(), self.clone());
        dependencies.insert(indices.id(), indices.clone());
        dependencies.insert(updates.id(), updates.clone());

        let serialized_params = bincode::serialize(&ScatterParams {
            axis: axis,
            reduction: reduction,
            input: self.id(),
            indices: indices.id(),
            updates: updates.id(),
        })
        .unwrap();

        Tensor::new(
            self.view().clone(),
            TensorInput::index(IndexType::ScatterElements, dependencies, serialized_params),
        )
    }

    pub fn Gather(&self, axis: ViewType, indices: &Tensor) -> Tensor {
        let indices_view = indices.view();
        let (batch_shape, gather_element_shape) = self.view().shape.split_at(axis as usize);

        let adjusted_shape = batch_shape
            .iter()
            .chain(indices_view.shape.iter())
            .chain(gather_element_shape.iter().skip(1))
            .map(|&x| x)
            .collect_vec();
        let adjusted_view = TensorView::from_shape(&adjusted_shape);

        // Could be done using a chain of squeeze/unsqueeze but explicitly construct the TensorView
        // instead here
        let transmuted_shape = batch_shape
            .iter()
            .map(|_| &1)
            .chain(indices_view.shape.iter())
            .chain(gather_element_shape.iter().skip(1).map(|_| &1))
            .map(|&n| n)
            .collect::<Vec<_>>();
        let transmuted_indices = indices.reshape(&TensorView::from_shape(&transmuted_shape));
        let broadcasted_indices = transmuted_indices.broadcast_to(&adjusted_view);

        self.GatherElements(axis, &broadcasted_indices)
    }

    pub fn GatherElements(&self, axis: ViewType, indices: &Tensor) -> Tensor {
        self.gather_op(axis, indices)
    }

    pub fn Scatter(&self, axis: ViewType, indices: &Tensor, updates: &Tensor) -> Tensor {
        self.ScatterElements(axis, ScatterReduction::None, indices, updates)
    }

    pub fn ScatterElements(
        &self,
        axis: ViewType,
        reduction: ScatterReduction,
        indices: &Tensor,
        updates: &Tensor,
    ) -> Tensor {
        if let ScatterReduction::None = reduction {
            self.scatter_op(axis, reduction, indices, updates)
        } else {
            panic!("No reduction are supported due to the lack of dispatch-level synchronization barriers");
        }
    }
}
