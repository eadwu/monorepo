use crate::topograph::{GraphDependencies, UniqueIdentifier};
use crate::primitives::tensor::*;

impl UniqueIdentifier for Tensor {
    type Id = u32;
    fn id(&self) -> Self::Id {
        self.id()
    }
}

impl GraphDependencies for UnarySpec {
    type Dependency = Tensor;
    fn dependencies(&self) -> Vec<Self::Dependency> {
        vec![self.input.clone()]
    }
}

impl GraphDependencies for BinarySpec {
    type Dependency = Tensor;
    fn dependencies(&self) -> Vec<Self::Dependency> {
        vec![self.lhs.clone(), self.rhs.clone()]
    }
}

impl GraphDependencies for ReduceSpec {
    type Dependency = Tensor;
    fn dependencies(&self) -> Vec<Self::Dependency> {
        vec![self.input.clone()]
    }
}

impl GraphDependencies for OperationSpec {
    type Dependency = Tensor;
    fn dependencies(&self) -> Vec<Self::Dependency> {
        match self {
            OperationSpec::UnaryOp(spec) => spec.dependencies(),
            OperationSpec::BinaryOp(spec) => spec.dependencies(),
            OperationSpec::ReduceOp(spec) => spec.dependencies(),
        }
    }
}

impl GraphDependencies for Tensor {
    type Dependency = Tensor;
    fn dependencies(&self) -> Vec<Self::Dependency> {
        match &self.data() {
            TensorInput::NoOp(input) => vec![input.clone()],
            TensorInput::ExplicitInput(_) => vec![],
            TensorInput::OperationResult(result) => result.dependencies(),
            _ => panic!("Unsupported TensorInput to retrieve dependencies"),
        }
    }
}
