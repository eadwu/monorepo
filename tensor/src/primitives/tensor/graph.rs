use crate::primitives::tensor::*;

pub trait GraphDependencies {
    fn dependencies(&self) -> Vec<Tensor>;
}

impl GraphDependencies for UnarySpec {
    fn dependencies(&self) -> Vec<Tensor> {
        vec![self.input.clone()]
    }
}

impl GraphDependencies for BinarySpec {
    fn dependencies(&self) -> Vec<Tensor> {
        vec![self.lhs.clone(), self.rhs.clone()]
    }
}

impl GraphDependencies for ReduceSpec {
    fn dependencies(&self) -> Vec<Tensor> {
        vec![self.input.clone()]
    }
}

impl GraphDependencies for OperationSpec {
    fn dependencies(&self) -> Vec<Tensor> {
        match self {
            OperationSpec::UnaryOp(spec) => spec.dependencies(),
            OperationSpec::BinaryOp(spec) => spec.dependencies(),
            OperationSpec::ReduceOp(spec) => spec.dependencies(),
        }
    }
}

impl GraphDependencies for Tensor {
    fn dependencies(&self) -> Vec<Tensor> {
        match &self.data() {
            TensorInput::NoOp(input) => vec![input.clone()],
            TensorInput::ExplicitInput(_) => vec![],
            TensorInput::OperationResult(result) => result.dependencies(),
            _ => panic!("Unsupported TensorInput to retrieve dependencies"),
        }
    }
}
