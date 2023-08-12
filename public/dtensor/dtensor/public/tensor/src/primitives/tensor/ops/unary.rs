use crate::primitives::tensor::Tensor;

use super::{OperationSpec, TensorInput};

#[derive(Clone, Copy, Debug)]
pub enum UnaryType {
    IDENTITY, // NOOP,
    EXP2,
    LOG2,
    // CAST,
    SIN,
    SQRT,
    RECIP,
}

#[derive(Clone, Debug)]
pub struct UnarySpec {
    pub op: UnaryType,
    pub input: Tensor,
}

impl TensorInput {
    pub fn unary(op: UnaryType, input: Tensor) -> TensorInput {
        TensorInput::OperationResult(OperationSpec::UnaryOp(UnarySpec { op, input }))
    }
}

impl Tensor {
    fn unary_op(&self, op: UnaryType) -> Tensor {
        Tensor::new(self.view().clone(), TensorInput::unary(op, self.clone()))
    }

    pub fn Identity(&self) -> Tensor {
        self.unary_op(UnaryType::IDENTITY)
    }

    pub fn Exp2(&self) -> Tensor {
        self.unary_op(UnaryType::EXP2)
    }

    pub fn Log2(&self) -> Tensor {
        self.unary_op(UnaryType::LOG2)
    }

    pub fn Sin(&self) -> Tensor {
        self.unary_op(UnaryType::SIN)
    }

    pub fn Sqrt(&self) -> Tensor {
        self.unary_op(UnaryType::SIN)
    }

    pub fn Recip(&self) -> Tensor {
        self.unary_op(UnaryType::RECIP)
    }
}
