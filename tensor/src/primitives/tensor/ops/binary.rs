use crate::primitives::tensor::Tensor;

use super::{OperationSpec, TensorInput};

#[derive(Clone, Copy, Debug)]
pub enum BinaryType {
    ADD,
    SUB,
    MULTIPLY,
    DIVIDE,
    MAX,
    MOD,
    EQUAL,
    LESSTHAN,
}

#[derive(Clone, Debug)]
pub struct BinarySpec {
    pub op: BinaryType,
    pub lhs: Tensor,
    pub rhs: Tensor,
}

impl TensorInput {
    pub fn binary(op: BinaryType, lhs: Tensor, rhs: Tensor) -> TensorInput {
        TensorInput::OperationResult(OperationSpec::BinaryOp(BinarySpec { op, lhs, rhs }))
    }
}

impl Tensor {
    fn binary_op(&self, op: BinaryType, rhs: &Tensor) -> Tensor {
        let datatype = self.datatype().agreeable_type(rhs.datatype());
        let lhs = self.broadcast(&rhs).Cast(datatype);
        let rhs = rhs.broadcast(&self).Cast(datatype);

        Tensor::new(
            lhs.view().clone(),
            TensorInput::binary(op, lhs, rhs),
            datatype,
        )
    }

    pub fn Add(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::ADD, rhs)
    }

    pub fn Sub(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::SUB, rhs)
    }

    pub fn Multiply(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::MULTIPLY, rhs)
    }

    pub fn Divide(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::DIVIDE, rhs)
    }

    pub fn Maximum(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::MAX, rhs)
    }

    pub fn Mod(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::MOD, rhs)
    }

    pub fn Equal(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::EQUAL, rhs)
    }

    pub fn LessThan(&self, rhs: &Tensor) -> Tensor {
        self.binary_op(BinaryType::LESSTHAN, rhs)
    }
}
