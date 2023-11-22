use crate::primitives::tensor::{Tensor, TensorType};
use crate::primitives::tensorview::TensorView;

use super::{OperationSpec, TensorInput};

#[derive(Clone, Copy, Debug)]
pub enum UnaryType {
    IDENTITY,
    EXP2,
    LOG2,
    CAST,
    SIN,
    SQRT,
    ABS,
    FLOOR,
    CEIL,
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
    fn unary_op(&self, op: UnaryType, datatype: TensorType) -> Tensor {
        Tensor::new(
            TensorView::from_contiguous_shape(self.shape()),
            TensorInput::unary(op, self.clone()),
            datatype,
        )
    }

    fn upgradable_unary_op(&self, op: UnaryType, datatype: TensorType) -> Tensor {
        self.Cast(datatype).unary_op(op, datatype)
    }

    fn noop_unary_op(
        &self,
        op: UnaryType,
        datatype: TensorType,
        noop_datatypes: &[TensorType],
    ) -> Tensor {
        if noop_datatypes
            .iter()
            .any(|&datatype| datatype == self.datatype())
        {
            self.clone()
        } else {
            self.upgradable_unary_op(op, datatype)
        }
    }

    pub fn Identity(&self) -> Tensor {
        self.unary_op(UnaryType::IDENTITY, self.datatype())
    }

    pub fn Exp2(&self) -> Tensor {
        self.upgradable_unary_op(UnaryType::EXP2, TensorType::F32)
    }

    pub fn Log2(&self) -> Tensor {
        self.upgradable_unary_op(UnaryType::LOG2, TensorType::F32)
    }

    pub fn Cast(&self, datatype: TensorType) -> Tensor {
        if self.datatype() == datatype {
            self.clone()
        } else {
            self.unary_op(UnaryType::CAST, datatype)
        }
    }

    pub fn Sin(&self) -> Tensor {
        self.upgradable_unary_op(UnaryType::SIN, TensorType::F32)
    }

    pub fn Sqrt(&self) -> Tensor {
        self.upgradable_unary_op(UnaryType::SQRT, TensorType::F32)
    }

    pub fn Abs(&self) -> Tensor {
        self.unary_op(UnaryType::ABS, self.datatype())
    }

    pub fn Floor(&self) -> Tensor {
        self.noop_unary_op(
            UnaryType::FLOOR,
            TensorType::F32,
            &[TensorType::I32, TensorType::U32],
        )
    }

    pub fn Ceil(&self) -> Tensor {
        self.noop_unary_op(
            UnaryType::CEIL,
            TensorType::F32,
            &[TensorType::I32, TensorType::U32],
        )
    }
}
