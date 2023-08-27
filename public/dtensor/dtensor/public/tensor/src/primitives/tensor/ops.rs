#![allow(non_snake_case)]
mod binary;
pub use binary::*;

mod unary;
pub use unary::*;

mod reduce;
pub use reduce::*;

mod view;
pub use view::*;

mod load;
pub use load::*;

mod index;
pub use index::*;

mod derived;
pub use derived::*;

use super::Tensor;

#[derive(Clone, Debug)]
pub enum TensorInput {
    NoOp(Tensor),
    ExplicitInput(InputSpec),
    OperationResult(OperationSpec),
    Invalidated,
}

#[derive(Clone, Debug)]
pub enum OperationSpec {
    UnaryOp(UnarySpec),
    BinaryOp(BinarySpec),
    ReduceOp(ReduceSpec),
    ViewOp(ViewSpec),
    IndexOp(IndexSpec),
}

impl TensorInput {
    pub fn no_op(input: Tensor) -> TensorInput {
        TensorInput::NoOp(input)
    }
}
