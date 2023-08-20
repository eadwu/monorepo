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

#[derive(Clone, Debug)]
pub enum TensorInput {
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
}
