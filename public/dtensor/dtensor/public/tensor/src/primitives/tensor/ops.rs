#![allow(non_snake_case)]
mod binary;
pub use binary::*;

mod unary;
pub use unary::*;

mod view;
pub use view::*;

mod load;
pub use load::*;

#[derive(Clone, Debug)]
pub enum TensorInput {
    ExplicitInput(InputSpec),
    OperationResult(OperationSpec),
}

#[derive(Clone, Debug)]
pub enum OperationSpec {
    UnaryOp(UnarySpec),
    BinaryOp(BinarySpec),
    ViewOp(ViewSpec),
}
