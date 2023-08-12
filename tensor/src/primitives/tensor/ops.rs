mod binary;
use std::path::PathBuf;

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

impl TensorInput {
    pub fn from_raw(file: PathBuf, offset: usize) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Raw(RawSpec { file, offset }))
    }
}
