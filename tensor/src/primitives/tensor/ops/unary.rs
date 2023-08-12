use crate::primitives::tensor::Tensor;

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
