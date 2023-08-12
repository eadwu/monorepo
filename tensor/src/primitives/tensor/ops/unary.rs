use crate::primitives::tensor::Tensor;

pub enum UnaryType {
    IDENTITY, // NOOP,
    EXP2,
    LOG2,
    // CAST,
    SIN,
    SQRT,
    RECIP,
}

pub struct UnarySpec {
    pub op: UnaryType,
    pub input: Tensor,
}
