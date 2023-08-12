use crate::primitives::tensor::Tensor;

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
