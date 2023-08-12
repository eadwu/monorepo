use crate::primitives::tensor::Tensor;

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

pub struct BinarySpec {
    pub op: BinaryType,
    pub lhs: Tensor,
    pub rhs: Tensor,
}
