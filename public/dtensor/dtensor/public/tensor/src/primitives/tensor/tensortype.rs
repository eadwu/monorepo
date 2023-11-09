pub trait TensorDataElement = Into<TensorType> + bytemuck::Pod;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TensorType {
    I32,
    U32,
    F32,
    F16,
}

impl TensorType {
    pub const fn byte_size(&self) -> usize {
        match self {
            TensorType::I32 => 4,
            TensorType::U32 => 4,
            TensorType::F32 => 4,
            TensorType::F16 => 2,
        }
    }

    pub fn agreeable_type(self, other: TensorType) -> TensorType {
        if self == other {
            self
        } else {
            TensorType::F32
        }
    }
}

impl From<f32> for TensorType {
    fn from(_: f32) -> Self {
        TensorType::F32
    }
}

