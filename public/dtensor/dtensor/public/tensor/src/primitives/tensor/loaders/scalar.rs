use crate::primitives::tensor::{Tensor, TensorType};

pub trait ScalarLoader {
    fn load(self, value: &str) -> Vec<u8>;
}

impl ScalarLoader for &Tensor {
    fn load(self, value: &str) -> Vec<u8> {
        match self.datatype() {
            TensorType::F16 => (value.parse::<f32>().map(half::f16::from_f32).unwrap())
                .to_le_bytes()
                .to_vec(),
            TensorType::F32 => bytemuck::cast_slice(&[value.parse::<f32>().unwrap()]).to_vec(),
            TensorType::I32 => bytemuck::cast_slice(&[value.parse::<i32>().unwrap()]).to_vec(),
            TensorType::U32 => bytemuck::cast_slice(&[value.parse::<u32>().unwrap()]).to_vec(),
        }
    }
}
