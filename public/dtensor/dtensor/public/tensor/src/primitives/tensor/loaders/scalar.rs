use crate::primitives::tensor::{Tensor, TensorType};

pub trait ScalarLoader {
    fn load<T: bytemuck::Pod>(self, value: &str) -> Vec<T>;
}

impl ScalarLoader for &Tensor {
    fn load<T: bytemuck::Pod>(self, value: &str) -> Vec<T> {
        let bytes = match self.datatype() {
            TensorType::F16 => (value.parse::<f32>().map(half::f16::from_f32).unwrap())
                .to_ne_bytes()
                .to_vec(),
            TensorType::F32 => value.parse::<f32>().unwrap().to_ne_bytes().to_vec(),
            TensorType::I32 => value.parse::<i32>().unwrap().to_ne_bytes().to_vec(),
            TensorType::U32 => value.parse::<u32>().unwrap().to_ne_bytes().to_vec(),
        };

        bytemuck::cast_slice(&bytes[..]).to_vec()
    }
}
