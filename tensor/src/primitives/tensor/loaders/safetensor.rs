use safetensors::{Dtype, SafeTensors};

use crate::primitives::tensor::{SafetensorSpec, Tensor};
use crate::FILE_MANAGER;

pub trait SafetensorLoader {
    fn load(self, spec: &SafetensorSpec) -> Vec<u8>;
}

impl SafetensorLoader for &Tensor {
    fn load(self, spec: &SafetensorSpec) -> Vec<u8> {
        let buffer = FILE_MANAGER.lock().unwrap().open(&spec.file, 0).unwrap();
        let safetensors = SafeTensors::deserialize(&buffer).unwrap();
        let tensor = safetensors.tensor(&spec.tensor).unwrap();

        let dtype = tensor.dtype();
        let converted_data = tensor
            .data()
            .chunks(dtype.size())
            .map(|chunk| parse_bytes(dtype, chunk))
            .collect::<Vec<_>>();

        bytemuck::cast_slice(&converted_data[..]).to_vec()
    }
}

fn parse_bytes(t: safetensors::Dtype, bytes: &[u8]) -> f32 {
    match t {
        Dtype::BF16 => f32::from(half::bf16::from_le_bytes(bytes.try_into().unwrap())),
        // Dtype::BOOL is a byte so restrict outcomes for bool conversion instead
        Dtype::BOOL => (u8::from_le_bytes(bytes.try_into().unwrap()) != 0) as u8 as f32,
        Dtype::F32 => f32::from_le_bytes(bytes.try_into().unwrap()),
        Dtype::F64 => f64::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::I16 => i16::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::I32 => i32::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::I64 => i64::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::I8 => i8::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::U16 => u16::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::U32 => u32::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::U64 => u64::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::U8 => u8::from_le_bytes(bytes.try_into().unwrap()) as f32,
        Dtype::F16 => f32::from(half::f16::from_le_bytes(bytes.try_into().unwrap())),
        _ => panic!("Unsupported Safetensor type {:?}", t),
    }
}
