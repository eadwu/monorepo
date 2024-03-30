use sha1::{Digest, Sha1};
use std::path::Path;

use filemanager::FileManager;

use crate::onnx;
use crate::onnx::tensor_proto::{DataLocation, DataType};
use crate::onnx::{ByteSize, TensorProto};

const HASH_CHUNK_SIZE: usize = 1024 * 1024 * 2;

impl TensorProto {
    fn parse_bytes(bytes: &[u8], data_type: DataType) -> f32 {
        match data_type {
            DataType::Bfloat16 => f32::from(half::bf16::from_le_bytes(bytes.try_into().unwrap())),
            DataType::Bool => u8::from_le_bytes(bytes.try_into().unwrap()) as f32,
            // DataType::Complex128 => (),
            // DataType::Complex64 => (),
            DataType::Double => f64::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Float => f32::from_le_bytes(bytes.try_into().unwrap()),
            DataType::Float16 => f32::from(half::f16::from_le_bytes(bytes.try_into().unwrap())),
            // DataType::Float8e4m3fn => (),
            // DataType::Float8e4m3fnuz => (),
            // DataType::Float8e5m2 => (),
            // DataType::Float8e5m2fnuz => (),
            DataType::Int16 => i16::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Int32 => i32::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Int64 => i64::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Int8 => i8::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::String => u8::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Uint16 => u16::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Uint32 => u32::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Uint64 => u64::from_le_bytes(bytes.try_into().unwrap()) as f32,
            DataType::Uint8 => u8::from_le_bytes(bytes.try_into().unwrap()) as f32,
            _ => panic!("Unable to extract ONNX datatype {:?}", data_type),
        }
    }

    fn load_slice(slice: &[u8], data_type: DataType) -> Vec<f32> {
        slice
            .chunks(data_type.size())
            .map(|chunk| TensorProto::parse_bytes(chunk, data_type))
            .collect::<Vec<_>>()
    }

    fn load_raw_data(&self) -> Vec<f32> {
        if self.raw_data.len() > 0 {
            TensorProto::load_slice(&self.raw_data[..], self.data_type())
        } else {
            match self.data_type() {
                DataType::Float | DataType::Complex64 => TensorProto::load_slice(
                    bytemuck::cast_slice(&self.float_data[..]),
                    self.data_type(),
                ),
                DataType::Int32
                | DataType::Int16
                | DataType::Int8
                | DataType::Uint16
                | DataType::Uint8
                | DataType::Bool
                | DataType::Float16
                | DataType::Bfloat16
                | DataType::Float8e4m3fn
                | DataType::Float8e4m3fnuz
                | DataType::Float8e5m2
                | DataType::Float8e5m2fnuz => TensorProto::load_slice(
                    bytemuck::cast_slice(&self.int32_data[..]),
                    self.data_type(),
                ),
                DataType::String => self
                    .string_data
                    .iter()
                    .flatten()
                    .map(|&x| x as f32)
                    .collect::<Vec<_>>(),
                DataType::Int64 => TensorProto::load_slice(
                    bytemuck::cast_slice(&self.int64_data[..]),
                    self.data_type(),
                ),
                DataType::Double | DataType::Complex128 => TensorProto::load_slice(
                    bytemuck::cast_slice(&self.double_data[..]),
                    self.data_type(),
                ),
                DataType::Uint32 | DataType::Uint64 => TensorProto::load_slice(
                    bytemuck::cast_slice(&self.uint64_data[..]),
                    self.data_type(),
                ),
                _ => panic!("{:?} is not supported", self.data_type()),
            }
        }
    }

    fn load_external_data(&self, model_path: &Path) -> Vec<f32> {
        assert!(
            self.data_location() == onnx::tensor_proto::DataLocation::External,
            "Only `external` stored TensorProto should reach here"
        );

        let external_data = self
            .external_data
            .iter()
            .map(|kv_proto| (kv_proto.key.clone(), kv_proto.value.clone()))
            .collect::<std::collections::HashMap<_, _>>();

        let location_key = "location";
        let offset_key = "offset";
        let length_key = "length";
        let checksum_key = "checksum";
        assert!(
            external_data.contains_key(location_key),
            "Key `{}` must be provided for ExternalData",
            location_key
        );

        let external_path = model_path.with_file_name(external_data.get(location_key).unwrap());
        let data = FileManager::new(1).open(&external_path, 0).unwrap();

        let offset = external_data
            .get(offset_key)
            .map(|offset| offset.parse::<usize>().unwrap())
            .unwrap_or(0);
        let length = external_data
            .get(length_key)
            .map(|length| length.parse::<usize>().unwrap())
            .unwrap_or(data.len());
        let expected_checksum = external_data.get(checksum_key);

        let data_type = self.data_type();
        assert!(
            length % data_type.size() == 0,
            "{} is not divisible by data type size {}",
            length,
            data_type.size()
        );

        let real_checksum = data
            .chunks(HASH_CHUNK_SIZE)
            .fold(Sha1::new(), |hasher, chunk| hasher.chain_update(chunk))
            .finalize();
        let real_checksum = hex::encode(real_checksum);
        assert!(
            expected_checksum
                .map(|checksum| checksum == &real_checksum)
                .unwrap_or(true),
            "Checksum don't match, expected {}, got {}",
            expected_checksum.unwrap(),
            &real_checksum
        );

        TensorProto::load_slice(&data[offset..offset + length], data_type)
    }

    pub fn load(&self, model_path: Option<&Path>) -> Vec<f32> {
        match self.data_location() {
            DataLocation::Default => self.load_raw_data(),
            DataLocation::External => self.load_external_data(model_path.unwrap()),
        }
    }
}
