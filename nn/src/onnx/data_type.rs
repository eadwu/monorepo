use crate::onnx;
use crate::onnx::tensor_proto::DataType;

impl onnx::TensorProto {
    pub fn data_type(&self) -> DataType {
        DataType::try_from(self.data_type).unwrap()
    }
}

pub trait ByteSize {
    fn size(&self) -> usize;
}

impl ByteSize for DataType {
    fn size(&self) -> usize {
        match self {
            DataType::Bfloat16 => 2,
            DataType::Bool => 1,
            DataType::Complex128 => 16,
            DataType::Complex64 => 8,
            DataType::Double => 8,
            DataType::Float => 4,
            DataType::Float16 => 2,
            DataType::Float8e4m3fn => 1,
            DataType::Float8e4m3fnuz => 1,
            DataType::Float8e5m2 => 1,
            DataType::Float8e5m2fnuz => 1,
            DataType::Int16 => 2,
            DataType::Int32 => 4,
            DataType::Int64 => 8,
            DataType::Int8 => 1,
            DataType::String => 1,
            DataType::Uint16 => 2,
            DataType::Uint32 => 4,
            DataType::Uint64 => 8,
            DataType::Uint8 => 1,
            // DataType::Undefined => (),
            _ => panic!(
                "Unable to determine size of ONNX datatype in bytes {:?}",
                self
            ),
        }
    }
}
