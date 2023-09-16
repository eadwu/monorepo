use crate::primitives::tensor::{RawSpec, Tensor};
use crate::FILE_MANAGER;

pub trait RawFileLoader {
    fn load(self, spec: &RawSpec) -> Vec<u8>;
}

impl RawFileLoader for &Tensor {
    fn load(self, spec: &RawSpec) -> Vec<u8> {
        let data = FILE_MANAGER
            .lock()
            .unwrap()
            .open(&spec.file, spec.offset, spec.size)
            .unwrap();
        let bytes = &data[..spec.size];
        return bytes.to_vec();
    }
}
