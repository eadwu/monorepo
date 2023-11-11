use crate::primitives::tensor::{InternalSpec, Tensor};
use crate::FILE_MANAGER;

pub trait InternalLoader {
    fn load(self, spec: &InternalSpec) -> Vec<u8>;
}

impl InternalLoader for &Tensor {
    fn load(self, spec: &InternalSpec) -> Vec<u8> {
        let data = FILE_MANAGER.lock().unwrap().open(&spec.path, 0).unwrap();
        data.to_vec()
    }
}
