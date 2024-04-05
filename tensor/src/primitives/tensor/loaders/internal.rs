use crate::primitives::tensor::{InternalSpec, Tensor};
use crate::FILE_MANAGER;

pub trait InternalLoader {
    fn load<T: bytemuck::Pod>(self, spec: &InternalSpec) -> Vec<T>;
}

impl InternalLoader for &Tensor {
    fn load<T: bytemuck::Pod>(self, spec: &InternalSpec) -> Vec<T> {
        let data = FILE_MANAGER.lock().unwrap().open(&spec.path, 0).unwrap();
        bytemuck::cast_slice(&data[..]).to_vec()
    }
}
