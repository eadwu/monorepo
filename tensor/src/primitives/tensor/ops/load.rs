use std::{path::{Path, PathBuf}, sync::Arc};

use crate::primitives::tensor::TensorDataElement;

use super::TensorInput;

#[derive(Clone, Debug)]
pub enum InputSpec {
    Scalar(Vec<u8>),
    Internal(InternalSpec),
    Safetensor(SafetensorSpec),
}

#[derive(Clone, Debug)]
pub struct InternalSpec {
    pub path: Arc<PathBuf>,
}

#[derive(Clone, Debug)]
pub struct SafetensorSpec {
    pub file: PathBuf,
    pub tensor: String,
}

impl TensorInput {
    pub fn from_scalar<T: TensorDataElement>(value: T) -> TensorInput {
        let slice = [value];
        let bytes = bytemuck::cast_slice(&slice);
        TensorInput::ExplicitInput(InputSpec::Scalar(bytes.to_vec()))
    }

    pub fn from_internal(file: &Path) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Internal(InternalSpec {
            path: Arc::new(file.to_path_buf()),
        }))
    }

    pub fn from_safetensor(file: &Path, tensor: &str) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Safetensor(SafetensorSpec {
            file: file.to_path_buf(),
            tensor: tensor.to_string(),
        }))
    }
}
