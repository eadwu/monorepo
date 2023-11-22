use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::primitives::tensor::TensorDataElement;
use crate::primitives::tensorview::ViewType;

use super::TensorInput;

#[derive(Clone, Debug)]
pub enum InputSpec {
    Scalar(String),
    Range(RangeSpec),
    Internal(InternalSpec),
    Safetensor(SafetensorSpec),
}

#[derive(Clone, Debug)]
pub struct RangeSpec {
    pub start: ViewType,
    pub end: ViewType,
    pub step: ViewType,
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
        TensorInput::ExplicitInput(InputSpec::Scalar(value.to_string()))
    }

    pub fn from_range(start: ViewType, end: ViewType, step: ViewType) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Range(RangeSpec { start, end, step }))
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
