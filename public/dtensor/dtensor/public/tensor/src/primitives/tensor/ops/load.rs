use std::path::{Path, PathBuf};

use super::TensorInput;

#[derive(Clone, Debug)]
pub enum InputSpec {
    Raw(RawSpec),
    Safetensor(SafetensorSpec),
}

#[derive(Clone, Debug)]
pub struct RawSpec {
    pub file: PathBuf,
    pub size: usize,
    pub offset: usize,
}

#[derive(Clone, Debug)]
pub struct SafetensorSpec {
    pub file: PathBuf,
    pub tensor: String,
}

impl TensorInput {
    pub fn from_raw(file: &Path, size: usize, offset: usize) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Raw(RawSpec {
            file: file.to_path_buf(),
            size: size,
            offset: offset,
        }))
    }

    pub fn from_safetensor(file: &Path, tensor: &str) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Safetensor(SafetensorSpec {
            file: file.to_path_buf(),
            tensor: tensor.to_string(),
        }))
    }
}
