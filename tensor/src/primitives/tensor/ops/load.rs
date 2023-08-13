use std::path::{Path, PathBuf};

use super::TensorInput;

#[derive(Clone, Debug)]
pub enum InputSpec {
    Raw(RawSpec),
}

#[derive(Clone, Debug)]
pub struct RawSpec {
    pub file: PathBuf,
    pub size: usize,
    pub offset: usize,
}

impl TensorInput {
    pub fn from_raw(file: &Path, size: usize, offset: usize) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Raw(RawSpec {
            file: file.to_path_buf(),
            size: size,
            offset: offset,
        }))
    }
}
