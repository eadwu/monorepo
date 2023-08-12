use std::path::{Path, PathBuf};

use super::TensorInput;

#[derive(Clone, Debug)]
pub enum InputSpec {
    Raw(RawSpec),
}

#[derive(Clone, Debug)]
pub struct RawSpec {
    pub file: PathBuf,
    pub offset: usize,
}

impl TensorInput {
    pub fn from_raw(file: &Path, offset: usize) -> TensorInput {
        TensorInput::ExplicitInput(InputSpec::Raw(RawSpec {
            file: file.to_path_buf(),
            offset: offset,
        }))
    }
}
