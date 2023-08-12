use std::path::PathBuf;

#[derive(Clone, Debug)]
pub enum InputSpec {
    Raw(RawSpec)
}

#[derive(Clone, Debug)]
pub struct RawSpec {
    pub file: PathBuf,
    pub offset: usize,
}
