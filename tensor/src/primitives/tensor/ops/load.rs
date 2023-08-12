use std::path::PathBuf;

pub enum InputSpec {
    Raw(RawSpec)
}

pub struct RawSpec {
    pub file: PathBuf,
    pub offset: usize,
}
