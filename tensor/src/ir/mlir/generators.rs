use crate::ir::mlir::ShaderIR;

mod wgsl;
pub use wgsl::*;

impl ShaderIR {
    pub fn variable(&self) -> String {
        format!("var_{}", self.id())
    }
}
