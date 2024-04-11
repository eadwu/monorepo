#![allow(unused, non_snake_case)]
use crate::onnx;

mod interface;
pub use interface::*;

pub mod v21;

impl onnx::NodeProto {
    fn io_interface(&self) -> Vec<String> {
        self.input
            .iter()
            .chain(self.output.iter())
            .map(|x| x.clone())
            .collect::<Vec<_>>()
    }
}
