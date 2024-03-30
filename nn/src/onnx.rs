mod attributes;
pub(crate) use attributes::*;

mod bytes_parser;
pub(crate) use bytes_parser::*;

mod internal {
    include!(concat!(env!("OUT_DIR"), "/onnx.rs"));
}
pub(crate) use internal::*;
