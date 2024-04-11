mod attributes;
pub(crate) use attributes::*;

mod bytes_parser;
pub(crate) use bytes_parser::*;

mod data_type;
pub(crate) use data_type::*;

mod runtime;
pub use runtime::*;

mod internal {
    include!(concat!(env!("OUT_DIR"), "/onnx.rs"));
}
pub(crate) use internal::*;
