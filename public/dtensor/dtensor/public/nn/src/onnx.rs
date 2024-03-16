mod attributes;
pub(crate) use attributes::*;

mod internal {
    include!(concat!(env!("OUT_DIR"), "/onnx.rs"));
}
pub(crate) use internal::*;
