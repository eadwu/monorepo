use std::io::Result;

fn main() -> Result<()> {
    prost_build::compile_protos(&["onnx.proto3"], &["../third_party/onnx/onnx"])?;
    Ok(())
}
