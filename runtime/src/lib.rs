#[cfg(all(feature = "dtensor_shader_stitch", feature = "dtensor_shader_collapse"))]
compile_error!("feature \"dtensor_shader_stitch\" and feature \"dtensor_shader_collapse\" cannot be enabled at the same time");

pub mod webgpu;
