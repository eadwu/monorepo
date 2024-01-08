pub mod benchmark;

pub mod generators;

pub(crate) mod utils;

mod tensor;
pub use tensor::*;

mod pipeline;
pub use pipeline::*;

const WORKGROUP_SIZE: WebGPUWorkGroup = WebGPUWorkGroup { x: 4, y: 4, z: 4 };
const MAXIMUM_DISPATCH_WORKGROUP_DIMENSION: usize = 65535;

#[derive(Debug)]
pub struct WebGPUDevice {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
}

#[derive(Debug)]
pub struct TensorLayout {
    pub data: wgpu::Buffer,
}

#[derive(Debug)]
pub struct WebGPUWorkGroup {
    pub x: u32,
    pub y: u32,
    pub z: u32,
}

impl WebGPUWorkGroup {
    pub fn new(x: u32, y: u32, z: u32) -> WebGPUWorkGroup {
        assert!(
            (x as usize) <= MAXIMUM_DISPATCH_WORKGROUP_DIMENSION,
            "{} exceeds maximum workgroup dimension",
            x
        );
        assert!(
            (y as usize) <= MAXIMUM_DISPATCH_WORKGROUP_DIMENSION,
            "{} exceeds maximum workgroup dimension",
            y
        );
        assert!(
            (z as usize) <= MAXIMUM_DISPATCH_WORKGROUP_DIMENSION,
            "{} exceeds maximum workgroup dimension",
            z
        );

        WebGPUWorkGroup { x, y, z }
    }

    pub fn serialize_strides(&self, variable_name: &str) -> String {
        format!(
            "
const {variable_name}: vec3u = vec3u({stride_x}u, {stride_y}u, {stride_z}u);
",
            variable_name = variable_name,
            stride_x = self.y * self.z,
            stride_y = self.z,
            stride_z = 1
        )
    }

    pub fn serialize_decorator(&self) -> String {
        format!(
            "@workgroup_size({x}, {y}, {z})",
            x = self.x,
            y = self.y,
            z = self.z,
        )
    }
}

#[derive(Debug)]
pub struct WebGPUTensor {
    pub identifier: String,
}

impl WebGPUTensor {
    pub fn new(identifier: &str) -> WebGPUTensor {
        let identifier = identifier.to_string();
        WebGPUTensor { identifier }
    }

    pub fn name(&self) -> String {
        format!("tensor_{UniqueId}", UniqueId = self.identifier)
    }

    pub fn serialize_type(
        &self,
        data_wgsl_type: &str,
        bind_group: &str,
        permission: &str,
    ) -> String {
        format!(
            "
@group({BindGroup}) @binding(0) var<storage, {StoragePermission}> {TensorName}: {Vec}<{DataType}>;
",
            Vec = "array",
            DataType = data_wgsl_type,
            BindGroup = bind_group,
            StoragePermission = permission,
            TensorName = self.name()
        )
    }
}
