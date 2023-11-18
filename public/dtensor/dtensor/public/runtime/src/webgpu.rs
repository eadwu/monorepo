pub mod generators;

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
    pub length: u32,
    pub ndim: u32,
    pub nviews: u32,
    pub view_size: u32,
    pub shape_offset: u32,
    pub stride_offset: u32,
    pub contiguous_stride_offset: u32,
    pub metadata: Vec<u32>,
}

impl WebGPUTensor {
    pub fn new(
        identifier: &str,
        length: u32,
        ndim: u32,
        nviews: u32,
        view_size: u32,
        shape_offset: u32,
        stride_offset: u32,
        contiguous_stride_offset: u32,
        metadata: Vec<u32>,
    ) -> WebGPUTensor {
        let identifier = identifier.to_string();
        WebGPUTensor {
            identifier,
            length,
            ndim,
            nviews,
            view_size,
            shape_offset,
            stride_offset,
            contiguous_stride_offset,
            metadata,
        }
    }

    pub fn name(&self) -> String {
        format!("Tensor_{UniqueId}", UniqueId = self.identifier)
    }

    fn type_name(&self) -> String {
        format!("WebGPUTensor_{UniqueId}", UniqueId = self.identifier)
    }

    pub fn serialize_type(
        &self,
        data_wgsl_type: &str,
        bind_group: &str,
        permission: &str,
    ) -> String {
        format!(
            "
struct {TensorType} {{
    length: {ViewType},
    ndim: {ViewType},
    nviews: {ViewType},
    view_size: {ViewType},
    shape_offset: {ViewType},
    stride_offset: {ViewType},
    contiguous_stride_offset: {ViewType},
    metadata: {Vec}<{ViewType}, {MetadataLength}>,
    data: {Vec}<{DataType}>,
}}

@group({BindGroup}) @binding(0) var<storage, {StoragePermission}> {TensorName}: {TensorType};
",
            TensorType = self.type_name(),
            ViewType = "u32",
            Vec = "array",
            MetadataLength = self.metadata.len(),
            DataType = data_wgsl_type,
            BindGroup = bind_group,
            StoragePermission = permission,
            TensorName = self.name()
        )
    }

    pub fn bytes(&self) -> Vec<u8> {
        let static_metadata = [
            self.length,
            self.ndim,
            self.nviews,
            self.view_size,
            self.shape_offset,
            self.stride_offset,
            self.contiguous_stride_offset,
        ];

        let metadata_all = static_metadata
            .iter()
            .chain(self.metadata.iter())
            .map(|&x| x)
            .collect::<Vec<_>>();

        bytemuck::cast_slice(&metadata_all[..]).to_vec()
    }
}
