use ::tensor::primitives::tensorview::{TensorView, TensorViewTracker, ViewType};
use num::integer::Roots;

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
    pub metadata: wgpu::Buffer,
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

impl From<&TensorView> for WebGPUWorkGroup {
    fn from(value: &TensorView) -> Self {
        let length = value.len();
        let z = length.cbrt();
        let y = ((length / z) + 1).sqrt();
        let x = length / (y * z) + 1;
        assert!(x as u64 * y as u64 * z as u64 >= length as u64);
        WebGPUWorkGroup::new(x, y, z)
    }
}

#[derive(Debug)]
pub struct TensorMetadata {
    pub length: ViewType,
    pub ndim: ViewType,
    pub nviews: ViewType,
    pub view_size: ViewType,
    pub shape_offset: ViewType,
    pub stride_offset: ViewType,
    pub contiguous_stride_offset: ViewType,
    pub metadata: Vec<ViewType>,
}

impl From<&TensorViewTracker> for TensorMetadata {
    fn from(viewtracker: &TensorViewTracker) -> Self {
        let length = viewtracker.len();
        let ndim = viewtracker.ndim();

        let view_history = viewtracker.serialized_history_fifo();
        let nviews = view_history.len() as ViewType;

        let shape_offset = 0;
        let stride_offset = shape_offset + ndim;
        let contiguous_stride_offset = stride_offset + ndim;
        let view_size = contiguous_stride_offset + ndim; // dimension * 3

        let static_metadata = [
            length,
            ndim,
            nviews,
            view_size,
            shape_offset,
            stride_offset,
            contiguous_stride_offset,
        ];
        let view_metadata = view_history
            .iter()
            .flat_map(|view| {
                view.shape
                    .iter()
                    .chain(view.stride.iter())
                    .chain(view.contiguous_stride.iter())
            })
            .map(|&x| x)
            // If it is a scalar then the metadata is 0 bytes
            // WebGPU does not like 0-length arrays, so append an extra 0
            .chain(std::iter::once(0));

        let metadata = static_metadata
            .into_iter()
            .chain(view_metadata)
            .collect::<Vec<_>>();

        TensorMetadata::new(
            length,
            ndim,
            nviews,
            view_size,
            shape_offset,
            stride_offset,
            contiguous_stride_offset,
            metadata,
        )
    }
}

impl TensorMetadata {
    pub fn new(
        length: ViewType,
        ndim: ViewType,
        nviews: ViewType,
        view_size: ViewType,
        shape_offset: ViewType,
        stride_offset: ViewType,
        contiguous_stride_offset: ViewType,
        metadata: Vec<ViewType>,
    ) -> TensorMetadata {
        TensorMetadata {
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

    pub fn serialize_definition() -> String {
        format!(
            "
struct TensorMetadata {{
    length: {ViewType},
    ndim: {ViewType},
    nviews: {ViewType},
    view_size: {ViewType},
    shape_offset: {ViewType},
    stride_offset: {ViewType},
    contiguous_stride_offset: {ViewType},
    metadata: {Vec}<{ViewType}>,
}}
",
            ViewType = "u32",
            Vec = "array",
        )
    }

    pub fn bytes(&self) -> &[u8] {
        bytemuck::cast_slice(&self.metadata)
    }
}
