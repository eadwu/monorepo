use std::iter::once;

use ::tensor::primitives::tensor::{TensorView, ViewType};

mod tensor;
pub use tensor::*;

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
pub struct TensorMetadata {
    pub length: ViewType,
    pub dimension: ViewType,
    pub shape_offset: ViewType,
    pub stride_offset: ViewType,
    pub contiguous_stride_offset: ViewType,
    pub offset_offset: ViewType,
    pub metadata: Vec<ViewType>,
}

impl From<&TensorView> for TensorMetadata {
    fn from(view: &TensorView) -> Self {
        let length = view.len();
        let dimension = view.dimension();

        let shape_offset = 0;
        let stride_offset = shape_offset + dimension;
        let contiguous_stride_offset = stride_offset + dimension;
        let offset_offset = contiguous_stride_offset + dimension;

        let metadata = once(&length)
            .chain(once(&dimension))
            .chain(once(&shape_offset))
            .chain(once(&stride_offset))
            .chain(once(&contiguous_stride_offset))
            .chain(once(&offset_offset))
            .chain(view.shape.iter())
            .chain(view.stride.iter())
            .chain(view.contiguous_stride.iter())
            .chain(view.offset.iter())
            .map(|&x| x)
            .collect::<Vec<_>>();

        TensorMetadata::new(
            length,
            dimension,
            shape_offset,
            stride_offset,
            contiguous_stride_offset,
            offset_offset,
            metadata,
        )
    }
}

impl TensorMetadata {
    pub fn new(
        length: ViewType,
        dimension: ViewType,
        shape_offset: ViewType,
        stride_offset: ViewType,
        contiguous_stride_offset: ViewType,
        offset_offset: ViewType,
        metadata: Vec<ViewType>,
    ) -> TensorMetadata {
        TensorMetadata {
            length,
            dimension,
            shape_offset,
            stride_offset,
            contiguous_stride_offset,
            offset_offset,
            metadata,
        }
    }

    pub fn bytes(&self) -> &[u8] {
        bytemuck::cast_slice(&self.metadata)
    }
}
