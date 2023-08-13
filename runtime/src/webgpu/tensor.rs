use tensor::primitives::tensor::{Tensor, TensorView, TensorType};
use wgpu::util::DeviceExt;

use super::{TensorLayout, WebGPUDevice, TensorMetadata};

const WEBGPU_MINIMUM_BUFFER_SIZE: usize = 16;
const WEBGPU_FLOAT4_ALIGNMENT: usize = std::mem::size_of::<TensorType>() * 4;

trait ToWebGPUBuffer {
    fn as_webgpu_buffer(&self, wgpu_device: &WebGPUDevice) -> wgpu::Buffer;
}

impl ToWebGPUBuffer for Tensor {
    fn as_webgpu_buffer(&self, wgpu_device: &WebGPUDevice) -> wgpu::Buffer {
        let WebGPUDevice { device, queue: _ } = wgpu_device;

        let data = self.load();
        let minimum_size = data.len().max(WEBGPU_MINIMUM_BUFFER_SIZE);
        let aligned_size =
            minimum_size + (WEBGPU_FLOAT4_ALIGNMENT - 1) & !(WEBGPU_FLOAT4_ALIGNMENT - 1);

        let buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: aligned_size as u64,
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_SRC
                | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: true,
        });
        buffer.slice(..).get_mapped_range_mut()[..data.len()].copy_from_slice(&data);
        buffer.unmap();
        buffer
    }
}

impl ToWebGPUBuffer for TensorView {
    fn as_webgpu_buffer(&self, wgpu_device: &WebGPUDevice) -> wgpu::Buffer {
        let WebGPUDevice { device, queue: _ } = wgpu_device;
        device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: Into::<TensorMetadata>::into(self).bytes(),
            usage: wgpu::BufferUsages::UNIFORM
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
        })
    }
}

pub trait ToWebGPUTensorLayout {
    fn as_webgpu_tensor(&self, wgpu_device: &WebGPUDevice) -> TensorLayout;
}

impl ToWebGPUTensorLayout for Tensor {
    fn as_webgpu_tensor(&self, wgpu_device: &WebGPUDevice) -> TensorLayout {
        TensorLayout {
            metadata: self.view().as_webgpu_buffer(wgpu_device),
            data: self.as_webgpu_buffer(wgpu_device),
        }
    }
}
