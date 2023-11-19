use num::integer::Roots;
use tensor::primitives::tensor::{Tensor, TensorType};
use tensor::primitives::tensorview::{TensorView, ViewType};

use super::{TensorLayout, WebGPUDevice, WebGPUTensor, WebGPUWorkGroup};

const WEBGPU_MINIMUM_BUFFER_SIZE: usize = 16;
const WEBGPU_VEC4_ALIGNMENT: usize = TensorType::F32.byte_size() * 4;
const _: () = assert!(
    WEBGPU_VEC4_ALIGNMENT & (WEBGPU_VEC4_ALIGNMENT - 1) == 0,
    "WEBGPU_FLOAT4_ALIGNMENT must be a power of 2"
);

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

trait ToWebGPUBuffer {
    fn as_webgpu_buffer(&self, wgpu_device: &WebGPUDevice) -> wgpu::Buffer;
}

impl ToWebGPUBuffer for Tensor {
    fn as_webgpu_buffer(&self, wgpu_device: &WebGPUDevice) -> wgpu::Buffer {
        let WebGPUDevice { device, queue: _ } = wgpu_device;

        let data_len_bytes = self.data_len() as usize * self.datatype().byte_size();
        let minimum_size = data_len_bytes.max(WEBGPU_MINIMUM_BUFFER_SIZE);
        let aligned_size =
            minimum_size + (WEBGPU_VEC4_ALIGNMENT - 1) & !(WEBGPU_VEC4_ALIGNMENT - 1);

        #[cfg(feature = "wgpu_direct_buffer")]
        let buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: aligned_size as u64,
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::MAP_READ
                | wgpu::BufferUsages::MAP_WRITE,
            mapped_at_creation: true,
        });
        #[cfg(not(feature = "wgpu_direct_buffer"))]
        let buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: aligned_size as u64,
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: true,
        });

        if self.has_data() {
            let data = self.load();
            buffer.slice(..).get_mapped_range_mut()[..data.len()].copy_from_slice(&data[..]);
        }

        buffer.unmap();
        buffer
    }
}

pub trait ToWebGPUTensorLayout {
    fn as_webgpu_tensor(&self, wgpu_device: &WebGPUDevice) -> TensorLayout;
}

impl ToWebGPUTensorLayout for Tensor {
    fn as_webgpu_tensor(&self, wgpu_device: &WebGPUDevice) -> TensorLayout {
        TensorLayout {
            data: self.as_webgpu_buffer(wgpu_device),
        }
    }
}

pub trait ToWebGPUBindGroup {
    fn as_webgpu_bind_group(
        &self,
        bind_group_layout: &wgpu::BindGroupLayout,
        wgpu_device: &WebGPUDevice,
    ) -> wgpu::BindGroup;
}

impl ToWebGPUBindGroup for TensorLayout {
    fn as_webgpu_bind_group(
        &self,
        bind_group_layout: &wgpu::BindGroupLayout,
        wgpu_device: &WebGPUDevice,
    ) -> wgpu::BindGroup {
        let WebGPUDevice { device, queue: _ } = wgpu_device;

        device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: self.data.as_entire_binding(),
            }],
        })
    }
}

impl From<&Tensor> for WebGPUTensor {
    fn from(value: &Tensor) -> Self {
        WebGPUTensor::new(&value.id().to_string())
    }
}
