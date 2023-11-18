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

        let tensor_metadata = Into::<WebGPUTensor>::into(self);
        let metadata_bytes = tensor_metadata.bytes();
        let metadata_len_bytes = metadata_bytes.len();

        let data_len_bytes = self.data_len() as usize * self.datatype().byte_size();
        let total_len_bytes = metadata_len_bytes + data_len_bytes;

        let minimum_size = total_len_bytes.max(WEBGPU_MINIMUM_BUFFER_SIZE);
        let aligned_size =
            minimum_size + (WEBGPU_VEC4_ALIGNMENT - 1) & !(WEBGPU_VEC4_ALIGNMENT - 1);

        let buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: aligned_size as u64,
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_SRC
                | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: true,
        });

        {
            let mut buffer_data = buffer.slice(..).get_mapped_range_mut();
            buffer_data[..metadata_len_bytes].copy_from_slice(&metadata_bytes[..]);
            if self.has_data() {
                let data = self.load();
                let end = metadata_len_bytes + data.len();
                buffer_data[metadata_len_bytes..end].copy_from_slice(&data[..]);
            }
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
        let viewtracker = value
            .viewtracker()
            .align_to_ndim(value.viewtracker().max_ndim());
        let length = viewtracker.len();
        let ndim = viewtracker.ndim();

        let view_history = viewtracker.serialized_history_fifo();
        let nviews = view_history.len() as ViewType;

        let shape_offset = 0;
        let stride_offset = shape_offset + ndim;
        let contiguous_stride_offset = stride_offset + ndim;
        let view_size = contiguous_stride_offset + ndim; // dimension * 3

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
            .chain(std::iter::once(0))
            .collect::<Vec<_>>();

        WebGPUTensor::new(
            &value.id().to_string(),
            length,
            ndim,
            nviews,
            view_size,
            shape_offset,
            stride_offset,
            contiguous_stride_offset,
            view_metadata,
        )
    }
}
