use super::WebGPUDevice;

#[derive(Clone, Copy, Debug)]
pub enum WebGPUEncoderTimestamps {
    Start = 0,
    ComputePassConfigured = 1,
    ComputePassFinished = 2,
    OutputCopyToCpuStart = 3,
    OutputCopyToCpuEnd = 4,
    End = 5,
}

impl WebGPUEncoderTimestamps {
    pub fn size() -> u32 {
        WebGPUEncoderTimestamps::End as u32 + 1
    }
}

pub enum WebGPUComputePassTimestamps {
    Start = 0,
    End = 1,
}

impl WebGPUComputePassTimestamps {
    pub fn size() -> u32 {
        WebGPUComputePassTimestamps::End as u32 + 1
    }
}

pub struct WebGPUTimestamps {
    pub n_timestamps: u32,
    pub query_set: wgpu::QuerySet,
}

impl WebGPUTimestamps {
    pub fn new(n_timestamps: u32, wgpu_device: &WebGPUDevice) -> WebGPUTimestamps {
        let WebGPUDevice { device, queue: _ } = wgpu_device;
        let query_set = device.create_query_set(&wgpu::QuerySetDescriptor {
            label: None,
            count: n_timestamps as _,
            ty: wgpu::QueryType::Timestamp,
        });

        WebGPUTimestamps {
            n_timestamps,
            query_set,
        }
    }

    pub fn total_size_bytes(&self) -> u64 {
        std::mem::size_of::<u64>() as u64 * self.n_timestamps as u64
    }

    pub fn resolve_query_set(
        &self,
        wgpu_encoder: &mut wgpu::CommandEncoder,
        wgpu_device: &WebGPUDevice,
    ) -> wgpu::Buffer {
        let WebGPUDevice { device, queue: _ } = wgpu_device;
        let resolved_results = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: self.total_size_bytes(),
            usage: wgpu::BufferUsages::QUERY_RESOLVE | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });

        wgpu_encoder.resolve_query_set(
            &self.query_set,
            // NOTE: https://github.com/gfx-rs/wgpu/issues/3993
            // Musn't be larger than the number valid queries in the set.
            0..self.n_timestamps,
            &resolved_results,
            0,
        );

        let mappable_results = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: resolved_results.size(),
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: false,
        });

        wgpu_encoder.copy_buffer_to_buffer(
            &resolved_results,
            0,
            &mappable_results,
            0,
            resolved_results.size(),
        );

        mappable_results
    }

    pub fn read_results(
        &self,
        mappable_results: &wgpu::Buffer,
        wgpu_device: &WebGPUDevice,
    ) -> Vec<u64> {
        let WebGPUDevice { device, queue: _ } = wgpu_device;

        mappable_results
            .slice(..)
            .map_async(wgpu::MapMode::Read, |_| ());
        device.poll(wgpu::Maintain::Wait);

        let timestamps = {
            let timestamp_view = mappable_results
                .slice(..self.total_size_bytes())
                .get_mapped_range();
            bytemuck::cast_slice(&timestamp_view).to_vec()
        };

        mappable_results.unmap();
        timestamps
    }
}
