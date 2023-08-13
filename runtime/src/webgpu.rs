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
