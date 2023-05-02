pub mod primitives;

use wgpu;

pub struct WebGPU {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
}

pub type WgpuDevice = std::rc::Rc<WebGPU>;
