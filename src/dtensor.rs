pub mod primitives;

use wgpu;

pub type WgpuDevice = (wgpu::Device, wgpu::Queue);
