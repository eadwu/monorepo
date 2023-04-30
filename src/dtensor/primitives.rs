pub mod ops;

use crate::dtensor;
use wgpu::{self, util::DeviceExt};

// Restrict to f32 for simplicity
type TensorType = f32;

#[derive(Debug)]
pub struct Tensor<'tensor> {
    // Tensor Shape
    rank: usize,
    shape: Vec<usize>,
    stride: Vec<usize>,
    contiguous_stride: Vec<usize>,

    // Extra State
    contiguous: bool,
    wgpu_device: &'tensor dtensor::WgpuDevice,

    // Data
    n: usize,
    data: wgpu::Buffer,
}

fn compute_contiguous_stride(shape: &[usize]) -> Vec<usize> {
    shape
        .iter()
        .rev()
        .scan(1, |state, &x| {
            let current_state = *state;
            *state = *state * x;
            Some(current_state)
        })
        .collect::<Vec<usize>>()
        .into_iter()
        .rev()
        .collect()
}

impl<'tensor> Tensor<'tensor> {
    // Constructors

    pub async fn new<T: bytemuck::Pod>(
        shape: &[usize],
        data: &[T],
        wgpu_device: &'tensor dtensor::WgpuDevice,
    ) -> Tensor<'tensor> {
        let (device, _) = wgpu_device;
        let data_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(data),
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::COPY_SRC,
        });

        Tensor::with_buffer(shape, data_buffer, wgpu_device)
    }

    pub async fn of_shape(shape: &[usize], wgpu_device: &'tensor dtensor::WgpuDevice) -> Tensor<'tensor> {
        let (device, _) = wgpu_device;

        let n_elements: usize = shape.iter().product();
        let size = n_elements * std::mem::size_of::<TensorType>();
        let data_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: size as wgpu::BufferAddress,
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });

        Tensor::with_buffer(shape, data_buffer, wgpu_device)
    }

    // Properties
    pub fn rank(&self) -> usize {
        self.rank
    }

    pub fn shape(&self) -> &[usize] {
        &self.shape
    }

    pub fn stride(&self) -> &[usize] {
        &self.stride
    }

    pub fn contiguous(&self) -> bool {
        self.contiguous
    }

    pub fn device(&self) -> &'tensor dtensor::WgpuDevice {
        self.wgpu_device
    }

    pub fn size(&self) -> usize {
        self.n * std::mem::size_of::<TensorType>()
    }

    pub fn len(&self) -> usize {
        self.n
    }

    // Private Helpers
    fn with_buffer(
        shape: &[usize],
        buffer: wgpu::Buffer,
        wgpu_device: &'tensor dtensor::WgpuDevice,
    ) -> Tensor<'tensor> {
        let rank = shape.len();
        let n_elements: usize = shape.iter().product();

        // Stride is the offset in terms of the `data` contiguous vector
        // Calculated by a running sum right-to-left
        let stride: Vec<usize> = compute_contiguous_stride(&shape);

        // STORAGE: Mark as a buffer usable as a Storage buffer for bind group
        // MAP_READ: Allow mapping over to the CPU to allow reading
        // MAP_WRITE: Allow mapping over to the CPU to allow modification
        // COPY_DST: Allow to be a destination for a copy
        // COPY_SRC: Allow to be a source for a copy
        let contiguous_stride = compute_contiguous_stride(&shape);

        Tensor {
            rank: rank,
            shape: shape.iter().map(|x| x.clone()).collect::<Vec<usize>>(),
            stride: stride,
            contiguous_stride: contiguous_stride,
            contiguous: true,
            wgpu_device: wgpu_device,
            n: n_elements,
            data: buffer,
        }
    }
}
