use itertools::{EitherOrBoth::*, Itertools};
use std::{borrow::Borrow, ops};

use crate::dtensor;
use impl_ops::*;
use wgpu::{self, util::DeviceExt};

// Restrict to f32 for simplicity
type TensorType = f32;

pub struct Tensor {
    // Tensor Shape
    rank: usize,
    shape: Vec<usize>,
    stride: Vec<usize>,
    contiguous_stride: Vec<usize>,

    // Extra State
    contiguous: bool,
    wgpu_device: dtensor::WgpuDevice,

    // Data
    n: usize,
    data: wgpu::Buffer,
}

#[repr(C)]
#[derive(bytemuck::Pod, Copy, Clone, bytemuck::Zeroable)]
struct TensorMetadata {
    rank: u32,
    length: u32,
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

impl Tensor {
    // Constructors
    pub async fn literal(literal: f32, wgpu_device: &dtensor::WgpuDevice) -> Tensor {
        Tensor::new(&[1], &[literal], wgpu_device).await
    }

    pub async fn new<T: bytemuck::Pod>(
        shape: &[usize],
        data: &[T],
        wgpu_device: &dtensor::WgpuDevice,
    ) -> Tensor {
        let stride = compute_contiguous_stride(&shape);
        let dtensor::WebGPU { device, queue: _ } = wgpu_device.borrow();
        let data_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(data),
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
        });

        Tensor::with_strided_buffer(shape, &stride, data_buffer, wgpu_device)
    }

    pub async fn of_shape(shape: &[usize], wgpu_device: &dtensor::WgpuDevice) -> Tensor {
        let stride = compute_contiguous_stride(&shape);
        let dtensor::WebGPU { device, queue: _ } = wgpu_device.borrow();

        let n_elements: usize = shape.iter().product();
        let size = n_elements * std::mem::size_of::<TensorType>();
        let data_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: size as wgpu::BufferAddress,
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });

        Tensor::with_strided_buffer(shape, &stride, data_buffer, wgpu_device)
    }

    // Properties
    pub fn buffer(&self) -> &wgpu::Buffer {
        &self.data
    }

    pub fn rank(&self) -> usize {
        self.rank
    }

    pub fn shape(&self) -> &[usize] {
        &self.shape
    }

    pub fn stride(&self) -> &[usize] {
        &self.stride
    }

    pub fn contiguous_stride(&self) -> &[usize] {
        &self.contiguous_stride
    }

    pub fn contiguous(&self) -> bool {
        self.contiguous
    }

    pub fn device(&self) -> &dtensor::WebGPU {
        self.wgpu_device.borrow()
    }

    pub fn wgpu_device(&self) -> &dtensor::WgpuDevice {
        &self.wgpu_device
    }

    pub fn size(&self) -> usize {
        self.n * std::mem::size_of::<TensorType>()
    }

    pub fn len(&self) -> usize {
        self.n
    }

    // Utilities
    pub fn buffer_with(&self, usage: wgpu::BufferUsages) -> wgpu::Buffer {
        let dtensor::WebGPU { device, queue } = self.device();

        let new_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: self.size() as u64,
            usage: usage,
            mapped_at_creation: false,
        });

        let mut encoder =
            device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        encoder.copy_buffer_to_buffer(&self.data, 0, &new_buffer, 0, self.size() as u64);
        queue.submit(Some(encoder.finish()));

        new_buffer
    }

    pub fn bind_group(&self, bind_group_layout: &wgpu::BindGroupLayout) -> wgpu::BindGroup {
        let dtensor::WebGPU { device, queue: _ } = self.device();

        let shape = usize_to_u32(self.shape());
        let shape_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(&shape),
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
        });

        let stride = usize_to_u32(self.stride());
        let stride_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(&stride),
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
        });

        let contiguous_stride = self::usize_to_u32(self.contiguous_stride());
        let contiguous_stride_buffer =
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: bytemuck::cast_slice(&contiguous_stride),
                usage: wgpu::BufferUsages::STORAGE
                    | wgpu::BufferUsages::COPY_DST
                    | wgpu::BufferUsages::COPY_SRC,
            });

        let metadata = TensorMetadata {
            rank: self.rank() as u32,
            length: self.len() as u32,
        };
        let metadata_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(&[metadata]),
            usage: wgpu::BufferUsages::UNIFORM
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
        });

        device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: self.data.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: shape_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: stride_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 3,
                    resource: contiguous_stride_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 4,
                    resource: metadata_buffer.as_entire_binding(),
                },
            ],
        })
    }

    pub fn broadcastable_to(&self, other: &Tensor) {
        // https://numpy.org/doc/stable/user/basics.broadcasting.html
        // When operating on two arrays, NumPy compares their shapes element-wise.
        // It starts with the trailing (i.e. rightmost) dimension and works its way left.
        // Two dimensions are compatible when
        //   1. they are equal, or
        //   2. one of them is 1.
        let a_shape = self.shape().iter().rev();
        let b_shape = other.shape().iter().rev();

        // Polyfill with 1-dimensions to get the same ranks
        let (a_shape, b_shape): (Vec<usize>, Vec<usize>) = a_shape
            .zip_longest(b_shape)
            .map(|element| match element {
                Both(&l, &r) => {
                    if l == r || l == 1 || r == 1 {
                        (l, r)
                    } else {
                        panic!(
                            "Unable to broadcast tensor of shape {:?} and {:?}",
                            self.shape(),
                            other.shape()
                        );
                    }
                }
                Left(&l) => (l, 1),
                Right(&r) => (1, r),
            })
            .rev()
            .unzip();
    }

    pub async fn reshape(&self, shape: &[usize]) -> Tensor {
        dtensor::primitives::ops::reshape(self, shape).await
    }

    pub async fn as_contiguous(&self) -> Tensor {
        self.reshape(self.shape()).await
    }

    // Private Helpers
    pub fn with_strided_buffer(
        shape: &[usize],
        stride: &[usize],
        buffer: wgpu::Buffer,
        wgpu_device: &dtensor::WgpuDevice,
    ) -> Tensor {
        let rank = shape.len();
        let n_elements: usize = shape.iter().product();

        // Stride is the offset in terms of the `data` contiguous vector
        // Calculated by a running sum right-to-left
        let contiguous_stride = compute_contiguous_stride(&shape);

        Tensor {
            rank: rank,
            shape: shape.iter().map(|x| x.clone()).collect::<Vec<usize>>(),
            stride: stride.to_vec(),
            contiguous_stride: contiguous_stride,
            contiguous: true,
            wgpu_device: wgpu_device.clone(),
            n: n_elements,
            data: buffer,
        }
    }
}

impl Clone for Tensor {
    fn clone(&self) -> Self {
        let new_buffer = self.buffer_with(self.buffer().usage());
        Tensor::with_strided_buffer(self.shape(), self.stride(), new_buffer, self.wgpu_device())
    }
}

// Elementary Arithmetic Implementations
impl_op_ex!(+ |a: &Tensor, b: &Tensor| -> Tensor {
    futures::executor::block_on(dtensor::primitives::ops::add(a, b))
});

impl_op_ex_commutative!(+ |a: &Tensor, b: f32| -> Tensor {
    let literal = futures::executor::block_on(Tensor::literal(b, a.wgpu_device()));
    a + literal
});

impl_op_ex!(- |a: &Tensor, b: &Tensor| -> Tensor {
    futures::executor::block_on(dtensor::primitives::ops::subtract(a, b))
});

impl_op_ex!(- |a: &Tensor, b: f32| -> Tensor {
    let literal = futures::executor::block_on(Tensor::literal(b, a.wgpu_device()));
    a - literal
});

impl_op_ex!(* |a: &Tensor, b: &Tensor| -> Tensor {
    futures::executor::block_on(dtensor::primitives::ops::multiply(a, b))
});

impl_op_ex_commutative!(* |a: &Tensor, b: f32| -> Tensor {
    let literal = futures::executor::block_on(Tensor::literal(b, a.wgpu_device()));
    a * literal
});

impl_op_ex!(/ |a: &Tensor, b: &Tensor| -> Tensor {
    futures::executor::block_on(dtensor::primitives::ops::divide(a, b))
});

impl_op_ex!(/ |a: &Tensor, b: f32| -> Tensor {
    let literal = futures::executor::block_on(Tensor::literal(b, a.wgpu_device()));
    a / literal
});

// Helpers
fn usize_to_u32(data: &[usize]) -> Vec<u32> {
    data.iter().map(|&x| x as u32).collect::<Vec<_>>()
}
