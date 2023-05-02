use itertools::{EitherOrBoth::*, Itertools};
use std::{
    borrow::Borrow,
    ops::{Add, Div, Mul, Sub},
};

use crate::dtensor;
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

    pub async fn new<T: bytemuck::Pod>(
        shape: &[usize],
        data: &[T],
        wgpu_device: &dtensor::WgpuDevice,
    ) -> Tensor {
        let dtensor::WebGPU { device, queue } = wgpu_device.borrow();
        let data_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(data),
            usage: wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
        });

        Tensor::with_buffer(shape, data_buffer, wgpu_device)
    }

    pub async fn of_shape(shape: &[usize], wgpu_device: &dtensor::WgpuDevice) -> Tensor {
        let dtensor::WebGPU { device, queue } = wgpu_device.borrow();

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

        Tensor::with_buffer(shape, data_buffer, wgpu_device)
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
    fn with_buffer(
        shape: &[usize],
        buffer: wgpu::Buffer,
        wgpu_device: &dtensor::WgpuDevice,
    ) -> Tensor {
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
            wgpu_device: wgpu_device.clone(),
            n: n_elements,
            data: buffer,
        }
    }
}

// Elementary Arithmetic Implementations

impl Add for Tensor {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        futures::executor::block_on(dtensor::primitives::ops::add(&self, &other))
    }
}

impl Sub for Tensor {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        futures::executor::block_on(dtensor::primitives::ops::subtract(&self, &other))
    }
}

impl Mul for Tensor {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        futures::executor::block_on(dtensor::primitives::ops::multiply(&self, &other))
    }
}

impl Div for Tensor {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        futures::executor::block_on(dtensor::primitives::ops::divide(&self, &other))
    }
}
