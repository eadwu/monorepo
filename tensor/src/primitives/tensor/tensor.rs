use std::borrow::BorrowMut;
use std::mem::replace;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use rand::Rng;

use crate::primitives::tensorview::{TensorView, TensorViewTracker, ViewType};
use crate::FILE_MANAGER;

use super::*;

static ID_GENERATOR: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Debug)]
pub struct Tensor(Arc<TensorInternals>);

#[derive(Clone, Debug)]
pub struct TensorInternals {
    id: u32,
    view: TensorViewTracker,
    data: TensorInput,
    datatype: TensorType,
}

impl TensorInternals {
    pub fn new(
        view: TensorViewTracker,
        data: TensorInput,
        datatype: TensorType,
    ) -> TensorInternals {
        TensorInternals {
            id: ID_GENERATOR.fetch_add(1, Ordering::Relaxed),
            view: view,
            data: data,
            datatype: datatype,
        }
    }
}

impl Tensor {
    pub fn new<T: Into<TensorViewTracker>>(
        view: T,
        data: TensorInput,
        datatype: TensorType,
    ) -> Tensor {
        Tensor(Arc::new(TensorInternals::new(view.into(), data, datatype)))
    }

    pub fn scalar<T: TensorDataElement>(data: T) -> Tensor {
        let view = TensorView::from_contiguous_shape(&[]);
        Tensor::new(view, TensorInput::from_scalar(data), data.into())
    }

    pub fn from_contiguous<T: TensorDataElement>(data: &[T], shape: &[ViewType]) -> Tensor {
        Tensor::with_shape(data, TensorView::from_contiguous_shape(shape))
    }

    pub fn arange(shape: &[ViewType]) -> Tensor {
        let view = TensorView::from_contiguous_shape(shape);
        let n = view.len();
        let data = (0..n).collect::<Vec<u32>>();
        Tensor::with_shape(&data[..], view)
    }

    pub fn randn(shape: &[ViewType], mean: Option<f32>, std_dev: Option<f32>) -> Tensor {
        let view = TensorView::from_contiguous_shape(shape);
        let n = view.len() as usize;

        let mean = mean.unwrap_or(0.0);
        let std_dev = std_dev.unwrap_or(1.0);
        let distribution = rand_distr::Normal::new(mean, std_dev).unwrap();
        let data = rand::thread_rng()
            .sample_iter(distribution)
            .take(n)
            .collect::<Vec<f32>>();
        Tensor::with_shape(&data[..], view)
    }

    pub fn zeros_like(shape: &[ViewType]) -> Tensor {
        let view = TensorView::from_contiguous_shape(shape);
        Tensor::scalar(0).broadcast_to(&view)
    }

    pub fn with_shape<T: TensorDataElement>(data: &[T], view: TensorView) -> Tensor {
        let inferred_datatype = data
            .first()
            .map(|&x| x)
            .map(Into::<TensorType>::into)
            .unwrap();
        Tensor::from_raw_bytes(bytemuck::cast_slice(data), view, inferred_datatype)
    }

    pub fn from_raw_bytes(data: &[u8], view: TensorView, datatype: TensorType) -> Tensor {
        let path = FILE_MANAGER
            .lock()
            .unwrap()
            .create_with_bytes(data)
            .unwrap();
        Tensor::new(view, TensorInput::from_internal(&path), datatype)
    }
}

impl Tensor {
    pub fn has_data(&self) -> bool {
        match self.data() {
            TensorInput::ExplicitInput(_) => true,
            _ => false,
        }
    }

    pub fn id(&self) -> u32 {
        self.0.id
    }

    pub fn view(&self) -> &TensorView {
        &self.0.view
    }

    pub fn viewtracker(&self) -> &TensorViewTracker {
        &self.0.view
    }

    pub fn data(&self) -> &TensorInput {
        &self.0.data
    }

    pub fn datatype(&self) -> TensorType {
        self.0.datatype
    }

    pub fn load(&self) -> Vec<u8> {
        assert!(self.has_data(), "Tensor does not contain any data");

        if let TensorInput::ExplicitInput(input) = self.data() {
            return match input {
                InputSpec::Scalar(stream) => stream.clone(),
                InputSpec::Internal(spec) => <&Tensor as InternalLoader>::load(self, spec),
                InputSpec::Safetensor(spec) => <&Tensor as SafetensorLoader>::load(self, spec),
            };
        }

        unreachable!("Reached unreachable path for Tensor.load()");
    }
}

impl Drop for Tensor {
    fn drop(&mut self) {
        fn consume_to(value: &mut Tensor, dest: &mut Vec<Tensor>) {
            if let Some(tensor) = Arc::get_mut(&mut value.0) {
                if let TensorInput::ExplicitInput(ref mut input) = tensor.data {
                    if let InputSpec::Internal(ref mut spec) = input {
                        if let Some(path) = Arc::get_mut(spec.path.borrow_mut()) {
                            FILE_MANAGER.lock().unwrap().close(path);
                        }
                    }
                }

                if let TensorInput::NoOp(ref mut input) = tensor.data {
                    dest.push(input.clone());
                }

                if let TensorInput::OperationResult(ref mut result) = tensor.data {
                    if let OperationSpec::UnaryOp(ref mut op) = result {
                        dest.push(op.input.clone());
                    }

                    if let OperationSpec::BinaryOp(ref mut op) = result {
                        dest.push(op.lhs.clone());
                        dest.push(op.rhs.clone());
                    }

                    if let OperationSpec::ReduceOp(ref mut op) = result {
                        dest.push(op.input.clone());
                    }

                    if let OperationSpec::ViewOp(ref mut op) = result {
                        dest.push(op.input.clone());
                    }
                }

                let _ = replace(&mut tensor.data, TensorInput::Invalidated);
            }
        }

        let mut stack = vec![];
        consume_to(self, &mut stack);
        while let Some(mut value) = stack.pop() {
            consume_to(&mut value, &mut stack);
        }
    }
}
