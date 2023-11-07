use std::mem::replace;
use std::path::Path;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use num_traits::AsPrimitive;
use rand::Rng;
use uuid::Uuid;

use crate::FILE_MANAGER;

use super::*;

static ID_GENERATOR: AtomicU32 = AtomicU32::new(0);

pub type TensorType = f32;

#[derive(Clone, Debug)]
pub struct Tensor(Arc<TensorInternals>);

#[derive(Clone, Debug)]
pub struct TensorInternals {
    id: u32,
    view: TensorView,
    data: TensorInput,
}

impl TensorInternals {
    pub fn new(view: TensorView, data: TensorInput) -> TensorInternals {
        TensorInternals {
            id: ID_GENERATOR.fetch_add(1, Ordering::Relaxed),
            view: view,
            data: data,
        }
    }
}

impl Tensor {
    pub fn new(view: TensorView, data: TensorInput) -> Tensor {
        Tensor(Arc::new(TensorInternals::new(view, data)))
    }

    pub fn scalar<T: AsPrimitive<TensorType>>(data: T) -> Tensor {
        Tensor::from_contiguous(&[data.as_()], &[])
    }

    pub fn from_contiguous(data: &[TensorType], shape: &[ViewType]) -> Tensor {
        Tensor::with_shape(data, TensorView::from_shape(shape))
    }

    pub fn arange(shape: &[ViewType]) -> Tensor {
        let view = TensorView::from_shape(shape);
        let n = view.len() as usize;
        let data = (0..n).map(|x| x as TensorType).collect::<Vec<_>>();
        Tensor::with_shape(&data, view)
    }

    pub fn randn(shape: &[ViewType], mean: Option<f32>, std_dev: Option<f32>) -> Tensor {
        let view = TensorView::from_shape(shape);
        let n = view.len() as usize;

        let mean = mean.unwrap_or(0.0);
        let std_dev = std_dev.unwrap_or(1.0);
        let distribution = rand_distr::Normal::new(mean, std_dev).unwrap();
        let data = rand::thread_rng()
            .sample_iter(distribution)
            .take(n)
            .collect::<Vec<_>>();
        Tensor::with_shape(&data[..], view)
    }

    pub fn zeros_like(shape: &[ViewType]) -> Tensor {
        let view = TensorView::from_shape(shape);
        Tensor::scalar(0).broadcast_to(&view)
    }

    pub fn with_shape(data: &[TensorType], view: TensorView) -> Tensor {
        let identifier = Uuid::new_v4().to_string();
        let path = Path::new(&identifier);
        FILE_MANAGER
            .lock()
            .unwrap()
            .create_with_bytes(path, bytemuck::cast_slice(data))
            .unwrap();

        Tensor::new(
            view,
            TensorInput::from_raw(path, std::mem::size_of_val(data), 0),
        )
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

    pub fn data(&self) -> &TensorInput {
        &self.0.data
    }

    pub fn load(&self) -> Vec<u8> {
        assert!(self.has_data(), "Tensor does not contain any data");

        if let TensorInput::ExplicitInput(input) = self.data() {
            if let InputSpec::Raw(spec) = input {
                return <&Tensor as RawFileLoader>::load(self, spec);
            } else if let InputSpec::Safetensor(spec) = input {
                return <&Tensor as SafetensorLoader>::load(self, spec);
            }
        }

        unreachable!("Reached unreachable path for Tensor.load()");
    }
}

impl Drop for Tensor {
    fn drop(&mut self) {
        fn consume_to(value: &mut Tensor, dest: &mut Vec<Tensor>) {
            if let Some(tensor) = Arc::get_mut(&mut value.0) {
                if let TensorInput::ExplicitInput(ref mut input) = tensor.data {
                    if let InputSpec::Raw(ref mut spec) = input {
                        FILE_MANAGER.lock().unwrap().close(&spec.file);
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
