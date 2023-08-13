use std::mem::replace;
use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::{AtomicU32, Ordering};

use num_traits::AsPrimitive;
use uuid::Uuid;

use crate::FILE_MANAGER;

use super::*;

static ID_GENERATOR: AtomicU32 = AtomicU32::new(0);

pub type TensorType = f32;

#[derive(Clone, Debug)]
pub struct Tensor(Rc<TensorInternals>);

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
        Tensor(Rc::new(TensorInternals::new(view, data)))
    }

    pub fn scalar<T: AsPrimitive<TensorType>>(data: T) -> Tensor {
        let data = vec![data.as_()];
        let shape = vec![1];
        Tensor::from_contiguous(&data, &shape)
    }

    pub fn from_contiguous(data: &[TensorType], shape: &[ViewType]) -> Tensor {
        let identifier = Uuid::new_v4().to_string();
        let path = Path::new(&identifier);
        FILE_MANAGER
            .lock()
            .unwrap()
            .create_with_bytes(path, bytemuck::cast_slice(data))
            .unwrap();

        Tensor::new(
            TensorView::from_shape(shape),
            TensorInput::from_raw(path, std::mem::size_of_val(data), 0),
        )
    }
}

impl Tensor {
    fn has_data(&self) -> bool {
        match self.data() {
            TensorInput::ExplicitInput(_) => true,
            _ => false,
        }
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
                let data = FILE_MANAGER
                    .lock()
                    .unwrap()
                    .open(&spec.file, spec.offset)
                    .unwrap();
                let bytes = &data[..spec.size];
                return bytes.to_vec();
            }
        }

        unreachable!("Reached unreachable path for Tensor.load()");
    }
}

impl Drop for Tensor {
    fn drop(&mut self) {
        fn consume_to(value: &mut Tensor, dest: &mut Vec<Tensor>) {
            if let Some(tensor) = Rc::get_mut(&mut value.0) {
                if let TensorInput::OperationResult(ref mut result) = tensor.data {
                    if let OperationSpec::UnaryOp(ref mut op) = result {
                        dest.push(op.input.clone());
                    }

                    if let OperationSpec::BinaryOp(ref mut op) = result {
                        dest.push(op.lhs.clone());
                        dest.push(op.rhs.clone());
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
