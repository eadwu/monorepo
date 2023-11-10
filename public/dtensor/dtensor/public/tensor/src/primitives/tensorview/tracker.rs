use std::ops::Deref;

use super::TensorView;

#[derive(Clone, Debug)]
pub struct TensorViewTracker {
    current: TensorView,
    history: Vec<TensorView>,
}

impl Deref for TensorViewTracker {
    type Target = TensorView;

    fn deref(&self) -> &Self::Target {
        &self.current
    }
}

impl From<TensorView> for TensorViewTracker {
    fn from(value: TensorView) -> Self {
        TensorViewTracker::new(&value, &[])
    }
}

impl TensorViewTracker {
    pub fn new(view: &TensorView, history: &[TensorView]) -> TensorViewTracker {
        TensorViewTracker {
            current: view.clone(),
            history: history.to_vec(),
        }
    }
}
