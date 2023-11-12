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

    pub fn root_view(&self) -> &TensorView {
        self.history.first().unwrap_or(&self.current)
    }

    pub fn track_view(&self, view: &TensorView) -> TensorViewTracker {
        TensorViewTracker::new(view, &self.seralized_history_lilo()[..])
    }

    pub fn seralized_history_lilo(&self) -> Vec<TensorView> {
        self.history
            .iter()
            .chain(std::iter::once(&self.current))
            .map(|view| view.clone())
            .collect::<Vec<_>>()
    }
}
