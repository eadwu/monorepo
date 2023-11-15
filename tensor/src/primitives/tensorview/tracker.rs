use std::ops::Deref;

use super::{TensorView, ViewType};

#[derive(Clone, Debug)]
pub struct TensorViewTracker {
    current: TensorView,
    history: Vec<TensorView>,
    public: TensorView,
}

impl Deref for TensorViewTracker {
    type Target = TensorView;

    fn deref(&self) -> &Self::Target {
        &self.public
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
            public: TensorView::from_contiguous_shape(&view.shape[..]),
        }
    }

    pub fn root_view(&self) -> &TensorView {
        self.history.first().unwrap_or(&self.current)
    }

    pub fn swap_view(&self, view: &TensorView) -> TensorViewTracker {
        TensorViewTracker::new(view, &self.history[..])
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

    pub fn serialized_history_fifo(&self) -> Vec<TensorView> {
        self.seralized_history_lilo()
            .into_iter()
            .rev()
            .collect::<Vec<_>>()
    }

    pub fn max_ndim(&self) -> ViewType {
        self.seralized_history_lilo()
            .iter()
            .map(TensorView::ndim)
            .max()
            .unwrap_or(0)
    }

    pub fn align_to_ndim(&self, ndim: ViewType) -> TensorViewTracker {
        let aligned_views = self
            .seralized_history_lilo()
            .into_iter()
            .map(|view| view.at_least_ndim(ndim))
            .collect::<Vec<_>>();
        let last_view_index = aligned_views.len() - 1;

        TensorViewTracker::new(
            &aligned_views[last_view_index],
            &aligned_views[..last_view_index],
        )
    }
}
