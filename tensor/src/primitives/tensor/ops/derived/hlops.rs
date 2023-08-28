use crate::primitives::tensor::{Tensor, TensorView, ViewType};

impl Tensor {
    pub fn Clip(&self, min: &Tensor, max: &Tensor) -> Tensor {
        self.Minimum(min).Maximum(max)
    }

    pub fn MatMul(&self, other: &Tensor) -> Tensor {
        let input = match self.view().dimension() {
            0 => self.broadcast_to(&TensorView::from_shape(&[1, 1])),
            1 => self.unsqueeze(1),
            _ => self.clone(),
        };

        let other = match other.view().dimension() {
            0 => other.broadcast_to(&TensorView::from_shape(&[1, 1])),
            1 => other.unsqueeze(0),
            _ => other.clone(),
        };

        let input_dimension = input.view().dimension() as usize;
        let other_dimension = other.view().dimension() as usize;
        let n = input.view().shape[input_dimension - 2];
        let k = input.view().shape[input_dimension - 1];
        let m = other.view().shape[other_dimension - 1];

        assert!(
            other.view().shape[(other.view().dimension() - 2) as usize] == k,
            "Failed to multiply matrices of shape {:?} @ {:?}",
            self.view(),
            other.view()
        );

        // (..., n, k, 1)
        let input = input.unsqueeze(input_dimension as ViewType);
        // (..., 1, k, m) - index `-3` which in this case will be axis `other_dimension + 1 - 3`
        let other = other.unsqueeze((other_dimension - 2) as ViewType);
        // (..., n, k, m)
        let intermediate_result = input.Multiply(&other);
        let reduce_dimension = intermediate_result.view().dimension() - 2;
        // (..., n, m)
        intermediate_result.Sum(&[reduce_dimension], false)
    }
}
