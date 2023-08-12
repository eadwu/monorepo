use itertools::{EitherOrBoth::*, Itertools};

pub type ViewType = u32;
pub struct TensorView {
    pub contiguous: bool,
    pub shape: Box<[ViewType]>,
    pub stride: Box<[ViewType]>,
    pub offset: Box<[ViewType]>,
    pub contiguous_stride: Box<[ViewType]>,
}

impl TensorView {
    pub fn new(
        contiguous: bool,
        shape: Box<[ViewType]>,
        stride: Box<[ViewType]>,
        offset: Box<[ViewType]>,
    ) -> TensorView {
        let contiguous_shape = TensorView::compute_contiguous_stride(&shape);
        TensorView {
            contiguous: contiguous,
            shape: shape,
            stride: stride,
            offset: offset,
            contiguous_stride: contiguous_shape.into_boxed_slice(),
        }
    }

    pub fn from_shape(shape: Box<[ViewType]>) -> TensorView {
        let stride = TensorView::compute_contiguous_stride(&shape);
        let offset = shape.iter().map(|_| 0).collect_vec();
        TensorView::new(
            true,
            shape,
            stride.into_boxed_slice(),
            offset.into_boxed_slice(),
        )
    }

    pub fn compute_contiguous_stride(contiguous_shape: &[ViewType]) -> Vec<ViewType> {
        // When it is stored contiguously, the stride is the product of the size
        // of the dimension before it
        contiguous_shape
            .iter()
            .rev()
            .scan(1, |state, &x| {
                let current_state = *state;
                *state = *state * x;
                Some(current_state)
            })
            .collect_vec()
            .into_iter()
            .rev()
            .collect_vec()
    }
}

impl TensorView {
    pub fn broadcast(&self, other: &TensorView) -> TensorView {
        // https://numpy.org/doc/stable/user/basics.broadcasting.html
        // When operating on two arrays, NumPy compares their shapes element-wise.
        // It starts with the trailing (i.e. rightmost) dimension and works its way left.
        // Two dimensions are compatible when
        //   1. they are equal, or
        //   2. one of them is 1.
        let my_shape_rev = self.shape.iter().map(|&x| x).rev().collect_vec();
        let other_shape_rev = other.shape.iter().map(|&x| x).rev().collect_vec();

        let (my_expanded_shape_rev, other_expanded_shape_rev): (Vec<ViewType>, Vec<ViewType>) =
            my_shape_rev
                .iter()
                .zip_longest(other_shape_rev.iter())
                .map(|element| match element {
                    Left(&l) => (l, 1),
                    Right(&r) => (1, r),
                    Both(&l, &r) => {
                        assert!(
                            l == r || l == 1 || r == 1,
                            "Unable to broadcast from `{:?}` <-> `{:?}`",
                            self.shape,
                            other.shape
                        );
                        (l, r)
                    }
                })
                .unzip();

        // Broadcasted shape is max of either dimension, assuming it is broadcastable
        let broadcasted_shape = my_expanded_shape_rev
            .iter()
            .zip(other_expanded_shape_rev.iter())
            .map(|(&my_dimension, &other_dimension)| std::cmp::max(my_dimension, other_dimension))
            .rev()
            .collect_vec();

        // https://github.com/numpy/numpy/blob/main/doc/source/reference/arrays.ndarray.rst
        // If self.shape[k] == 1 then for any legal index index[k] == 0.
        //   This means that in the formula for the offset n_k = 0 and thus
        //   s_k n_k = 0 and the value of s_k = self.strides[k] is arbitrary.
        // If an array has no elements (self.size == 0) there is no legal index
        //   and the strides are never used. Any array with no elements may be
        //   considered C-style and Fortran-style contiguous.
        let my_stride_rev = self.stride.iter().map(|&x| x).rev().collect_vec();
        let adjusted_stride = my_expanded_shape_rev
            .iter()
            .zip_longest(my_stride_rev.iter())
            .map(|element| match element {
                Left(&dimension) => {
                    assert!(
                        dimension == 1,
                        "Extra broadcasted dimension should be only of length 1, got {}",
                        dimension
                    );
                    0
                }
                Right(_) => panic!(
                    "Original stride dimensions ({}) should not exceed broadcasted dimensions ({})",
                    my_expanded_shape_rev.len(),
                    self.stride.len()
                ),
                Both(&dimension, &stride) => {
                    if dimension == 1 {
                        0
                    } else {
                        stride
                    }
                }
            })
            .rev()
            .collect_vec();

        let offset = broadcasted_shape.iter().map(|_| 0).collect_vec();
        TensorView::new(
            false,
            broadcasted_shape.into_boxed_slice(),
            adjusted_stride.into_boxed_slice(),
            offset.into_boxed_slice(),
        )
    }
}
