use std::slice::Iter;

use itertools::{EitherOrBoth::*, Itertools};

pub type ViewType = i32;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TensorView {
    pub contiguous: bool,
    pub shape: Box<[ViewType]>,
    pub stride: Box<[ViewType]>,
    pub contiguous_stride: Box<[ViewType]>,
}

impl TensorView {
    pub fn as_defined(
        contiguous: bool,
        shape: Box<[ViewType]>,
        stride: Box<[ViewType]>,
        contiguous_stride: Box<[ViewType]>,
    ) -> TensorView {
        TensorView {
            contiguous,
            shape,
            stride,
            contiguous_stride,
        }
    }

    pub fn new(contiguous: bool, shape: Box<[ViewType]>, stride: Box<[ViewType]>) -> TensorView {
        let contiguous_stride = TensorView::compute_contiguous_stride(&shape);
        TensorView::as_defined(
            contiguous,
            shape,
            stride,
            contiguous_stride.into_boxed_slice(),
        )
    }

    pub fn from_contiguous_shape(shape: &[ViewType]) -> TensorView {
        let stride = TensorView::compute_contiguous_stride(shape);
        TensorView::new(
            true,
            shape.to_vec().into_boxed_slice(),
            stride.into_boxed_slice(),
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
    pub fn len(&self) -> ViewType {
        self.shape.iter().product()
    }

    pub fn ndim(&self) -> ViewType {
        self.shape.len() as ViewType
    }

    pub fn pad(&self, padding: &[(ViewType, ViewType)]) -> TensorView {
        assert!(
            (self.ndim() as usize) == padding.len(),
            "Padding must be specified for all dimensions"
        );

        let expanded_shape = self
            .shape
            .iter()
            .zip(padding.iter())
            .map(|(rank, (padding_pre, padding_post))| padding_pre + rank + padding_post)
            .collect_vec();

        TensorView::from_contiguous_shape(&expanded_shape)
    }

    fn _split_and_join<T>(
        slice: &[T],
        index: usize,
        default: T,
        merge_function: impl Fn(Iter<'_, T>, Iter<'_, T>, T) -> Vec<T>,
    ) -> Vec<T> {
        let (left_exclusive, right_inclusive) = slice.split_at(index);
        merge_function(left_exclusive.iter(), right_inclusive.iter(), default)
    }

    fn _join_squeeze<T: Copy>(
        left_exclusive: Iter<'_, T>,
        right_inclusive: Iter<'_, T>,
        _: T,
    ) -> Vec<T> {
        left_exclusive
            .chain(right_inclusive.skip(1))
            .map(|&x| x)
            .collect_vec()
    }

    fn _join_unsqueeze<T: Copy>(
        left_exclusive: Iter<'_, T>,
        right_inclusive: Iter<'_, T>,
        default: T,
    ) -> Vec<T> {
        left_exclusive
            .chain(std::iter::once(&default))
            .chain(right_inclusive)
            .map(|&x| x)
            .collect_vec()
    }

    pub fn squeeze(&self, axis: ViewType) -> TensorView {
        let axis_rank = self.shape[axis as usize];

        assert!(
            axis < self.ndim(),
            "Squeeze axis {} is out of bounds, 0 <= axis < {}",
            axis,
            self.ndim()
        );

        assert!(
            axis_rank == 1,
            "Squeeze axis {} cannot be removed as axis rank {} != 1",
            axis,
            axis_rank
        );

        let axis = axis as usize;
        let shape = TensorView::_split_and_join(&self.shape, axis, 1, TensorView::_join_squeeze);
        let stride = TensorView::_split_and_join(&self.stride, axis, 0, TensorView::_join_squeeze);

        TensorView::new(
            self.contiguous,
            shape.into_boxed_slice(),
            stride.into_boxed_slice(),
        )
    }

    pub fn transpose(&self, axes: &[ViewType]) -> TensorView {
        let axis = if axes.len() == self.ndim() as usize {
            axes.iter().map(|&x| x as usize).collect::<Vec<_>>()
        } else {
            self.shape
                .iter()
                .enumerate()
                .rev()
                .map(|(idx, _)| idx)
                .collect::<Vec<_>>()
        };

        // Ensure inputs are within bounds
        axis.iter()
            .for_each(|&axis| assert!(axis < self.ndim() as usize));

        let shape = axis
            .iter()
            .map(|&axis| self.shape[axis])
            .collect::<Vec<_>>();
        let stride = axis
            .iter()
            .map(|&axis| self.stride[axis])
            .collect::<Vec<_>>();
        TensorView::new(false, shape.into_boxed_slice(), stride.into_boxed_slice())
    }

    pub fn unsqueeze(&self, axis: ViewType) -> TensorView {
        assert!(
            axis <= self.ndim(),
            "Unsqueeze axis {} is out of bounds, 0 <= axis <= {}",
            axis,
            self.ndim()
        );

        let axis = axis as usize;
        let shape = TensorView::_split_and_join(&self.shape, axis, 1, TensorView::_join_unsqueeze);
        let stride =
            TensorView::_split_and_join(&self.stride, axis, 0, TensorView::_join_unsqueeze);

        TensorView::new(
            self.contiguous,
            shape.into_boxed_slice(),
            stride.into_boxed_slice(),
        )
    }

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

        let shape = broadcasted_shape.into_boxed_slice();
        let stride = adjusted_stride.into_boxed_slice();

        if shape == self.shape {
            self.clone()
        } else {
            TensorView::new(false, shape, stride)
        }
    }

    pub fn at_least_ndim(&self, ndim: ViewType) -> TensorView {
        // If self.dim() > ndim, then it is capped to ndim
        // Prevent negative numbers, which don't exist for unsigned integers
        let missing_axis = ndim - ndim.min(self.ndim());
        (0..missing_axis).fold(self.clone(), |acc, _| acc.unsqueeze(acc.ndim()))
    }
}
