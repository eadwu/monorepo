use std::slice::Iter;

use itertools::{EitherOrBoth::*, Itertools};
use num_traits::AsPrimitive;

mod passthrough;
pub use passthrough::*;

pub type ViewType = u32;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TensorView {
    pub contiguous: bool,
    pub shape: Box<[ViewType]>,
    pub _container_shape: Box<[ViewType]>,
    pub stride: Box<[ViewType]>,
    pub contiguous_stride: Box<[ViewType]>,
    pub offset: Box<[ViewType]>,
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
            shape: shape.clone(),
            _container_shape: shape,
            stride: stride,
            contiguous_stride: contiguous_shape.into_boxed_slice(),
            offset: offset,
        }
    }

    pub fn from_shape(shape: &[ViewType]) -> TensorView {
        let stride = TensorView::compute_contiguous_stride(shape);
        let offset = shape.iter().map(|_| 0).collect::<Vec<_>>();
        TensorView::new(
            true,
            shape.to_vec().into_boxed_slice(),
            stride.into_boxed_slice(),
            offset.into_boxed_slice(),
        )
    }

    pub fn from_container(
        view: &TensorView,
        shape: Box<[ViewType]>,
        offset: Box<[ViewType]>,
    ) -> TensorView {
        TensorView {
            contiguous: false,
            shape: shape,
            _container_shape: view._container_shape.clone(),
            stride: view.stride.clone(),
            contiguous_stride: view.contiguous_stride.clone(),
            offset: offset,
        }
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
        self._container_shape.iter().product()
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

        TensorView::from_shape(&expanded_shape)
        // TensorView::new(
        //     true,
        //     expanded_shape.into_boxed_slice(),
        //     self.stride.clone(),
        // )
    }

    pub fn offset(&self, offsets: &[(ViewType, ViewType)]) -> TensorView {
        assert!(
            (self.ndim() as usize) == offsets.len(),
            "Offsets must be specified for all dimensions"
        );

        let shape = self
            .shape
            .iter()
            .zip(offsets.iter())
            .map(|(shape, (pre, post))| shape - pre - post)
            .collect_vec();
        let offset = self
            .offset
            .iter()
            .zip(offsets.iter())
            .map(|(previous_offset, (pre, _))| previous_offset + pre)
            .collect_vec();

        TensorView::from_container(self, shape.into_boxed_slice(), offset.into_boxed_slice())
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

    fn _join_squeeze<T: AsPrimitive<usize>>(
        left_exclusive: Iter<'_, T>,
        right_inclusive: Iter<'_, T>,
        default: T,
    ) -> Vec<T> {
        left_exclusive
            .chain(right_inclusive.skip(default.as_()))
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
        let stride = TensorView::_split_and_join(&self.stride, axis, 1, TensorView::_join_squeeze);
        let offset = TensorView::_split_and_join(&self.offset, axis, 0, TensorView::_join_squeeze);

        TensorView::new(
            self.contiguous,
            shape.into_boxed_slice(),
            stride.into_boxed_slice(),
            offset.into_boxed_slice(),
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
        let offset = axis
            .iter()
            .map(|&axis| self.offset[axis])
            .collect::<Vec<_>>();
        TensorView::new(
            false,
            shape.into_boxed_slice(),
            stride.into_boxed_slice(),
            offset.into_boxed_slice(),
        )
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
        let offset =
            TensorView::_split_and_join(&self.offset, axis, 0, TensorView::_join_unsqueeze);

        TensorView::new(
            self.contiguous,
            shape.into_boxed_slice(),
            stride.into_boxed_slice(),
            offset.into_boxed_slice(),
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

        let my_offset_rev = self.offset.iter().map(|&x| x).rev().collect_vec();
        let adjusted_offset = my_offset_rev
            .iter()
            .zip_longest(my_expanded_shape_rev)
            .map(|element| match element {
                Left(&offset) => offset,
                Right(_) => 0,
                Both(&offset, _) => offset,
            })
            .rev()
            .collect_vec();

        let shape = broadcasted_shape.into_boxed_slice();
        let stride = adjusted_stride.into_boxed_slice();
        let offset = adjusted_offset.into_boxed_slice();

        if shape == self.shape {
            self.clone()
        } else {
            TensorView::new(false, shape, stride, offset)
        }
    }
}
