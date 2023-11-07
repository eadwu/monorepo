use crate::primitives::tensor::{Tensor, TensorView, ViewType};

#[derive(Clone, Copy, Debug)]
pub enum ConvPadding<'a> {
    Same,
    SameUpper,
    SameLower,
    Valid,
    Custom(&'a [(ViewType, ViewType)]),
}

#[derive(Clone, Copy, Debug)]
pub enum ScatterReduction {
    None,
    Add,
    Mul,
    Max,
    Min,
}

impl Tensor {
    pub fn ArgMax(&self, axis: ViewType, keep_dims: bool, select_last_index: bool) -> Tensor {
        let max_along_axis = self.Max(&[axis], true);
        let mask = self.Equal(&max_along_axis);

        let (_, shape_per_batch) = self.shape().split_at(axis as usize);
        let axis_size = self.shape()[axis as usize];
        let elements_per_batch = shape_per_batch[1..]
            .iter()
            .fold(1, |acc, &shape| acc * shape);
        let indices = Tensor::arange(&self.shape())
            .Divide(&Tensor::scalar(elements_per_batch))
            .Mod(&Tensor::scalar(axis_size))
            .Floor();

        if select_last_index {
            mask.Multiply(&indices).Max(&[axis], keep_dims)
        } else {
            let batch_normalizer = Tensor::scalar(axis_size - 1);
            let adjusted_indices = batch_normalizer.Sub(&indices);
            let masked_normalization = mask.Multiply(&batch_normalizer);
            masked_normalization
                .Sub(&mask.Multiply(&adjusted_indices))
                .Max(&[axis], keep_dims)
        }
    }

    pub fn ArgMin(&self, axis: ViewType, keep_dims: bool, select_last_index: bool) -> Tensor {
        self.Neg().ArgMax(axis, keep_dims, select_last_index)
    }

    pub fn AvgPool<'a>(
        &self,
        kernel_size: &[ViewType],
        kernel_strides: &[ViewType],
        padding: ConvPadding<'a>,
    ) -> Tensor {
        let kernel = Tensor::scalar(1).reshape(&TensorView::from_shape(kernel_size));
        let n = Tensor::scalar(kernel.len());
        self.Conv(&kernel, kernel_strides, padding).Divide(&n)
    }

    pub fn Clip(&self, min: &Tensor, max: &Tensor) -> Tensor {
        self.Minimum(min).Maximum(max)
    }

    pub fn Convolve<'a>(
        &self,
        kernel: &Tensor,
        kernel_strides: &[ViewType],
        padding: ConvPadding<'a>,
        reduction: impl Fn(&Tensor, &[ViewType], bool) -> Tensor,
    ) -> Tensor {
        let input = self.contiguous();
        let kernel = kernel.contiguous();

        let [input_batch_size, c_in, features @ ..] = &input.shape() else {
            panic!("Conv expects an input signature of N x C x ...")
        };
        let [_, in_channels, filters @ ..] = &kernel.shape() else {
            panic!("Conv expects an kernel signature of C_out x C_in x ...")
        };

        assert!(
            features.len() == filters.len(),
            "Dimension of features ({}) is expected to equate dimensions of kernel ({})",
            features.len(),
            filters.len()
        );

        assert!(
            kernel_strides.len() == features.len(),
            "A stride should be defined ({} defined) for all dimensions of the kernel ({})",
            kernel_strides.len(),
            features.len(),
        );

        assert!(
            c_in == in_channels,
            "Input channels in ({}) should equate kernel input channels ({})",
            c_in,
            in_channels
        );

        // Compute necessary padding for convolution
        let axis_padding = match padding {
            ConvPadding::Valid => self.shape().iter().map(|_| (0, 0)).collect::<Vec<_>>(),
            ConvPadding::Same | ConvPadding::SameLower | ConvPadding::SameUpper => {
                kernel_strides.iter().for_each(|&stride| {
                    assert!(stride == 1, "Same padding expects strides of only 1")
                });

                let unpadded_dimensions = std::iter::once(input_batch_size)
                    .chain(std::iter::once(c_in))
                    .map(|_| (0, 0));

                // P = ((S-1)*W-S+F)/2, with F = filter size, S = stride, W = input size
                // https://stats.stackexchange.com/questions/297678/how-to-calculate-optimal-zero-padding-for-convolutional-neural-networks
                let padded_dimensions = features
                    .iter()
                    .zip(kernel_strides.iter().zip(filters.iter()))
                    .map(|(&feature, (&stride, &filter))| {
                        let padding_needed = (stride - 1) * feature - stride + filter;
                        let padding_extra = (padding_needed / 2 + 1).min(padding_needed);
                        let padding_leftover = padding_needed - padding_extra;

                        match padding {
                            ConvPadding::SameLower => (padding_extra, padding_leftover),
                            ConvPadding::Same | ConvPadding::SameUpper => {
                                (padding_leftover, padding_extra)
                            }
                            _ => unreachable!(),
                        }
                    });

                unpadded_dimensions
                    .chain(padded_dimensions)
                    .collect::<Vec<_>>()
            }
            ConvPadding::Custom(explicit_padding) => explicit_padding.to_vec(),
        };

        // Perform convolution after padding is done
        let input = input.pad(&axis_padding);

        let [input_batch_size, c_in, features @ ..] = &input.shape() else {
            panic!("Conv expects an input signature of N x C x ...")
        };
        let [batch_stride, c_in_stride, features_stride @ ..] = &input.view().stride[..] else {
            panic!("Conv expects an input signature of N x C x ...")
        };

        // X_out = (X_in + 2*padding - dilation*(kernel_size-1) - 1 / stride).floor()
        // h_in is padded here now
        let output_dimensions = features
            .iter()
            .zip(filters.iter().zip(kernel_strides.iter()))
            .map(|(input, (filter, stride))| (input - 1 * (filter - 1) - 1) / stride + 1)
            .collect::<Vec<_>>();

        // (n, c_out, c_in, output_size..., kernel_size...)
        // n batch size of
        // c_out (1 broadcasted) output channels which are the
        // sum of c_in input channels which are of size
        // (output_size...) computed using
        // (kernel_size) convolved kernels
        //
        // (n, 1, c_in, h_out, w_out, k_h, k_w)
        // Simplier explanation of (h_out, w_out, k_h, k_w), since it is known
        // the output size is (h_out, w_out), use some stride_tricks to get the
        // (k_h, k_w) window for each element that is convolved with the kernel
        let input_shape_trick = [*input_batch_size, 1, *c_in]
            .iter()
            .chain(output_dimensions.iter())
            .chain(filters.iter())
            .map(|&x| x)
            .collect::<Vec<_>>();
        let input_stride_trick = [*batch_stride, 0, *c_in_stride]
            .into_iter()
            .chain(
                features_stride
                    .iter()
                    .zip(kernel_strides.iter())
                    .map(|(&feature_stride, &filter_stride)| feature_stride * filter_stride),
            )
            .chain(features_stride.iter().map(|&x| x))
            .collect::<Vec<_>>();
        let input_offset_trick = input_shape_trick.iter().map(|_| 0).collect::<Vec<_>>();
        let input = input.reshape_unsafe(&TensorView::new(
            false,
            input_shape_trick.into_boxed_slice(),
            input_stride_trick.into_boxed_slice(),
            input_offset_trick.into_boxed_slice(),
        ));

        // Expand kernel to expected shape
        // [1, c_out, c_in, output_size_broadcastable..., kernel_size...]
        let mut kernel = kernel.unsqueeze(0);
        for _ in features {
            kernel = kernel.unsqueeze(3);
        }
        let kernel = kernel;

        let convolver = input.Multiply(&kernel);
        let convolver_dimensions = convolver.ndim();
        // 2nd dimension is [n, c_out, c_in, ...]
        // Reduce the filter dimensions as well to make it [n, c_out, output_size]
        let reduce_dimensions = [2]
            .into_iter()
            .chain(
                filters
                    .iter()
                    .enumerate()
                    .map(|(i, _)| convolver_dimensions - (i as ViewType) - 1),
            )
            .collect::<Vec<_>>();

        reduction(&convolver, &reduce_dimensions[..], false)
    }

    pub fn Conv<'a>(
        &self,
        kernel: &Tensor,
        kernel_strides: &[ViewType],
        padding: ConvPadding<'a>,
    ) -> Tensor {
        self.Convolve(kernel, kernel_strides, padding, Tensor::Sum)
    }

    pub fn InstanceNormalization(&self, epsilon: &Tensor) -> Tensor {
        assert!(
            self.ndim() == 4,
            "InstanceNormalization is only defined for a NCHW tensor"
        );

        let feature_axes = self
            .view()
            .shape
            .iter()
            .enumerate()
            .skip(2)
            .map(|(axis, _)| axis as u32)
            .collect::<Vec<_>>();
        self.Normalization(&feature_axes[..], epsilon)
    }

    pub fn Gather(&self, axis: ViewType, indices: &Tensor) -> Tensor {
        let indices_view = indices.view();
        let (batch_shape, gather_element_shape) = self.shape().split_at(axis as usize);

        let adjusted_shape = batch_shape
            .iter()
            .chain(indices_view.shape.iter())
            .chain(gather_element_shape.iter().skip(1))
            .map(|&x| x)
            .collect::<Vec<_>>();
        let adjusted_view = TensorView::from_shape(&adjusted_shape);

        // Could be done using a chain of squeeze/unsqueeze but explicitly construct the TensorView
        // instead here
        let transmuted_shape = batch_shape
            .iter()
            .map(|_| &1)
            .chain(indices_view.shape.iter())
            .chain(gather_element_shape.iter().skip(1).map(|_| &1))
            .map(|&n| n)
            .collect::<Vec<_>>();
        let transmuted_indices = indices.reshape(&TensorView::from_shape(&transmuted_shape));
        let broadcasted_indices = transmuted_indices.broadcast_to(&adjusted_view);

        self.GatherElements(axis, &broadcasted_indices)
    }

    pub fn GatherElements(&self, axis: ViewType, indices: &Tensor) -> Tensor {
        // Contrived literal implementation
        // Reduce view to a 3-tuple, say (..batch, axis, gather..) and compute indices
        let self_batch_shape = &self.shape()[axis as usize..];
        let self_batch_nelements = self_batch_shape.iter().fold(1, |acc, &x| acc * x);

        let gather_shape = &self.shape()[axis as usize + 1..];
        let gather_shape_nelements = gather_shape.iter().fold(1, |acc, &x| acc * x);

        let indices_batch_shape = &indices.shape()[axis as usize..];
        let indices_batch_nelements = indices_batch_shape.iter().fold(1, |acc, &x| acc * x);

        let self_batch_nelements_tensor = Tensor::scalar(self_batch_nelements);
        let gather_shape_nelements_tensor = Tensor::scalar(gather_shape_nelements);
        let indices_batch_nelements_tensor = Tensor::scalar(indices_batch_nelements);

        let indices_index = Tensor::arange(indices.shape());
        let indices_gather_index = indices_index.Mod(&gather_shape_nelements_tensor);
        let indices_batch_index = indices_index
            .Divide(&indices_batch_nelements_tensor)
            .Floor();

        let fixed_indices = indices_batch_index
            // batch index of [..axis]
            .Multiply(&self_batch_nelements_tensor)
            // gather axis specified offset
            .Add(&indices.Multiply(&gather_shape_nelements_tensor))
            // index within the gather shape [axis+1..]
            .Add(&indices_gather_index);
        let fixed_indices = fixed_indices.reshape(&TensorView::from_shape(&[fixed_indices.len()]));

        // Each index should get its own `mask` of self.view()
        let fixed_indices = (0..self.ndim())
            .into_iter()
            .fold(fixed_indices, |acc, _| acc.unsqueeze(acc.ndim()));
        let self_indices = Tensor::arange(self.shape());
        let mask = fixed_indices.Equal(&self_indices);

        self.Multiply(&mask)
            .Sum(&(1..=self.ndim()).collect::<Vec<_>>()[..], false)
            .reshape(indices.view())
    }

    pub fn MatMul(&self, other: &Tensor) -> Tensor {
        // m x k @ k x n
        let input = match self.ndim() {
            // m -> [1, m]
            0 => self.broadcast_to(&TensorView::from_shape(&[1, 1])),
            // [m] -> [m, 1]
            1 => self.unsqueeze(1),
            // [..., m, k]
            _ => self.clone(),
        };

        let other = match other.ndim() {
            // n -> [1, n]
            0 => other.broadcast_to(&TensorView::from_shape(&[1, 1])),
            // [n] -> [1, n]
            1 => other.unsqueeze(0),
            // [..., k, n]
            _ => other.clone(),
        };

        let input_dimension = input.ndim();
        let other_dimension = other.ndim();
        let [_batch_size @ .., _n, k] = &input.shape() else {
            panic!("MatMul requires input to be at least 2D");
        };
        let [_batch_size @ .., other_k, _m] = &other.shape() else {
            panic!("MatMul requires tensor to be at least 2D");
        };

        assert!(
            k == other_k,
            "Failed to multiply matrices of shape {:?} @ {:?}",
            self.view(),
            other.view()
        );

        // (..., n, k, 1)
        let input = input.unsqueeze(input_dimension);
        // (..., 1, k, m) - index `-3` which in this case will be axis `other_dimension + 1 - 3`
        let other = other.unsqueeze(other_dimension - 2);
        // (..., n, k, m)
        let intermediate_result = input.Multiply(&other);
        // (..., n, m) by summing along k
        let reduce_dimension = intermediate_result.ndim() - 2;
        intermediate_result.Sum(&[reduce_dimension], false)
    }

    pub fn MaxPool<'a>(
        &self,
        kernel_size: &[ViewType],
        kernel_strides: &[ViewType],
        padding: ConvPadding<'a>,
    ) -> Tensor {
        let kernel = Tensor::scalar(1).reshape(&TensorView::from_shape(kernel_size));
        self.Convolve(&kernel, kernel_strides, padding, Tensor::Max)
    }

    pub fn Normalization(&self, feature_axes: &[ViewType], epsilon: &Tensor) -> Tensor {
        let mean = self.Mean(&feature_axes[..], true);
        let variance = self.Variance(&feature_axes[..], true);

        let centered_input = self.Sub(&mean);
        let standard_deviation = variance.Add(epsilon).Sqrt();
        centered_input.Divide(&standard_deviation)
    }

    pub fn Scatter(&self, axis: ViewType, indices: &Tensor, updates: &Tensor) -> Tensor {
        self.ScatterElements(axis, ScatterReduction::None, indices, updates)
    }

    pub fn ScatterElements(
        &self,
        axis: ViewType,
        reduction: ScatterReduction,
        indices: &Tensor,
        updates: &Tensor,
    ) -> Tensor {
        // Simplify problem to a 3-tuple shape
        // (batch.., axis, ..gather)
        let self_batch_shape = &self.shape()[axis as usize..];
        let self_batch_nelements = self_batch_shape.iter().fold(1, |acc, &x| acc * x);

        let gather_shape = &self.shape()[axis as usize + 1..];
        let gather_shape_nelements = gather_shape.iter().fold(1, |acc, &x| acc * x);

        let indices_batch_shape = &indices.shape()[axis as usize..];
        let indices_batch_nelements = indices_batch_shape.iter().fold(1, |acc, &x| acc * x);

        let self_batch_nelements_tensor = Tensor::scalar(self_batch_nelements);
        let gather_shape_nelements_tensor = Tensor::scalar(gather_shape_nelements);
        let indices_batch_nelements_tensor = Tensor::scalar(indices_batch_nelements);

        let indices_index = Tensor::arange(indices.shape());
        let indices_gather_index = indices_index.Mod(&gather_shape_nelements_tensor);
        let indices_batch_index = indices_index
            .Divide(&indices_batch_nelements_tensor)
            .Floor();

        let fixed_indices = indices_batch_index
            // batch index of [..axis]
            .Multiply(&self_batch_nelements_tensor)
            // gather axis specified offset
            .Add(&indices.Multiply(&gather_shape_nelements_tensor))
            // index within the gather shape [axis+1..]
            .Add(&indices_gather_index);
        let fixed_indices = fixed_indices.reshape(&TensorView::from_shape(&[fixed_indices.len()]));

        // Each index should get its own `mask` of self.view()
        let fixed_indices = (0..self.ndim())
            .into_iter()
            .fold(fixed_indices, |acc, _| acc.unsqueeze(acc.ndim()));
        let self_indices = Tensor::arange(self.shape());
        let mask = fixed_indices.Equal(&self_indices);

        let fixed_updates = updates.reshape(&TensorView::from_shape(&[updates.len()]));
        let fixed_updates = (0..self.ndim())
            .into_iter()
            .fold(fixed_updates, |acc, _| acc.unsqueeze(acc.ndim()));

        let value_updates = fixed_updates.Multiply(&mask).Sum(&[0], false);
        match reduction {
            ScatterReduction::None => {
                let inverted_mask = mask.Max(&[0], false).Not();
                value_updates.Add(&self.Multiply(&inverted_mask))
            }
            _ => panic!("Only ScatterReduction::None is currently supported"),
        }
    }

    pub fn Slice(
        &self,
        starts: &[ViewType],
        ends: &[ViewType],
        axes: &[ViewType],
        steps: &[ViewType],
    ) -> Tensor {
        starts
            .iter()
            .zip(ends.iter())
            .zip(axes.iter())
            .zip(steps.iter())
            .fold(self.clone(), |acc, (((&start, &end), &axis), &step)| {
                let shape_at_axis = self.shape()[axis as usize];
                // `end` may exceed the shape of the axis, but does nothing special
                // Although it is important for Gather for it to be within the bounds
                let end = end.min(shape_at_axis);
                let indices = (start..end)
                    .step_by(step as usize)
                    .map(|x| x as f32)
                    .collect::<Vec<_>>();
                let indices_tensor = Tensor::from_contiguous(&indices[..], &[indices.len() as u32]);
                // Equivalent to slicing by one dimension each time
                acc.Gather(axis, &indices_tensor)
            })
    }
}
