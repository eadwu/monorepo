use crate::primitives::tensor::{Tensor, TensorView, ViewType};

#[derive(Clone, Copy, Debug)]
pub enum ConvPadding<'a> {
    Same,
    Valid,
    Custom(&'a [(ViewType, ViewType)])
}

impl Tensor {
    pub fn Clip(&self, min: &Tensor, max: &Tensor) -> Tensor {
        self.Minimum(min).Maximum(max)
    }

    pub fn Conv2D<'a>(
        &self,
        kernel: &Tensor,
        strides: (ViewType, ViewType),
        padding: ConvPadding<'a>,
    ) -> Tensor {
        // https://jessicastringham.net/2017/12/31/stride-tricks/
        let input = self.contiguous();
        let kernel = kernel.contiguous();

        let (stride_h, stride_w) = &strides;
        let [n @ .., c_in, h_in, w_in] = &input.view().shape[..] else {
            panic!("Unsupported input for Conv2D, expected signature of N x C x H x W")
        };
        let [c_out, k_h, k_w] = &kernel.view().shape[..] else {
            panic!("Unsupported kernel for Conv2D, expected signature of C_out x K0 x K1")
        };

        let axis_padding = if let ConvPadding::Valid = padding {
            self.view().shape.iter().map(|_| (0, 0)).collect::<Vec<_>>()
        } else if let ConvPadding::Same = padding {
            assert!(*stride_h == 1, "`same` padding expects a stride of 1, got {}", stride_h);
            assert!(*stride_w == 1, "`same` padding expects a stride of 1, got {}", stride_w);

            // P = ((S-1)*W-S+F)/2, with F = filter size, S = stride, W = input size
            // https://stats.stackexchange.com/questions/297678/how-to-calculate-optimal-zero-padding-for-convolutional-neural-networks
            n.iter()
                .chain(std::iter::once(c_in))
                .map(|_| (0, 0))
                .chain(
                    // Compute output size using input size, kernel size, and stride size
                    [h_in, w_in]
                        .iter()
                        .zip([k_h, k_w].iter().zip([stride_h, stride_w].iter()))
                        .map(|(&&input, (&&filter, &&stride))| {
                            let padding_needed = (stride - 1) * input - stride + filter;
                            // Extra padding is always to the "right"
                            let padding_post = padding_needed.div_ceil(2);
                            let padding_pre = padding_needed - padding_post;
                            (padding_pre, padding_post)
                        }),
                )
                .collect::<Vec<_>>()
        } else if let ConvPadding::Custom(axis_padding) = padding {
            axis_padding.to_vec()
        } else {
            unreachable!("Unsupporting ({:?}) padding strategy for Conv2D", padding);
        };

        let input = input.pad(&axis_padding);
        let [n @ .., c_in, h_in, w_in] = &input.view().shape[..] else {
            panic!("Unsupported input for Conv2D, expected signature of N x C x H x W")
        };
        let [n_stride @ .., c_in_stride, h_in_stride, w_in_stride] = &input.view().stride[..]
        else {
            panic!("Unsupported input for Conv2D, expected signature of N x C x H x W")
        };

        // X_out = (X_in + 2*padding - dilation*(kernel_size-1) - 1 / stride).floor()
        // h_in is padded here now
        let h_out = (h_in - 1 * (k_h - 1) - 1) / stride_h + 1;
        let w_out = (w_in - 1 * (k_w - 1) - 1) / stride_w + 1;

        // (n, 1, c_in, h_out, w_out, k_h, k_w)
        // n batch size of
        // c_out (1 broadcasted) output channels which are the
        // sum of c_in input channels which are of size
        // (h_out, w_out) computed using
        // (k_h, k_w) convolved kernels
        //
        // Simplier explanation of (h_out, w_out, k_h, k_w), since it is known
        // the output size is (h_out, w_out), use some stride_tricks to get the
        // (k_h, k_w) window for each element that is convolved with the kernel
        let desired_input_shape = n
            .iter()
            .map(|&x| x)
            .chain([1, *c_in, h_out, w_out, *k_h, *k_w].into_iter())
            .collect::<Vec<_>>();
        let desired_input_stride = n_stride
            .iter()
            .map(|&x| x)
            // The distance between each channel is inherited (c_in_stride)
            .chain([0, *c_in_stride].into_iter())
            // These strides control the distance between each window
            .chain([*h_in_stride * stride_h, *w_in_stride * stride_w].into_iter())
            // These strides control how to index the elements within the window
            .chain([*h_in_stride, *w_in_stride].into_iter())
            .collect::<Vec<_>>();
        let input = input.reshape_unsafe(&TensorView::new(
            false,
            desired_input_shape.into_boxed_slice(),
            desired_input_stride.into_boxed_slice(),
        ));

        let desired_kernel_shape = n
            .iter()
            .map(|_| 1)
            .chain([*c_out, 1, 1, 1, *k_h, *k_w].into_iter())
            .collect::<Vec<_>>();
        let kernel = kernel.reshape(&TensorView::from_shape(&desired_kernel_shape));

        let convolve = input.Multiply(&kernel);
        let convolve_dimension = convolve.view().dimension();
        convolve.Sum(
            // [ ..., c_out, c_in, h_out, w_out, k_h, k_w ]
            // along c_in, k_h, k_w
            &[
                convolve_dimension - 5,
                convolve_dimension - 2,
                convolve_dimension - 1,
            ],
            false,
        )
    }

    pub fn MatMul(&self, other: &Tensor) -> Tensor {
        // m x k @ k x n
        let input = match self.view().dimension() {
            // m -> [1, m]
            0 => self.broadcast_to(&TensorView::from_shape(&[1, 1])),
            // [m] -> [m, 1]
            1 => self.unsqueeze(1),
            // [..., m, k]
            _ => self.clone(),
        };

        let other = match other.view().dimension() {
            // n -> [1, n]
            0 => other.broadcast_to(&TensorView::from_shape(&[1, 1])),
            // [n] -> [1, n]
            1 => other.unsqueeze(0),
            // [..., k, n]
            _ => other.clone(),
        };

        let input_dimension = input.view().dimension();
        let other_dimension = other.view().dimension();
        let [_batch_size @ .., _n, k] = &input.view().shape[..] else {
            panic!("MatMul requires input to be at least 2D");
        };
        let [_batch_size @ .., other_k, _m] = &other.view().shape[..] else {
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
        let reduce_dimension = intermediate_result.view().dimension() - 2;
        intermediate_result.Sum(&[reduce_dimension], false)
    }
}
