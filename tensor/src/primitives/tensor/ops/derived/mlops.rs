use num_traits::AsPrimitive;

use crate::primitives::tensor::{Tensor, TensorType, ViewType};

impl Tensor {
    /* BinaryOp derivations */
    pub fn And(&self, other: &Tensor) -> Tensor {
        // Given binary values 0/1
        // AND corresponds to multiplication
        self.Multiply(other)
    }

    pub fn Greater(&self, other: &Tensor) -> Tensor {
        self.LessOrEqual(other).Not()
    }

    pub fn GreaterOrEqual(&self, other: &Tensor) -> Tensor {
        self.LessThan(other).Not()
    }

    pub fn LessOrEqual(&self, other: &Tensor) -> Tensor {
        self.LessThan(other).Or(&self.Equal(other))
    }

    pub fn Minimum(&self, other: &Tensor) -> Tensor {
        self.Neg().Maximum(&other.Neg()).Neg()
    }

    pub fn Or(&self, other: &Tensor) -> Tensor {
        self.Maximum(other)
    }

    pub fn Pow(&self, power: &Tensor) -> Tensor {
        self.Log2().Multiply(power).Exp2()
    }

    /* UnaryOp derivations */
    pub fn Celu<T: AsPrimitive<TensorType>>(&self, alpha: T) -> Tensor {
        let zero = Tensor::scalar(0);
        let one = Tensor::scalar(1);
        let alpha = Tensor::scalar(alpha);

        let celu = self.Divide(&alpha).Exp().Sub(&one).Multiply(&alpha);
        let masked_celu = zero.Minimum(&celu);
        self.Relu().Add(&masked_celu)
    }

    pub fn Cos(&self) -> Tensor {
        // Defined as the complement of Sin
        let pi_2 = Tensor::scalar(std::f32::consts::FRAC_PI_2);
        pi_2.Sub(&self).Sin()
    }

    pub fn Cosh(&self) -> Tensor {
        let one = Tensor::scalar(1);
        let two = Tensor::scalar(2);
        let numerator = one.Add(&two.Multiply(self).Neg().Exp());
        let denominator = self.Neg().Exp().Multiply(&two);
        numerator.Divide(&denominator)
    }

    pub fn Cot(&self) -> Tensor {
        self.Cos().Divide(&self.Sin())
    }

    pub fn Csc(&self) -> Tensor {
        let one = Tensor::scalar(1);
        one.Divide(&self.Sin())
    }

    pub fn Csch(&self) -> Tensor {
        let one = Tensor::scalar(1);
        one.Divide(&self.Sinh())
    }

    pub fn Elu<T: AsPrimitive<TensorType>>(&self, alpha: T) -> Tensor {
        let zero = Tensor::scalar(0);
        let one = Tensor::scalar(1);
        let alpha = Tensor::scalar(alpha);

        let leak = alpha.Multiply(&self.Exp().Sub(&one));
        let mask = self.LessOrEqual(&zero);
        let masked_leak = leak.Multiply(&mask);

        // z if z > 0
        // alpha * (exp(z) - 1) if z <= 0
        self.Relu().Add(&masked_leak)
    }

    pub fn Erf(&self) -> Tensor {
        let one = Tensor::scalar(1);

        let a1 = Tensor::scalar(0.254829592);
        let a2 = Tensor::scalar(-0.284496736);
        let a3 = Tensor::scalar(1.421413741);
        let a4 = Tensor::scalar(-1.453152027);
        let a5 = Tensor::scalar(1.061405429);
        let p = Tensor::scalar(0.3275911);

        let x = self.Abs();
        let sign = self.Divide(&x);

        // A&S Formula 7.1.26
        let t = one.Divide(&one.Add(&p.Multiply(&x)));
        let y_p1 = a5.Multiply(&t).Add(&a4);
        let y_p2 = y_p1.Multiply(&t).Add(&a3);
        let y_p3 = y_p2.Multiply(&t).Add(&a2);
        let y_p4 = y_p3.Multiply(&t).Add(&a1);
        let y = y_p4.Multiply(&t).Multiply(&x.Neg().Multiply(&x).Exp());

        sign.Multiply(&y)
    }

    pub fn Exp(&self) -> Tensor {
        let log_e_base_2 = Tensor::scalar(std::f32::consts::E.log2());
        self.Multiply(&log_e_base_2).Exp2()
    }

    pub fn Gelu(&self) -> Tensor {
        // Approximation via Tanh
        // https://pytorch.org/docs/stable/generated/torch.nn.GELU.html
        let point_5 = Tensor::scalar(0.5);
        let one = Tensor::scalar(1);

        let sqrt_2_over_pi = Tensor::scalar((2.0 / std::f32::consts::PI).sqrt());
        let constant = Tensor::scalar(0.044715);
        let three = Tensor::scalar(3);

        let tanh = sqrt_2_over_pi
            .Multiply(&self.Add(&constant.Multiply(&self.Pow(&three))))
            .Tanh();
        point_5.Multiply(self).Multiply(&one.Add(&tanh))
    }

    pub fn HardSigmoid(&self) -> Tensor {
        let half = Tensor::scalar(0.5);
        let three = Tensor::scalar(3);
        let six = Tensor::scalar(6);
        let neg_three = three.Neg();

        let hard_neg_mask = self.LessOrEqual(&neg_three);
        let hard_pos_mask = self.GreaterOrEqual(&three);
        let legal_mask = hard_neg_mask.Or(&hard_pos_mask);

        let hard_sigmoid = self.Divide(&six).Add(&half);
        hard_sigmoid.Multiply(&legal_mask).Add(&hard_pos_mask)
    }

    pub fn HardSwish(&self) -> Tensor {
        let three = Tensor::scalar(3);
        let six = Tensor::scalar(6);
        let neg_three = three.Neg();

        let hard_neg_mask = self.LessOrEqual(&neg_three);
        let hard_pos_mask = self.GreaterOrEqual(&three);
        let legal_mask = hard_neg_mask.Or(&hard_pos_mask);

        let hard_swish = self.Multiply(&self.Add(&three).Divide(&six));
        hard_swish
            .Multiply(&legal_mask)
            .Add(&self.Multiply(&hard_pos_mask))
    }

    pub fn LeakyRelu<T: AsPrimitive<TensorType>>(&self, alpha: T) -> Tensor {
        let zero = Tensor::scalar(0);
        let alpha = Tensor::scalar(alpha);

        let leak = alpha.Multiply(self);
        let mask = self.LessOrEqual(&zero);
        let masked_leak = leak.Multiply(&mask);

        // z if z > 0
        // alpha * z if z <= 0
        self.Relu().Add(&masked_leak)
    }

    pub fn Log(&self) -> Tensor {
        let log_2 = Tensor::scalar(2_f32.log(std::f32::consts::E));
        self.Log2().Multiply(&log_2)
    }

    pub fn LogSoftmax(&self, axis: ViewType) -> Tensor {
        self.Softmax(axis).Log()
    }

    pub fn Mish<T: AsPrimitive<TensorType>>(&self, beta: T, threshold: T) -> Tensor {
        self.Multiply(&self.Softplus(beta, threshold).Tanh())
    }

    pub fn Neg(&self) -> Tensor {
        let neg_one = Tensor::scalar(-1);
        neg_one.Multiply(self)
    }

    pub fn Not(&self) -> Tensor {
        let one = Tensor::scalar(1);
        one.Sub(self)
    }

    pub fn PRelu<T: AsPrimitive<TensorType>>(&self, alpha: T) -> Tensor {
        let zero = Tensor::scalar(0);
        let alpha = Tensor::scalar(alpha);

        // PReLU(x) = max(0,x) + a∗min(0,x)
        self.Relu().Add(&alpha.Multiply(&zero.Minimum(self)))
    }

    pub fn Relu(&self) -> Tensor {
        let zero = Tensor::scalar(0);
        self.Maximum(&zero)
    }

    pub fn Sec(&self) -> Tensor {
        let one = Tensor::scalar(1);
        one.Divide(&self.Cos())
    }

    pub fn Selu<T: AsPrimitive<TensorType>>(&self, alpha: T, scale: T) -> Tensor {
        let scale = Tensor::scalar(scale);
        self.Elu(alpha).Multiply(&scale)
    }

    pub fn Sigmoid(&self) -> Tensor {
        let one = Tensor::scalar(1);
        let denominator = one.Add(&self.Neg().Exp());
        one.Divide(&denominator)
    }

    pub fn Sech(&self) -> Tensor {
        let one = Tensor::scalar(1);
        one.Divide(&self.Cosh())
    }

    pub fn Sinh(&self) -> Tensor {
        let one = Tensor::scalar(1);
        let two = Tensor::scalar(2);
        let numerator = one.Sub(&two.Multiply(self).Neg().Exp());
        let denominator = self.Neg().Exp().Multiply(&two);
        numerator.Divide(&denominator)
    }

    pub fn Softmax(&self, axis: ViewType) -> Tensor {
        let stablized = self.Sub(&self.Max(&[axis], true)).Exp();
        let sum = stablized.Sum(&[axis], true);

        stablized.Divide(&sum)
    }

    pub fn Softplus<T: AsPrimitive<TensorType>>(&self, beta: T, threshold: T) -> Tensor {
        let one = Tensor::scalar(1);
        let beta = Tensor::scalar(beta);
        let threshold = Tensor::scalar(threshold);

        let in_threshold = self.LessOrEqual(&threshold).Multiply(self);
        let out_threshold = self.Greater(&threshold).Multiply(self);

        let scale = one.Divide(&beta);
        let smoothed = one.Add(&beta.Multiply(&in_threshold).Exp()).Log();
        scale.Multiply(&smoothed).Add(&out_threshold)
    }

    pub fn Tan(&self) -> Tensor {
        self.Sin().Divide(&self.Cos())
    }

    pub fn Tanh(&self) -> Tensor {
        self.Sinh().Divide(&self.Cosh())
    }

    /* ReduceOp derivations */
    pub fn Mean(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        let one = Tensor::scalar(1);

        let counter = one.reshape(self.view());
        let n = counter.Sum(axes, keep_dims);
        self.Sum(axes, keep_dims).Divide(&n)
    }

    pub fn Min(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.Neg().Max(axes, keep_dims).Neg()
    }

    pub fn L1(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        self.Abs().Sum(axes, keep_dims)
    }

    pub fn L2(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        let two = Tensor::scalar(2);

        self.Pow(&two).Sum(axes, keep_dims).Sqrt()
    }

    pub fn LogSumExp(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        let stabilizer = self.Max(axes, keep_dims);
        self.Sub(&stabilizer)
            .Exp()
            .Sum(axes, keep_dims)
            .Log()
            .Add(&stabilizer)
    }

    pub fn Variance(&self, axes: &[ViewType], keep_dims: bool) -> Tensor {
        let one = Tensor::scalar(1);
        let two = Tensor::scalar(2);

        let counter = one.reshape(self.view());
        let n = counter.Sum(axes, keep_dims);
        let mean = self.Mean(axes, keep_dims);
        self.Sub(&mean)
            .Pow(&two)
            .Sum(axes, keep_dims)
            .Divide(&n.Sub(&one))
    }
}
