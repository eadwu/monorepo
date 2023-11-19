use std::collections::HashMap;

use tensor::primitives::tensor::*;
use tensor::topograph::GraphView;

use crate::backprop::{BackPropable, BackPropagation, Gradients, TensorGradient};

impl BackPropagation for Tensor {
    fn backward(&self) -> Vec<TensorGradient> {
        self.data().backward(self)
    }

    fn backprop(&self, tensors: &[&Tensor]) -> Gradients {
        let runtime = self.as_runtime_graph();
        let mut gradients = HashMap::new();

        runtime.graph.iter().rev().for_each(|tensor| {
            tensor.backward().into_iter().for_each(|tensor_gradient| {
                let target_tensor_id = tensor_gradient.gradient_of().id();

                // Gradient is either the first of the product of the chain rule
                let gradient = gradients
                    .get(&target_tensor_id)
                    .map(|previous_gradient| tensor_gradient.gradient().Multiply(previous_gradient))
                    .unwrap_or(tensor_gradient.gradient().clone());

                gradients.insert(target_tensor_id, gradient);
            })
        });

        // Only return the gradients for the Tensors requested
        tensors
            .iter()
            .map(|tensor| {
                (
                    tensor.id(),
                    gradients.get(&tensor.id()).map(|tensor| tensor.clone()),
                )
            })
            .collect::<HashMap<_, _>>()
    }
}

impl BackPropable for UnarySpec {
    fn backward(&self, output: &Tensor) -> Vec<TensorGradient> {
        vec![TensorGradient::new(
            self.input.clone(),
            match self.op {
                UnaryType::ABS => self.input.Sign(),
                UnaryType::CAST => self.input.Equal(&output.Cast(self.input.datatype())),
                UnaryType::CEIL => self.input.Equal(&output.Cast(self.input.datatype())),
                // 2^x * ln(2)
                UnaryType::EXP2 => Tensor::scalar(std::f32::consts::LN_2).Multiply(output),
                UnaryType::FLOOR => self.input.Equal(&output.Cast(self.input.datatype())),
                UnaryType::IDENTITY => Tensor::scalar(1),
                // 1/(xln(2))
                UnaryType::LOG2 => Tensor::scalar(std::f32::consts::LN_2)
                    .Multiply(&self.input)
                    .Recip(),
                // -(1/x^2)
                UnaryType::RECIP => self.input.Pow(&Tensor::scalar(2)).Recip().Neg(),
                // cos(x)
                UnaryType::SIN => self.input.Cos(),
                // 1 / (2sqrt(x))
                UnaryType::SQRT => Tensor::scalar(2).Multiply(output).Recip(),
            },
        )]
    }
}

impl BackPropable for BinarySpec {
    fn backward(&self, output: &Tensor) -> Vec<TensorGradient> {
        let (lhs_gradient, rhs_gradient) = match self.op {
            BinaryType::ADD => (Tensor::scalar(1), Tensor::scalar(1)),
            BinaryType::DIVIDE => (
                // 1/y
                self.rhs.Recip(),
                // -x * 1/y**2
                self.rhs
                    .Pow(&Tensor::scalar(2))
                    .Recip()
                    .Multiply(&self.lhs)
                    .Neg(),
            ),
            // Passthrough gradients for comparison operators
            BinaryType::EQUAL => (Tensor::scalar(1), Tensor::scalar(1)),
            BinaryType::LESSTHAN => (Tensor::scalar(1), Tensor::scalar(1)),
            BinaryType::MAX => (output.Equal(&self.lhs), output.Equal(&self.rhs)),
            // https://math.stackexchange.com/questions/2364491/derivative-of-modulus-operator
            // https://math.stackexchange.com/questions/2651437/partial-of-modulo-operator-with-non-integers
            BinaryType::MOD => {
                let non_zero = output.Equal(&Tensor::scalar(0)).Not();
                (
                    Tensor::scalar(1).Multiply(&non_zero),
                    self.lhs.Divide(&self.rhs).Floor(),
                )
            }
            BinaryType::MULTIPLY => (self.rhs.clone(), self.lhs.clone()),
            BinaryType::SUB => (Tensor::scalar(1), Tensor::scalar(-1)),
        };

        vec![
            TensorGradient::new(self.lhs.clone(), lhs_gradient),
            TensorGradient::new(self.rhs.clone(), rhs_gradient),
        ]
    }
}

impl BackPropable for ReduceSpec {
    fn backward(&self, output: &Tensor) -> Vec<TensorGradient> {
        vec![TensorGradient::new(
            self.input.clone(),
            match self.op {
                ReduceType::SUM => Tensor::scalar(1),
                ReduceType::MAX => {
                    let output = if output.ndim() != self.input.ndim() {
                        // Squeezed so unsqueeze
                        output.unsqueeze(self.axis)
                    } else {
                        output.clone()
                    };

                    // Gradient of 1 iff number is maximum
                    output.Equal(&self.input)
                }
            },
        )]
    }
}

impl BackPropable for OperationSpec {
    fn backward(&self, output: &Tensor) -> Vec<TensorGradient> {
        match self {
            OperationSpec::UnaryOp(op) => op.backward(output),
            OperationSpec::BinaryOp(op) => op.backward(output),
            OperationSpec::ReduceOp(op) => op.backward(output),
        }
    }
}

impl BackPropable for TensorInput {
    fn backward(&self, output: &Tensor) -> Vec<TensorGradient> {
        match self {
            TensorInput::ExplicitInput(_) => {
                vec![TensorGradient::new(output.clone(), Tensor::scalar(1))]
            }
            TensorInput::Invalidated => panic!("Invalid TensorInput to perform backpropagation"),
            TensorInput::NoOp(_) => vec![TensorGradient::new(output.clone(), Tensor::scalar(1))],
            TensorInput::OperationResult(op) => op.backward(output),
        }
    }
}
