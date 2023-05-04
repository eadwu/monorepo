pub mod builders;
mod gemm;
mod reshape;
mod embedding_lookup;

use crate::dtensor::primitives::Tensor;
use builders::*;

pub use gemm::{mm, MM};
pub use reshape::{reshape, Reshape};
pub use embedding_lookup::{embedding_lookup, EmbeddingLookup};

macro_rules! impl_tensor_op {
        ($f_op:ident $op:tt |$( $i:ident : $v:ty ),*| -> $out:path $body:block) => {
        pub struct $f_op;
        impl $f_op {
            pub async fn forward(&self, $( $i : $v ),*) -> $out $body
        }

        pub async fn $op($( $i : $v ),*) -> $out {
            $f_op.forward($( $i ),*).await
        }
    };
}
pub (crate) use impl_tensor_op;

// Elementary Arithmetic
impl_tensor_op!(Add add |a: &Tensor, b: &Tensor| -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "add", "+").await
});

impl_tensor_op!(Subtract subtract |a: &Tensor, b: &Tensor| -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "subtract", "-").await
});

impl_tensor_op!(Multiply multiply |a: &Tensor, b: &Tensor| -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "multiply", "*").await
});

impl_tensor_op!(Divide divide |a: &Tensor, b: &Tensor| -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "divide", "/").await
});

// Element-wise operations
impl_tensor_op!(Clamp clamp |x: &Tensor, low: f32, high: f32| -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "clamp_function",
        format!("clamp({}, {:.32}, {:.32})", "{input}", low, high),
    )
    .await
});

impl_tensor_op!(Pow pow |x: &Tensor, power: f32| -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "power_function",
        format!("pow({}, {:.32})", "{input}", power),
    )
    .await
});

impl_tensor_op!(Exp exp |x: &Tensor| -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "natural_exponential_function",
        format!("exp({})", "{input}"),
    )
    .await
});

impl_tensor_op!(Sqrt sqrt |x: &Tensor| -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "square_root_function",
        format!("sqrt({})", "{input}"),
    )
    .await
});

impl_tensor_op!(Tanh tanh |x: &Tensor| -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "hyperbolic_tangent",
        format!("tanh({})", "{input}"),
    )
    .await
});
