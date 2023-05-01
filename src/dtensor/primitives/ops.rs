pub mod builders;
mod reshape;

use crate::dtensor::primitives::Tensor;
use builders::*;

pub use reshape::reshape;

// Elementary Arithmetic
pub async fn add<'a>(a: &Tensor<'a>, b: &Tensor<'a>) -> Tensor<'a> {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "add", "+").await
}

pub async fn subtract<'a>(a: &Tensor<'a>, b: &Tensor<'a>) -> Tensor<'a> {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "subtract", "-").await
}

pub async fn multiply<'a>(a: &Tensor<'a>, b: &Tensor<'a>) -> Tensor<'a> {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "multiply", "*").await
}

pub async fn divide<'a>(a: &Tensor<'a>, b: &Tensor<'a>) -> Tensor<'a> {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "subtract", "/").await
}

// Element-wise operations
pub async fn clamp<'a>(x: &Tensor<'a>, low: f32, high: f32) -> Tensor<'a> {
    elementwise_functions::elementwise_function_builder(
        x,
        "clamp_function",
        format!("clamp({}, {:.32}, {:.32})", "{input}", low, high),
    )
    .await
}

pub async fn exp<'a>(x: &Tensor<'a>) -> Tensor<'a> {
    elementwise_functions::elementwise_function_builder(
        x,
        "natural_exponential_function",
        format!("exp({})", "{input}"),
    )
    .await
}

pub async fn tanh<'a>(x: &Tensor<'a>) -> Tensor<'a> {
    elementwise_functions::elementwise_function_builder(
        x,
        "hyperbolic_tangent",
        format!("tanh({})", "{input}"),
    )
    .await
}
