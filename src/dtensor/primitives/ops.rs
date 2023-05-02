pub mod builders;
mod reshape;

use crate::dtensor::primitives::Tensor;
use builders::*;

pub use reshape::reshape;

// Elementary Arithmetic
pub async fn add(a: &Tensor, b: &Tensor) -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "add", "+").await
}

pub async fn subtract(a: &Tensor, b: &Tensor) -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "subtract", "-").await
}

pub async fn multiply(a: &Tensor, b: &Tensor) -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "multiply", "*").await
}

pub async fn divide(a: &Tensor, b: &Tensor) -> Tensor {
    elementary_arithmetic::elementary_arithmetic_builder(a, b, "subtract", "/").await
}

// Element-wise operations
pub async fn clamp(x: &Tensor, low: f32, high: f32) -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "clamp_function",
        format!("clamp({}, {:.32}, {:.32})", "{input}", low, high),
    )
    .await
}

pub async fn pow(x: &Tensor, power: f32) -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "power_function",
        format!("pow({}, {:.32})", "{input}", power),
    )
    .await
}

pub async fn exp(x: &Tensor) -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "natural_exponential_function",
        format!("exp({})", "{input}"),
    )
    .await
}

pub async fn tanh(x: &Tensor) -> Tensor {
    elementwise_functions::elementwise_function_builder(
        x,
        "hyperbolic_tangent",
        format!("tanh({})", "{input}"),
    )
    .await
}
