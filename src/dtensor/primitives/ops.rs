pub mod builders;

use builders::{*};
use crate::dtensor::primitives::{Tensor};

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
