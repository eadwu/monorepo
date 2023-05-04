// https://pytorch.org/docs/stable/generated/torch.nn.GELU.html
use crate::dtensor::{primitives::ops::*, primitives::Tensor};

const C1: f32 = 0.5;
const C2: f32 = 1.0;
const C3: f32 = (2.0 / std::f64::consts::PI) as f32;
const C4: f32 = 0.044715;

pub struct GeLU {}
impl GeLU {
    pub async fn new() -> GeLU {
      GeLU {  }
    }

    pub async fn forward(&self, x: &Tensor) -> Tensor {
        // GELU(x) = 0.5*x*(1+tanh(sqrt(2/pi) * (x+0.044715+x^3)))
        C1 * x * (C2 + tanh(&(C3 * (x + C4 + pow(x, 3.0).await))).await)
    }
}

/*
// PyTorch 2.0
pub struct GELU_PARAMS {
    approximate: Some(bool),
}

fn GELU_SHADER(params: &GELU_PARAMS) -> str {
    "
// Constants
const a1: f32 =  0.254829592;
const a2: f32 = -0.284496736;
const a3: f32 =  1.421413741;
const a4: f32 = -1.453152027;
const a5: f32 =  1.061405429;
const p : f32 =  0.3275911;
fn erf(a: f32) -> f32 {
  var sign: f32;
  if a >= 0 {
    sign = 1;
  } else {
    sign = -1;
  }

  let x: f32 = abs(a);

  // A&S formula 7.1.26
  t = 1.0/(1.0 + p*x);
  y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);
  return sign*y # erf(-x) = -erf(x);
}

fn gelu(x: f32) -> f32 {
  if (x < -10.0) {
    return 0.0;
  } else if (x > 10.0) {
    return x;
  } else {
    let cdf_approx: f32 = 0.5 * (1.0 + tanh(SQRPI * (x + 0.044715 * pow(x, 3))));
    return x * cdf_approx;
  }
}

struct TensorShape {
  rank: u32,
  shape: array<f32>,
  stride: array<f32>,
}

struct Tensor {
  data: array<f32>,
}

@group(0) @binding(0) var<storage, read> left: Tensor;
@group(0) @binding(1) var<storage, read> leftShape: TensorShape;

@group(1) @binding(0) var<storage, read> right: Tensor;
@group(1) @binding(1) var<storage, read> rightShape: TensorShape;

@group(2) @binding(0) var<storage, read_write> sum: Tensor;

@compute @workground_size(64, 1, 1)
fn main(@builtin(global_invocation_id) global_id: vec3u) {
  // Index for the sum is the same
  //
  // The index for the matrix needs to be reconstructed from the stride, say
  //   0 1 2
  //   3 4 5
  //   with stride 3x1
  // M[4] should be 4 if it is row-major contiguous
  // So it needs to be deconstructed to M[1][1] which is
  //   4 // 3 = 1
  //   4 - 3*1 = 1
  //
  // Now transpose with a stride of 1x3
  //   0 3
  //   1 4
  //   2 5
  //   Stored as `0 1 2 3 4 5`
  //
  // M[4] = 2 if it was row-major contiguous but is actually M[2] = 2 internally
  //   Using M[2][0] would give the perfect mapping using the stride
  //     2*1 + 3*0 = 2
  // M[4] // 3
  //   4 //  = 2
  //  (4 - 2*2) // 1 = 0
  //
  // Now with M[2][0] the offset can be calculated as 2*2 + 1*0 = 5
  let index = global_id.x;

  // NOTE: Implement broadcasting ...
  // // Compute index for left Tensor element
  // var leftOffset = index;
  // let leftElement = 0;
  // for (var i = 0u; i < leftShape.rank; i = i + 1u) {
  //   let stride = floor(leftOffset / leftShape.stride[i]) * leftShape.stride[i];
  //   leftOffset -= stride;
  //   leftElement += stride;
  //   leftOffset = leftOffset - floor(leftOffset / leftShape.stride[i]) * leftShape.stride[i];
  // }

  sum[index] = left.data[index] + right.data[index];
}
"
}
*/
