// struct Tensor {
//   rank: u32,
//   shape: array<u32>,
//   stride: array<u32>,
//   contiguous_stride: array<u32>,
//   data: array<f32>,
// }

@group(0) @binding(0) var<storage, read> left: array<f32>;
@group(0) @binding(1) var<uniform> left_rank: u32;
@group(0) @binding(2) var<storage, read> left_shape: array<u32>;
@group(0) @binding(3) var<storage, read> left_stride: array<u32>;
@group(0) @binding(4) var<storage, read> left_contiguous_stride: array<u32>;

@group(1) @binding(0) var<storage, read> right: array<f32>;
@group(1) @binding(1) var<uniform> right_rank: u32;
@group(1) @binding(2) var<storage, read> right_shape: array<u32>;
@group(1) @binding(3) var<storage, read> right_stride: array<u32>;
@group(1) @binding(4) var<storage, read> right_contiguous_stride: array<u32>;

@group(2) @binding(0) var<storage, read_write> sum: array<f32>;
@group(2) @binding(1) var<uniform> sum_size: u32;

@compute @workgroup_size(64, 1, 1)
fn main(@builtin(global_invocation_id) global_id: vec3u) {
  // Guard against out-of-bounds work group sizes
  // By definition workgroup grid is >= 0 so there is no need to check that
  if (global_id.x >= sum_size) {
    return;
  }

  // The index for the matrix needs to be reconstructed from the [real] stride, say
  //   0 1 2
  //   3 4 5
  //   with shape 2x3 and stride 3x1
  // M[4] should be 4 if it is row-major contiguous
  // So it needs to be deconstructed to M[1][1] which is
  //   4 // 3 = 1
  //   4 - 3*1 = 1
  //
  // Now transpose with shape 3x2 and an internal stride of 1x3
  //                              and an actual stride of 2x1
  //   0 3
  //   1 4
  //   2 5
  //   Stored as `0 1 2 3 4 5` internally with stride 2x1
  //          vs `0 3 1 4 2 5` with a stride of 2x1
  // The contiguous stride can be computed through a right-to-left rolling
  // accumulating product, starting from 1, stride[i] = stride[i+1] * shape[i+1]
  let index = global_id.x;

  var contiguous_offset = index;
  var mapped_offset = 0u;

  for (var i = 0u; i < left_rank; i++) {
    mapped_offset = mapped_offset + (contiguous_offset / left_contiguous_stride[i] * left_stride[i]);
    contiguous_offset = contiguous_offset % left_contiguous_stride[i];
  }

  let left_index = mapped_offset;
  contiguous_offset = index;
  mapped_offset = 0u;

  for (var i = 0u; i < right_rank; i++) {
    mapped_offset = mapped_offset + (contiguous_offset / right_contiguous_stride[i] * right_stride[i]);
    contiguous_offset = contiguous_offset % right_contiguous_stride[i];
  }

  let right_index = mapped_offset;

  sum[index] = left[index] + right[index];
}
