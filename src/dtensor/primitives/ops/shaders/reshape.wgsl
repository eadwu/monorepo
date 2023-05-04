struct TensorShape {
  rank: u32,
  shape: array<u32>,
  stride: array<u32>,
}

struct Tensor {
  data: array<f32>,
}

@group(0) @binding(0) var<storage, read> tensor: Tensor;
@group(0) @binding(1) var<storage, read> tensor_shape: TensorShape;

@group(1) @binding(0) var<storage, read> new_shape: TensorShape;

@group(2) @binding(0) var<storage, read_write> reshaped_tensor: Tensor;

fn compute_contiguous_stride(tensor_shape: TensorShape) -> array<u32> {
  var stride: array<u32,tensor_shape.rank>;
  var offset: u32 = 1;
  for (var i = tensor_shape.rank - 1; i >= 0; i--) {
    stride[i] = offset;
    offset = offset * tensor_shape.shape[i];
  }

  return stride;
}

fn map_offset(flat_index: u32, current_stride: array<u32>, alternate_stride: array<u32>) -> u32 {
  let rank = length(current_stride);
  var indices = array<u32,rank>;

  // Generate the indices as if the offset represented the data in a contiguous array.
  var offset = flat_index;
  for (var i = 0u; i < rank; i++) {
    // TODO: Check that this is integer division
    indices[i] = offset / current_stride[i];
    offset = offset % current_stride[i];
  }

  // Map over to the actual stride
  offset = 0;
  for (var i = 0u; i < rank; i++) {
    offset = offset + (indices[i] * alternate_stride[i]);
  }

  return offset;
}

@compute @workground_size(64, 1, 1)
fn main(@builtin(global_invocation_id) global_id: vec3u) {
  let contiguous_stride = compute_contiguous_stride(tensor_shape);

  let index = global_id.x;
  let mapped_index = map_offset(index, contiguous_stride, tensor_shape.stride);

  sum[index] = tensor.data[mapped_index];
}
