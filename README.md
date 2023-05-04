The original backend for implementing the backend has been scrapped with the incoming advent of Mojo, there's no point in maintaining extra code.

Stored away in the branch `dtensor-rs`, consider it as EOL with likely inference support for Whisper being the last development.

The main focus will now pivot to supporting Tensor operations in a distributed fashion.

------

# Auto-Differentiation

No changes will be needed as it will be built off the building blocks already existent.

# Tensor

- Tensors are __immutable__

## Representation

```
Tensor {
  shape
  stride
  contiguous_stride

  length
  size

  quad_tensor
}
```

Strides will be included as a way to allow for some "low-cost" operations, such as `transpose` and `broadcasting`.
- `contiguous_stride` may be removed but it allows for easy computation when doing naive translations between different Tensors

### Quad Tensor

How to represent a distributed `Tensor`? Well since it is rectangular it makes sense to represent it as a `QuadTree`. Is it the best? Maybe, maybe not, but it is the most natural progression.

```
QuadTensor {
  north_west: Option<QuadTensor>
  north_east: Option<QuadTensor>
  south_east: Option<QuadTensor>
  south_west: Option<QuadTensor>

  data
}
```

Maximum size of each `QuadTensor`? Let's say 256MB, the same restriction as WebGPU has on its `storage` buffers.

## Operations

The only real parts to elaborate are matrix multiplication and broadcasting:

### Matrix Multiplication

TLDR: See distributed matrix multiplication

### Broadcasting

Broadcasting is applying the 1-rank dimension across all inputs, this becomes a problem for any split `Tensor`, as some splits may not have the broadcasted dimension.

The trivial workaround is to store or extract those dimensions for the computation or make the broadcasting transparent to the user and manually create it in the background.
