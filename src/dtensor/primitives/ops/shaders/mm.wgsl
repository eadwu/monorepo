// PyTorch [Dense] Tensors are row-major
// All N-dimensional matrix multiplications can be reduced to 2D
struct Shape2D {
  shape: vec2f,
  stride: vec2f,
}

struct Tensor {
  data: array<f32>,
}

// A [bind] group represents the actual I/O for the shader
// The [bind] group layout represents the interface for the [bind] group
// It is a veiled abstraction layer to allow reusing boilerplate

// i.e. Each bind group represents a Matrix, either read or read_write
//      To make it more obvious, `@binding(1)` can be `TensorShape` instead of inlined

// https://www.khronos.org/opengl/wiki/Uniform_Buffer_Object

// Bind group `@group(0)` defines ro [input] matrix
@group(0) @binding(0) var<storage, read> left: Tensor; // M x K
@group(0) @binding(1) var<storage, read> leftShape: Shape2D;

// Bind group `@group(1)` defines ro [input] matrix
@group(1) @binding(0) var<storage, read> right: Tensor; // K x N
@group(1) @binding(1) var<storage, read> rightShape: Shape2D;

// Bind group `@group(2)` is the rw [output] matrix
@group(2) @binding(0) var<storage, read_write> product: Tensor; // M x N

// Essentially an annotated version of
//   https://developer.chrome.com/articles/gpu-compute/#compute-shader-code
fn naive(global_id: vec3u) {
  // Reference shape
  let productShape = vec2<f32>(leftShape.shape.x, rightShape.shape.y);

  // Guard against out-of-bounds work group sizes
  // By definition workgroup grid is >= 0 so there is no need to check that
  if (global_id.x >= u32(productShape.x) || global_id.y >= u32(productShape.y)) {
    return;
  }

  // Get the current position in the matrix, essentially the M[i][j] timestep
  // `let` is fixed at runtime, `const` is fixed at compile-time, `var` is mutable
  let gridPosition = vec2(global_id.x, global_id.y);

  // This should be straightforward, the classic linear algebra solution
  var result: f32 = 0.0;
  // Every element [i][j] is the row times the column, not going in depth
  // Should be obvious
  //
  // a b  x  e f  =  (ae+bg) (af+bh)
  // c d     g h  =  (ce+dg) (cf+dh)
  for (var i = 0u; i < u32(firstMatrix.size.y); i = i + 1u) {
    // Underlying assumption here is row-major matrix storage
    // Every row of the matrix is stored before the next row is included
    // The shape of a matrix is M x N, M rows and N columns
    //
    // a b  =  a b c d
    // c d
    //
    // Column-major is `a c b d`

    // To account for memory-efficient broadcasting, different "views" of a
    // Tensor are allowed, which are categorized by the `stride`
    //
    // So given M ~ 3x2, it has a stride of 2x1, i.e.
    //   0 1
    //   2 3
    //   4 5
    // It is represented in row-major order as `0 1 2 3 4 5` and if indexed as
    // a contiguous array will be M[i][j] = M[i*2 + j*1]
    //   M[1][0] = M[1*2 + 0*1] = M[2+0] = M[2]
    //   M[2][1] = M[2*2 + 1*1] = M[4+1] = M[5]
    //
    // Now transpose the matrix, M.T = N ~ 2x3, it has a stride of 1x2 relative to M
    //   0 2 4
    //   1 3 5
    //   M[0][2] = M[0*1 + 2*2] = M[0+4] = M[4]
    //   M[1][1] = M[1*1 + 1*2] = M[1+2] = M[3]

    // Strides here should always be relative to the 1D array, i.e. if is
    // contingous, a 3x2x2 will have a 4x2x1 stride

    // Now what about broadcasting? Broadcasting virtually extends a rank that
    // has only one dimension, so the stride can just be 0 for that rank
    // Given M ~ 1x2x1 and N ~ 2x1x2, M@N = 2x2x2, every M[1][j][k] index will
    //   always be constrained to the 2x1 dimensions

    // This loop calculates the element for [i][j]
    // For left this will be M[x][j]
    let leftRowElement = gridPosition.x*leftShape.stride.x + i*leftShape.stride.y;
    // For right this will be M[j][x]
    let rightColumnElement = i*rightShape.stride.x + gridPosition.y*rightShape.stride.y;

    // One element of the summation sequence for M[i][j]
    result = result + left.data[a]*right.data[b];
  }

  // Determine index in the resultant matrix where this should belong
  // No need to account for different strides since the output is new and shiny
  let index: u32 = gridPosition.x * u32(right.size.y) + gridPosition.y;
  product.data[index] = result;
}

// With the knowledge of how modern (2023) computer architectures stand, it should
// be clear the loop in the naive function is not cache-friendly which has runtime
// implications
//   https://docs.nvidia.com/deeplearning/performance/dl-performance-matrix-multiplication/index.html
//   https://sites.cs.ucsb.edu/~tyang/class/240a17/slides/Cache3.pdf
//   https://en.wikipedia.org/wiki/Cache-oblivious_algorithm

// compute - Declares the function to be an entry point for the compute shader stage of a compute pipeline.
// workground_size - Specifies the x, y, and z dimensions of the workgroup grid for the compute shader.
// workground_size ~ tiles or is it how much are run in parallel each time (batch-size)?
// global_invocation_id - The current invocation's global invocation ID, i.e. its position in the compute shader grid.
@compute @workgroup_size(8, 8, 1)
fn main(@builtin(global_invocation_id) global_id: vec3u) {
  naive(global_id)
}

// Extra resources
//   https://spatial-lang.org/gemm
