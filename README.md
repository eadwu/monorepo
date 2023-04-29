# Future Plan
- Broadcasting
  - Thinking about a clean implementation for the frontend, shader computation is simple

- Auto-differentiation
  - Main point of this reimplementation is to provide a graph anyway

- Allow distributed computation for base operations
  - Allows bypassing WebGPU limits

- Distinct names for every basic operation
  - Will allow aggregating into a single shader

- Expand to support all WebGPU numeric types
  - Restricted to float32 for now

# Differences
- Internal data is not mutable
  - Possible Solutions
    - Start as `MAP_WRITE | COPY_SRC` and then move to `MAP_STORAGE | COPY_DST | COPY_SRC`?
      - Provide `exec()` and `map()` APIs like `train()`, `eval()`
    - Map over to `MAP_STORAGE | COPY_DST | COPY_SRC` before every operation?
      - Provide incurs excessive overhead
  - __TODO__ Provide some API that allows a snapshot read-only, `MAP_READ | COPY_DST`, view when requested
    - Possibly another API returning a multidimensional array for easy indexing
  - `Tensor::new()` provides a constructor for arbitrary data which suffices for all cases
    - More of a workaround but accounts for all cases
  - Neural Network are initialized from saved weights or oneshot initializer functions
    - Not a priority to provide arbitary adjustments to internal data
