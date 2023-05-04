# Primitives
All the underlying primitives used for inference here, and what the underlying auto-differentiable engine is built on top of.

There is __no__ sparse tensor support right now. All tensor are expected to be dense.

- https://math.stackexchange.com/questions/94590/matrix-multiplication-for-n-dimensional-arrays
- https://numpy.org/doc/stable/user/basics.broadcasting.html#general-broadcasting-rules
- https://docs.nvidia.com/deeplearning/performance/dl-performance-fully-connected/index.html

# Workgroups
What the fuck? How does this abstract to anything above 3D

The only possible way this _should_ be possible is if everything is squashed down to 3 dimensions.

- https://surma.dev/things/webgpu/

# Parallel Contexts
- https://github.com/gpuweb/gpuweb/wiki/The-Multi-Explainer
