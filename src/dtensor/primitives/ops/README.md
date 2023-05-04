# Primitive Tensor Operations

All shaders __MUST__ have a generic shader for reference in `shaders/`.

Individual operations that are then actually put in Rust __MUST__ allow for specialized shaders.

Essentially two different shader implementations for each primitive operation being exposed:
1. Fully generic WGSL shader in `shaders/`
2. Personalized WGSL shader if all information needed is provided
- i.e. for a `Linear` layer, given `A ~ M x K` and `B ~ K x N`, if `M`, `K`, and `N` are explicitly given in the parameters, fallback to `(1)` if even just one is missing.

# GeMM

```
// Matrix Multiplication
// A x ... x Z @ Z x K = ABC..Y x Z @ Z x K = ABC..Y x K

// Convolution 2D
// N x C x W x H @ A x B
// Source of Truth - https://scocoyash.github.io/speeding-up-convolutions/
// Optimized FFT nlog(n) - https://stackoverflow.com/questions/18384054/what-are-the-downsides-of-convolution-by-fft-compared-to-realspace-convolution/18385421#18385421

// Misc Reference
// https://spatial-lang.org/gemm
// https://petewarden.com/2015/04/20/why-gemm-is-at-the-heart-of-deep-learning/

// https://pytorch.org/docs/stable/generated/torch.matmul.html
```
