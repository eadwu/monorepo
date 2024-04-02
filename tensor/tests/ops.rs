use dtensor::primitives::tensor::Tensor;
use dtensor::primitives::tensorview::TensorView;
use dtensor::runtime::webgpu::WebGPUEvaluation;

mod common;

#[tokio::test]
async fn multiply() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let view = TensorView::from_contiguous_shape(&[4]);
    let a = Tensor::from_contiguous(&[2, 4, 6, 8], &[2, 2]);
    let b = Tensor::from_contiguous(&[3, 3], &[2]).broadcast(&a);
    let a = a.reshape(&view);
    let b = b.reshape(&view);

    let result = a.Multiply(&b).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, i32>(&output), &[6, 12, 18, 24]);
}

#[tokio::test]
async fn broadcast() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let input = Tensor::arange(&[1, 4, 5, 2]);
    let view = TensorView::from_contiguous_shape(&[2, 1, 4, 5, 2]);
    let result = input
        .broadcast_to(&view)
        .Identity()
        .evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 0, 1, 2, 3, 4, 5, 6, 7,
            8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
            30, 31, 32, 33, 34, 35, 36, 37, 38, 39
        ]
    );

    let input = Tensor::from_contiguous(&[1.0, 1.1, 1.2, 2.0, 2.1, 2.2], &[6, 1, 1]);
    let view = TensorView::from_contiguous_shape(&[6, 3, 3]);
    let result = input.broadcast_to(&view).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[
            1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1,
            1.1, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
            2.0, 2.0, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2,
            2.2, 2.2, 2.2
        ]
    );
}

#[tokio::test]
async fn slice() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let input = Tensor::from_contiguous(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], &[2, 4]);
    let result = input
        .Slice(&[1, 0], &[2, 3], &[0, 1], &[1, 2])
        .evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[5.0, 7.0]);

    let result = input
        .Slice(&[0, 1], &[1, 1000], &[0, 1], &[1, 1])
        .evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[2.0, 3.0, 4.0]);
}

#[tokio::test]
async fn argmax() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let input = Tensor::from_contiguous(&[1, 2, 3, 3], &[2, 2]);
    let result = input.ArgMax(1, false, false).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, i32>(&output), &[1, 0]);

    let input = Tensor::arange(&[4, 2, 3, 5]);
    let result = input.ArgMax(1, false, false).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1
        ]
    );

    let result = input.ArgMax(3, true, false).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
    );

    let result = input.ArgMin(3, true, false).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    );
}

#[tokio::test]
async fn offset() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let input = Tensor::arange(&[2, 4, 8]);
    let result = input
        .Offset(&[(1, 0), (1, 1), (2, 3)])
        .evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[42, 43, 44, 50, 51, 52]
    );
}

#[tokio::test]
async fn pad() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let input = Tensor::from_contiguous(&[4, 2, 3, 1], &[2, 2]);
    let result = input.Pad(&[(1, 1), (1, 0)]).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[0, 0, 0, 0, 4, 2, 0, 3, 1, 0, 0, 0]
    );

    let input = Tensor::from_contiguous(&[1.0], &[1]);
    let result = input.Pad(&[(1, 2)]).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[0.0, 1.0, 0.0, 0.0]
    );
}

#[tokio::test]
async fn conv() {
    use dtensor::primitives::tensor::ConvPadding;
    let wgpu_device = common::wgpu_setup().await.unwrap();

    // Valid
    let input = Tensor::arange(&[1, 1, 4, 4]);
    let kernel = Tensor::arange(&[1, 1, 2, 2]);
    let result = input
        .Conv(&kernel, &[1, 1], ConvPadding::Valid)
        .evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[24, 30, 36, 48, 54, 60, 72, 78, 84]
    );

    // Same
    let input = Tensor::arange(&[1, 1, 4, 4]);
    let kernel = Tensor::arange(&[1, 1, 2, 2]);
    let result = input
        .Conv(&kernel, &[1, 1], ConvPadding::Same)
        .evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, i32>(&output),
        &[24, 30, 36, 14, 48, 54, 60, 22, 72, 78, 84, 30, 13, 14, 15, 0]
    );
}

#[tokio::test]
async fn matmul() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let a = Tensor::from_contiguous(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], &[2, 2, 2]);
    let b = Tensor::from_contiguous(&[9.0, 10.0, 11.0, 12.0, 13.0, 14.0], &[2, 3]);
    let result = a.MatMul(&b).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[33.0, 36.0, 39.0, 75.0, 82.0, 89.0, 117.0, 128.0, 139.0, 159.0, 174.0, 189.0]
    );
}

#[tokio::test]
async fn softmax() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let x = Tensor::from_contiguous(&[1.0, 2.0, 3.0], &[1, 3]);
    let result = x.Softmax(1).evaluate_webgpu(&wgpu_device);
    let output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[0.09003057, 0.24472847, 0.66524096]
    );
}

#[tokio::test]
async fn scatter() {
    use dtensor::primitives::tensor::ScatterReduction;
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let mut input: Tensor;
    let mut indices: Tensor;
    let mut updates: Tensor;
    let mut result: Tensor;
    let mut output: Vec<u8>;

    input = Tensor::from_contiguous(&[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], &[3, 3]);
    indices = Tensor::from_contiguous(&[1, 0, 2, 0, 2, 1], &[2, 3]);
    updates = Tensor::from_contiguous(&[1.0, 1.1, 1.2, 2.0, 2.1, 2.2], &[2, 3]);
    result = input
        .ScatterElements(0, ScatterReduction::None, &indices, &updates)
        .evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[2.0, 1.1, 0.0, 1.0, 0.0, 2.2, 0.0, 2.1, 1.2]
    );

    input = Tensor::from_contiguous(&[1.0, 2.0, 3.0, 4.0, 5.0], &[1, 5]);
    indices = Tensor::from_contiguous(&[1, 3], &[1, 2]);
    updates = Tensor::from_contiguous(&[1.1, 2.1], &[1, 2]);
    result = input
        .ScatterElements(1, ScatterReduction::None, &indices, &updates)
        .evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[1.0, 1.1, 3.0, 2.1, 5.0]
    );
}

#[tokio::test]
async fn reduce() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let a = Tensor::from_contiguous(&[1.0, 2.0, 3.0, 4.0], &[2, 2]);
    let mut b: Tensor;
    let mut result: Tensor;
    let mut output: Vec<u8>;

    b = a.Sum(&[], true);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[10.0]);

    b = a.Sum(&[], false);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[10.0]);

    b = a.Sum(&[1], true);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[3.0, 7.0]);

    b = a.Sum(&[1], false);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[3.0, 7.0]);

    b = a.Sum(&[0], true);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[4.0, 6.0]);

    b = a.Sum(&[0], false);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[4.0, 6.0]);

    b = a.Sum(&[0, 1], true);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[10.0]);

    b = a.Sum(&[0, 1], false);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[10.0]);

    b = Tensor::scalar(6128.0).Sum(&[], false);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[6128.0]);

    b = Tensor::scalar(6128.0).Sum(&[], true);
    result = b.evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(bytemuck::cast_slice::<u8, f32>(&output), &[6128.0]);
}

#[tokio::test]
async fn gather() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let mut data: Tensor;
    let mut indices: Tensor;
    let mut result: Tensor;
    let mut output: Vec<u8>;

    data = Tensor::from_contiguous(&[1.0, 1.2, 2.3, 3.4, 4.5, 5.7], &[3, 2]);
    indices = Tensor::from_contiguous(&[0, 0, 1, 1, 1, 1, 2, 2], &[2, 2, 2]);
    result = data
        .GatherElements(0, &indices)
        .evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[1.0, 1.2, 2.3, 3.4, 2.3, 3.4, 4.5, 5.7]
    );

    data = Tensor::from_contiguous(&[1.0, 1.2, 1.9, 2.3, 3.4, 3.9, 4.5, 5.7, 5.9], &[3, 3]);
    indices = Tensor::from_contiguous(&[0, 2, 0, 2, 0, 2], &[3, 1, 2]);
    result = data
        .GatherElements(1, &indices)
        .evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[1.0, 1.9, 2.3, 3.9, 4.5, 5.9]
    );

    data = Tensor::from_contiguous(&[1.0, 2.0, 3.0, 4.0], &[2, 2]);
    indices = Tensor::from_contiguous(&[0, 0, 1, 0], &[2, 2]);
    result = data
        .GatherElements(1, &indices)
        .evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[1.0, 1.0, 4.0, 3.0]
    );

    data = Tensor::from_contiguous(&[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0], &[3, 3]);
    indices = Tensor::from_contiguous(&[1, 2, 0, 2, 0, 0], &[2, 3]);
    result = data
        .GatherElements(0, &indices)
        .evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[4.0, 8.0, 3.0, 7.0, 2.0, 3.0]
    );
}

#[tokio::test]
async fn erf() {
    let wgpu_device = common::wgpu_setup().await.unwrap();

    let mut data: Tensor;
    let mut result: Tensor;
    let mut output: Vec<u8>;

    data = Tensor::from_contiguous(&[1.0, 1.1, 1.2, 1.3], &[4]);
    result = data.Erf().evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[0.15729931, 0.11979495, 0.08968594, 0.06599197]
    );

    data = Tensor::from_contiguous(&[-1.0, -1.1, -1.2, -1.3], &[4]);
    result = data.Erf().evaluate_webgpu(&wgpu_device);
    output = result.load();
    assert_eq!(
        bytemuck::cast_slice::<u8, f32>(&output),
        &[-0.15729931, -0.11979495, -0.08968594, -0.06599197]
    );
}
