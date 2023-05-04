mod dtensor;

use futures::{executor::block_on, join, Future};
use std::{borrow::Cow, str::FromStr};
use wgpu::{util::DeviceExt, Device, RequestAdapterOptions};

const FLOAT32_SIZE: usize = std::mem::size_of::<f32>();

// Indicates a u32 overflow in an intermediate Collatz value
const OVERFLOW: u32 = 0xffffffff;

fn ref_or_value<T: Into<String>>(ffs: T) -> String {
    // fn ref_or_value<'a>(ffs: Cow<'a, u32>) -> u32 {
    ffs.into() + "asdfasd"
}

fn ref_only(ffs: &str) -> String {
    ffs.to_string() + "Asdfasd"
}

fn value_only(ffs: String) -> String {
    ffs + "SADF"
}

async fn run() {
    let ffs: String = String::from("mmm");
    ref_only(&ffs);
    ref_or_value(&ffs);
    ref_or_value(ffs);
    // ref_only(ffs);
    // value_only(&ffs);
    // value_only(ffs);

    execute_gpu().await;
}

async fn execute_gpu() {
    // Instantiates instance of WebGPU
    let instance = wgpu::Instance::default();

    // `request_adapter` instantiates the general connection to the GPU
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions::default())
        .await
        .unwrap();

    // `request_device` instantiates the feature specific connection to the GPU, defining some parameters,
    //  `features` being the available features.
    let (device, queue) = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: None,
                features: wgpu::Features::empty(),
                limits: wgpu::Limits::downlevel_defaults(),
            },
            None,
        )
        .await
        .unwrap();

    let wgpu_device = std::rc::Rc::new(dtensor::WebGPU {
        device: device,
        queue: queue,
    });

    let info = adapter.get_info();
    // skip this on LavaPipe temporarily
    if info.vendor == 0x10005 {
        return;
    }

    // Input data
    let embeddings = dtensor::primitives::Tensor::new::<f32>(
        &vec![10, 2],
        &vec![
            1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0,
            17.0, 18.0, 19.0, 20.0,
        ],
        &wgpu_device,
    )
    .await;

    let embedding_lookup =
        dtensor::primitives::Tensor::new::<f32>(&vec![4], &vec![4.0, 0.0, 1.0, 9.0], &wgpu_device)
            .await;

    let left_tensor = dtensor::primitives::ops::embedding_lookup(&embeddings, &embedding_lookup).await;

    println!("{:?}", left_tensor.snapshot().await);

    let linear = dtensor::layers::Linear::new(2, 4, false, &wgpu_device).await;
    let mut layernorm = dtensor::layers::LayerNorm::new(&[4, 4], 1e-5, true, &wgpu_device).await;
    let gelu = dtensor::layers::activations::GeLU::new().await;

    let broadcast_tensor =
        dtensor::primitives::Tensor::new::<f32>(&vec![2], &vec![2.0, -2.0], &wgpu_device).await;

    // println!("{:?}", (left_tensor + broadcast_tensor).snapshot().await);
    println!(
        "{:?}",
        gelu.forward(&layernorm.forward(&linear.forward(&left_tensor).await).await)
            .await
            // layernorm.forward(linear.forward(left_tensor).await).await
            .snapshot()
            .await
    );
    // let sum = broadcast_tensor.reshape(&vec![2, 2, 2]).await;
    // let sum = dtensor::primitives::ops::clamp(&left_tensor, 2.0, 4.0).await;
    // let sum = left_tensor.as_contiguous().await;
    // let sum = dtensor::layers::activations::GeLU.forward(&left_tensor).await;
}

async fn retrieve_device_from_adapter(
    adapter: wgpu::Adapter,
) -> (wgpu::Adapter, wgpu::Device, wgpu::Queue) {
    let (device, queue) = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: None,
                features: wgpu::Features::empty(),
                limits: wgpu::Limits::downlevel_defaults(),
            },
            None,
        )
        .await
        .unwrap();

    (adapter, device, queue)
}

async fn enumerate_devices(
    webgpu: &wgpu::Instance,
) -> impl Iterator<Item = impl futures::Future<Output = (wgpu::Adapter, wgpu::Device, wgpu::Queue)>>
{
    webgpu
        .enumerate_adapters(wgpu::Backends::all())
        .map(retrieve_device_from_adapter)
}

fn main() {
    let instance = wgpu::Instance::default();

    /*
    let low_power = wgpu::RequestAdapterOptions{
        power_preference: wgpu::PowerPreference::HighPerformance,
        ..wgpu::RequestAdapterOptions::default()
    };

    let high_power = wgpu::RequestAdapterOptions{
        power_preference: wgpu::PowerPreference::LowPower,
        ..wgpu::RequestAdapterOptions::default()
    };

    let software = wgpu::RequestAdapterOptions{
        force_fallback_adapter: true,
        ..wgpu::RequestAdapterOptions::default()
    };

    let arr: [&RequestAdapterOptions; 3] = [&high_power, &low_power, &software];
    for adapter in arr {
        let device = block_on(instance.request_adapter(adapter));
        if device.is_some() {
            println!("{:?}", device.unwrap().get_info());
        }
    }
    */

    /*
    for (index, device) in block_on(enumerate_devices(&instance)).enumerate() {
        let (adapter, device, queue) = block_on(device);
        println!("{} {:?} {:?}", index, adapter.get_info(), device.features())
    }
    */

    #[cfg(not(target_arch = "wasm32"))]
    {
        env_logger::init();
        pollster::block_on(run());
    }
    #[cfg(target_arch = "wasm32")]
    {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_log::init().expect("could not initialize logger");
        wasm_bindgen_futures::spawn_local(run());
    }
}
