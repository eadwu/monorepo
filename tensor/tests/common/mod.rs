use dtensor::runtime::webgpu::WebGPUDevice;

pub async fn wgpu_setup() -> Option<WebGPUDevice> {
    // Instantiates instance of WebGPU
    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        // Expose workaround for https://github.com/gfx-rs/wgpu/issues/5390, WGPU_DEBUG=0
        #[cfg(debug_assertions)]
        flags: wgpu::InstanceFlags::default().with_env(),
        ..wgpu::InstanceDescriptor::default()
    });

    // `request_adapter` instantiates the general connection to the GPU
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            ..wgpu::RequestAdapterOptions::default()
        })
        .await?;

    // `request_device` instantiates the feature specific connection to the GPU, defining some parameters,
    //  `features` being the available features.
    let (device, queue) = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: None,
                // Raise limits to maximum
                required_features: adapter.features(),
                required_limits: adapter.limits(),
            },
            None,
        )
        .await
        .unwrap();

    let info = adapter.get_info();
    // skip this on LavaPipe temporarily
    if info.device_type == wgpu::DeviceType::Other {
        return None;
    }

    Some(WebGPUDevice { device, queue })
}
