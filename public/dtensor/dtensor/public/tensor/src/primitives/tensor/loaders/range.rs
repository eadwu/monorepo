use crate::primitives::tensor::{Tensor, RangeSpec};

pub trait RangeLoader {
    fn load<T: bytemuck::Pod>(self, spec: &RangeSpec) -> Vec<T>;
}

impl RangeLoader for &Tensor {
    fn load<T: bytemuck::Pod>(self, spec: &RangeSpec) -> Vec<T> {
        let data = (spec.start..spec.end)
            .step_by(spec.step as usize)
            .map(TryInto::<i32>::try_into)
            .map(Result::unwrap)
            .collect::<Vec<_>>();
        bytemuck::cast_slice(&data[..]).to_vec()
    }
}
