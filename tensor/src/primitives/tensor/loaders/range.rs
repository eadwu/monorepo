use crate::primitives::tensor::{Tensor, RangeSpec};

pub trait RangeLoader {
    fn load(self, spec: &RangeSpec) -> Vec<u8>;
}

impl RangeLoader for &Tensor {
    fn load(self, spec: &RangeSpec) -> Vec<u8> {
        let data = (spec.start..spec.end)
            .step_by(spec.step as usize)
            .map(TryInto::<i32>::try_into)
            .map(Result::unwrap)
            .collect::<Vec<_>>();
        bytemuck::cast_slice(&data[..]).to_vec()
    }
}
