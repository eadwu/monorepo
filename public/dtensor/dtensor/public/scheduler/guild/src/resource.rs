use super::{DeviceBrand, DeviceType, Region};

pub trait Requirement {
    fn satisfies(&self, other: &Self) -> bool;
}

impl Requirement for u64 {
    fn satisfies(&self, other: &Self) -> bool {
        self >= other
    }
}

impl Requirement for DeviceType {
    fn satisfies(&self, other: &Self) -> bool {
        match other {
            DeviceType::Any => true,
            _ => self == other,
        }
    }
}

impl Requirement for DeviceBrand {
    fn satisfies(&self, other: &Self) -> bool {
        match other {
            DeviceBrand::Any => true,
            _ => self == other,
        }
    }
}

impl Requirement for Region {
    fn satisfies(&self, other: &Self) -> bool {
        match other {
            Region::Any => true,
            _ => self == other,
        }
    }
}
