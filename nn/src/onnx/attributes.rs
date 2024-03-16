use std::collections::HashMap;

use crate::onnx::{
    AttributeProto, GraphProto, NodeProto, SparseTensorProto, TensorProto, TypeProto,
};

impl NodeProto {
    pub fn attributes(&self) -> HashMap<String, AttributeProto> {
        self.attribute
            .iter()
            .map(|attribute_proto| (attribute_proto.name.clone(), attribute_proto.clone()))
            .collect::<HashMap<_, _>>()
    }
}

impl AttributeProto {
    pub fn value<'a, T: Clone + From<&'a AttributeProto>>(&'a self) -> T {
        <&AttributeProto as Into<T>>::into(self).clone()
    }
}

impl From<&AttributeProto> for bool {
    fn from(value: &AttributeProto) -> Self {
        value.i != 0
    }
}

impl From<&AttributeProto> for f32 {
    fn from(value: &AttributeProto) -> Self {
        value.f
    }
}

impl From<&AttributeProto> for i32 {
    fn from(value: &AttributeProto) -> Self {
        value.i as i32
    }
}

impl From<&AttributeProto> for u32 {
    fn from(value: &AttributeProto) -> Self {
        value.i as u32
    }
}

impl From<&AttributeProto> for i64 {
    fn from(value: &AttributeProto) -> Self {
        value.i
    }
}

impl From<&AttributeProto> for String {
    fn from(value: &AttributeProto) -> Self {
        String::from_utf8_lossy(&value.s[..]).to_string()
    }
}

impl From<&AttributeProto> for TensorProto {
    fn from(value: &AttributeProto) -> Self {
        value.t.clone().unwrap()
    }
}

impl From<&AttributeProto> for GraphProto {
    fn from(value: &AttributeProto) -> Self {
        value.g.clone().unwrap()
    }
}

impl From<&AttributeProto> for SparseTensorProto {
    fn from(value: &AttributeProto) -> Self {
        value.sparse_tensor.clone().unwrap()
    }
}

impl From<&AttributeProto> for TypeProto {
    fn from(value: &AttributeProto) -> Self {
        value.tp.clone().unwrap()
    }
}

impl From<&AttributeProto> for Vec<f32> {
    fn from(value: &AttributeProto) -> Self {
        value.floats.clone()
    }
}

impl From<&AttributeProto> for Vec<i64> {
    fn from(value: &AttributeProto) -> Self {
        value.ints.clone()
    }
}

impl From<&AttributeProto> for Vec<String> {
    fn from(value: &AttributeProto) -> Self {
        value
            .strings
            .iter()
            .map(|s| String::from_utf8_lossy(s).to_string())
            .collect::<Vec<_>>()
    }
}

impl From<&AttributeProto> for Vec<TensorProto> {
    fn from(value: &AttributeProto) -> Self {
        value.tensors.clone()
    }
}

impl From<&AttributeProto> for Vec<GraphProto> {
    fn from(value: &AttributeProto) -> Self {
        value.graphs.clone()
    }
}

impl From<&AttributeProto> for Vec<SparseTensorProto> {
    fn from(value: &AttributeProto) -> Self {
        value.sparse_tensors.clone()
    }
}

impl From<&AttributeProto> for Vec<TypeProto> {
    fn from(value: &AttributeProto) -> Self {
        value.type_protos.clone()
    }
}
