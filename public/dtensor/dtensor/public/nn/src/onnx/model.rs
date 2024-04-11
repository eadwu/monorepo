#![allow(unused, non_snake_case)]
use core::{num, panic};
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use filemanager::FileManager;
use prost::Message;
use tensor::primitives::tensor::{ConvPadding, Tensor, TensorType};
use tensor::primitives::tensorview::TensorView;

use crate::onnx;

pub struct OnnxModel {
    model: onnx::ModelProto,
    tensors: HashMap<String, Tensor>,
    temporary: Rc<RefCell<HashMap<String, Tensor>>>,
}

impl OnnxModel {
    pub fn from_onnx(onnx_filepath: &Path) -> OnnxModel {
        if !onnx_filepath.exists() {
            log::error!("[ONNX] File at `{:?}` does not exist", onnx_filepath);
        }

        let filecontents = FileManager::new(1).open(onnx_filepath, 0).unwrap();
        let model_proto = onnx::ModelProto::decode(&filecontents[..]).unwrap();

        log::info!("[ONNX] Parsed `{:?}`", onnx_filepath);
        log::info!("[ONNX] IR Version: {}", model_proto.ir_version);

        let graph = model_proto.graph.as_ref().unwrap();
        let tensors = graph
            .initializer
            .iter()
            .map(|tensor_proto| {
                let shape = tensor_proto
                    .dims
                    .iter()
                    .map(|&x| x as i32)
                    .collect::<Vec<_>>();
                let data = tensor_proto.load(Some(onnx_filepath));

                log::debug!(
                    "[ONNX] Found initializer `{}` of shape `{:?}`",
                    &tensor_proto.name,
                    &shape[..]
                );

                let tensor = Tensor::from_contiguous(&data[..], &shape[..]);
                (tensor_proto.name.clone(), tensor)
            })
            .collect::<HashMap<_, _>>();

        if graph.sparse_initializer.len() != 0 {
            log::error!("[ONNX] Detected unused sparse tensors");
        }

        if graph.value_info.len() != 0 {
            log::error!("[ONNX] Detected unused value information");
        }

        // Prune initializers from instance to reduce memory usage
        let mut model_proto = model_proto.clone();
        let mut graph = graph.clone();
        graph.initializer = vec![];
        model_proto.graph = Some(graph);
        // Ensure immutability
        let model_proto = model_proto;

        OnnxModel {
            tensors,
            model: model_proto,
            temporary: Rc::new(RefCell::new(HashMap::new())),
        }
    }
}

impl onnx::runtime::OnnxRuntime for OnnxModel {
    fn convert_graph(&self, inputs: &HashMap<String, Tensor>) -> HashMap<String, Tensor> {
        // Enable inputs to be retrievable in the runtime
        inputs
            .iter()
            .for_each(|(name, tensor)| self.track_tensor(name, tensor.clone()));

        // Virtualize graph in DTensor
        let graph = self.model.graph.as_ref().unwrap();
        for x in &graph.node[..] {
            log::trace!("[ONNX] [{}] {:?} -> {:?}", x.op_type, x.input, x.output);
            self.serialize_node(x);
        }

        // Fetch output Tensors and clear temporary cache
        let output_tensors = graph
            .output
            .iter()
            .map(|value_proto| {
                (
                    value_proto.name.clone(),
                    self.tensor(&value_proto.name).unwrap().clone(),
                )
            })
            .collect::<HashMap<_, _>>();
        self.temporary.borrow_mut().clear();
        output_tensors
    }
}
