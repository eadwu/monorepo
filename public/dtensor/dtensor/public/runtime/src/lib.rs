use std::collections::HashMap;

use tensor::primitives::tensor::{OperationSpec, Tensor, TensorInput};

pub mod webgpu;

pub struct RuntimeGraph {
    pub dependencies: HashMap<u32, u32>,
    pub graph: Vec<Tensor>,
}

impl RuntimeGraph {
    pub fn new(dependencies: HashMap<u32, u32>, graph: Vec<Tensor>) -> RuntimeGraph {
        RuntimeGraph {
            dependencies,
            graph,
        }
    }
}

pub trait GraphView {
    fn as_runtime_graph(&self) -> RuntimeGraph;
}

impl GraphView for Tensor {
    fn as_runtime_graph(&self) -> RuntimeGraph {
        // Count how many paths reach every node
        let mut node_counts = HashMap::new();
        let mut queue = vec![self.clone()];

        while !queue.is_empty() {
            let current = queue.pop().unwrap();
            if !node_counts.contains_key(&current.id()) {
                node_counts.insert(current.id(), 1);
            } else {
                node_counts.insert(current.id(), node_counts.get(&current.id()).unwrap() + 1);
                continue;
            }

            if let TensorInput::NoOp(input) = current.data() {
                queue.push(input.clone());
            }

            if let TensorInput::OperationResult(operation) = current.data() {
                match operation {
                    OperationSpec::UnaryOp(op) => queue.push(op.input.clone()),
                    OperationSpec::BinaryOp(op) => {
                        queue.push(op.lhs.clone());
                        queue.push(op.rhs.clone());
                    }
                    OperationSpec::ReduceOp(op) => queue.push(op.input.clone()),
                }
            }
        }

        // Traverse each node until all paths are accounted for
        let mut graph = vec![];
        let mut queue = vec![self.clone()];
        while !queue.is_empty() {
            let current = queue.pop().unwrap();
            graph.push(current.clone());

            let mut parents = vec![];
            if let TensorInput::NoOp(input) = current.data() {
                parents.push(input.clone());
            }

            if let TensorInput::OperationResult(operation) = current.data() {
                match operation {
                    OperationSpec::UnaryOp(op) => parents.push(op.input.clone()),
                    OperationSpec::BinaryOp(op) => {
                        parents.push(op.lhs.clone());
                        parents.push(op.rhs.clone());
                    }
                    OperationSpec::ReduceOp(op) => parents.push(op.input.clone())
                }
            };

            for parent in parents {
                let count = node_counts.get(&parent.id()).unwrap().clone();
                if count == 1 {
                    queue.push(parent);
                } else {
                    node_counts.insert(parent.id(), count - 1);
                }
            }
        }

        // Get latest dependency (lifetime) for every Tensor in graph
        let graph: Vec<Tensor> = graph.into_iter().rev().collect();
        let mut dependencies = HashMap::new();
        for tensor in &graph {
            if let TensorInput::NoOp(input) = tensor.data() {
                dependencies.insert(input.id(), tensor.id());
            }

            if let TensorInput::OperationResult(operation) = tensor.data() {
                match operation {
                    OperationSpec::UnaryOp(op) => {
                        dependencies.insert(op.input.id(), tensor.id());
                    }
                    OperationSpec::BinaryOp(op) => {
                        dependencies.insert(op.lhs.id(), tensor.id());
                        dependencies.insert(op.rhs.id(), tensor.id());
                        }
                    OperationSpec::ReduceOp(op) => {
                        dependencies.insert(op.input.id(), tensor.id());
                    }
                }
            }
        }

        RuntimeGraph::new(dependencies, graph)
    }
}
