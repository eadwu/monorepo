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

            if let TensorInput::OperationResult(operation) = current.data() {
                if let OperationSpec::UnaryOp(op) = operation {
                    queue.push(op.input.clone());
                } else if let OperationSpec::BinaryOp(op) = operation {
                    queue.push(op.lhs.clone());
                    queue.push(op.rhs.clone());
                } else if let OperationSpec::ViewOp(op) = operation {
                    queue.push(op.input.clone());
                } else if let OperationSpec::IndexOp(op) = operation {
                    for (_, tensor) in &op.dependencies {
                        queue.push(tensor.clone());
                    }
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
            if let TensorInput::OperationResult(operation) = current.data() {
                if let OperationSpec::UnaryOp(op) = operation {
                    parents.push(op.input.clone());
                } else if let OperationSpec::BinaryOp(op) = operation {
                    parents.push(op.lhs.clone());
                    parents.push(op.rhs.clone());
                } else if let OperationSpec::ViewOp(op) = operation {
                    parents.push(op.input.clone());
                } else if let OperationSpec::IndexOp(op) = operation {
                    for (_, tensor) in &op.dependencies {
                        parents.push(tensor.clone());
                    }
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
            if let TensorInput::OperationResult(operation) = tensor.data() {
                if let OperationSpec::UnaryOp(op) = operation {
                    dependencies.insert(op.input.id(), tensor.id());
                } else if let OperationSpec::BinaryOp(op) = operation {
                    dependencies.insert(op.lhs.id(), tensor.id());
                    dependencies.insert(op.rhs.id(), tensor.id());
                } else if let OperationSpec::ViewOp(op) = operation {
                    dependencies.insert(op.input.id(), tensor.id());
                } else if let OperationSpec::IndexOp(op) = operation {
                    for (_, dependency) in &op.dependencies {
                        dependencies.insert(dependency.id(), tensor.id());
                    }
                }
            }
        }

        RuntimeGraph::new(dependencies, graph)
    }
}
