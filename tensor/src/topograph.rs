use std::collections::HashMap;

use crate::primitives::tensor::{GraphDependencies, OperationSpec, Tensor, TensorInput};

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

            queue.append(&mut current.dependencies());
        }

        // Traverse each node until all paths are accounted for
        let mut graph = vec![];
        let mut queue = vec![self.clone()];
        while !queue.is_empty() {
            let current = queue.pop().unwrap();
            graph.push(current.clone());

            let parents = current.dependencies();
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
        let graph = graph.into_iter().rev().collect::<Vec<_>>();
        let dependencies = graph
            .iter()
            .flat_map(|tensor| {
                tensor
                    .dependencies()
                    .iter()
                    .map(|input| (input.id(), tensor.id()))
                    .collect::<Vec<_>>()
            })
            .collect::<HashMap<_, _>>();

        RuntimeGraph::new(dependencies, graph)
    }
}
