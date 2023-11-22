use std::collections::HashMap;

pub type RuntimeGraph<T> = Vec<T>;

pub trait UniqueIdentifier {
    type Id: std::hash::Hash + std::cmp::Eq + PartialEq;
    fn id(&self) -> Self::Id;
}

pub trait GraphDependencies {
    type Dependency;
    fn dependencies(&self) -> Vec<Self::Dependency>;
}

pub trait GraphView {
    type GraphNode;
    fn linearize(&self) -> RuntimeGraph<Self::GraphNode>;
}

impl<T: UniqueIdentifier + GraphDependencies<Dependency = T> + Clone> GraphView for T {
    type GraphNode = T;
    fn linearize(&self) -> RuntimeGraph<Self::GraphNode> {
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

        graph.into_iter().rev().collect::<Vec<_>>()
    }
}
