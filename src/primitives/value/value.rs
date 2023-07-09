use impl_ops::*;
use std::mem;
use std::ops::{self, Neg};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use num_traits::AsPrimitive;

static ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);


#[derive(Debug, Clone, Copy)]
pub struct InputSpec {
    input: Value,
}

#[derive(Debug, Clone)]
pub struct FunctionSpec {
    f: FunctionType,
    args: Vec<Rc<NodeOperation>>,
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionType {
    Identity,
    Addition,
    Multiplication,
    Power,
}

#[derive(Debug, Clone)]
pub enum Operation {
    Input(InputSpec),
    Function(FunctionSpec),
}

#[derive(Debug, Clone)]
pub struct NodeOperation {
    id: usize,
    op: Operation,
}

#[derive(Debug, Clone, Copy)]
pub struct Value {
    data: f32,
}

impl Value {
    pub fn literal<T: AsPrimitive<f32>>(value: T) -> NodeOperation {
        Value { data: value.as_() }.into()
    }
}

impl Into<NodeOperation> for Value {
    fn into(self) -> NodeOperation {
        NodeOperation::new(Operation::Input(InputSpec { input: self }))
    }
}

// Basic mathematical operations
impl NodeOperation {
    pub fn id(&self) -> usize {
        self.id
    }


    pub fn new(op: Operation) -> NodeOperation {
        NodeOperation {
            id: ID_GENERATOR.fetch_add(1, Ordering::Relaxed),
            op: op,
        }
    }

    pub fn __identity__(self) -> NodeOperation {
        NodeOperation::new(Operation::Function(FunctionSpec {
            f: FunctionType::Identity,
            args: vec![Rc::new(self)],
        }))
    }

    pub fn __pow__(self, power: NodeOperation) -> NodeOperation {
        NodeOperation::new(Operation::Function(FunctionSpec {
            f: FunctionType::Power,
            args: vec![Rc::new(self), Rc::new(power)],
        }))
    }

    // Evaluate
    pub fn evaluate(&self) -> f32 {
        // match &self.op {
        //     Operation::Input(InputSpec { input }) => input.data,
        //     Operation::Function(FunctionSpec { f, args }) => {
        //         let lhs = args.get(0).unwrap();
        //         let rhs = args.get(1).unwrap();

        //         println!("{:p}", &rhs);

        //         match f {
        //             FunctionType::Identity => lhs.evaluate(),
        //             FunctionType::Addition => lhs.evaluate() + rhs.evaluate(),
        //             FunctionType::Multiplication => lhs.evaluate() * rhs.evaluate(),
        //             FunctionType::Power => lhs.evaluate().powf(rhs.evaluate()),
        //             _ => panic!("Unsupported FunctionType {:?}", f)
        //         }
        //     }
        // }
        println!("{:#?}", self.id);
        1.0
    }
}

impl Drop for NodeOperation {
    fn drop(&mut self) {
        match self.op.clone() {
            Operation::Function(mut f) => {
                mem::replace(&mut f.args, vec![]);
            }
            _ => ()
        }
    }
}

impl_op_ex!(+ |lhs: NodeOperation, rhs: NodeOperation| -> NodeOperation {
    NodeOperation::new(Operation::Function(FunctionSpec {
        f: FunctionType::Addition,
        args: vec![Rc::new(lhs), Rc::new(rhs)],
    }))
});

impl_op_ex!(* |lhs: NodeOperation, rhs: NodeOperation| -> NodeOperation {
    NodeOperation::new(Operation::Function(FunctionSpec {
        f: FunctionType::Multiplication,
        args: vec![Rc::new(lhs), Rc::new(rhs)],
    }))
});

// Derivative operators
impl Neg for NodeOperation {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self * Value::literal(-1)
    }
}

impl_op_ex!(- |lhs: NodeOperation, rhs: NodeOperation| -> NodeOperation {
    NodeOperation::new(Operation::Function(FunctionSpec {
        f: FunctionType::Addition,
        args: vec![Rc::new(lhs), Rc::new(-rhs)],
    }))
});

impl_op_ex!(/ |lhs: NodeOperation, rhs: NodeOperation| -> NodeOperation {
    NodeOperation::new(Operation::Function(FunctionSpec{
        f: FunctionType::Multiplication,
        args: vec![Rc::new(lhs), Rc::new(rhs.__pow__(Value::literal(-1)))],
    }))
});
