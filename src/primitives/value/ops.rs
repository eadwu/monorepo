// #[derive(Debug, Clone, Copy)]
// pub struct InputSpec {
//     input: Value,
// }

// #[derive(Debug, Clone)]
// pub struct FunctionSpec {
//     f: FunctionType,
//     args: Vec<Rc<NodeOperation>>,
// }

// #[derive(Debug, Clone, Copy)]
// pub enum FunctionType {
//     Identity,
//     Addition,
//     Multiplication,
//     Power,
// }

// #[derive(Debug, Clone)]
// pub enum Operation {
//     Input(InputSpec),
//     Function(FunctionSpec),
// }

pub enum Operation {
  UnaryOp(),
  BinaryOp(),
  ReduceOp(),
  MovementOp(),
}

pub enum UnaryType {
  IDENTITY,
  EXP2,
  LOG2,
  SIN,
}

pub struct UnarySpec {
  f: UnaryType,
  // input: Any,
}

pub enum BinaryType {
  ADD,
  SUBTRACT,
  MULTIPLY,
  POW,
}
