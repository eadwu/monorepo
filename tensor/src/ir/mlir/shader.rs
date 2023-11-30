use crate::primitives::tensor::*;
use crate::primitives::tensorview::{TensorView, TensorViewTracker};

use crate::ir::mlir::*;

trait SerializeShaderIR {
    fn shader_ir(&self, index_ir: &ShaderIR) -> ShaderIR;
}

trait UnrollShaderIR {
    fn unroll_ir(&self, metadata: &ShaderIRIndex, ir_type: ShaderIRType) -> Vec<VirtualShaderIR>;
}

#[derive(Clone, Debug)]
struct ShaderIROperation {
    datatype: ShaderIRType,
    op: ShaderIREvaluation,
}

#[derive(Clone, Debug)]
struct ShaderIRIndex {
    index: ShaderIR,
    tensor: Tensor,
}

#[derive(Clone, Debug)]
struct ShaderIRReduction {
    reduce_begin: ShaderIR,
    reduction: ShaderIREvaluation,
}

#[derive(Clone, Debug)]
enum VirtualShaderIR {
    ReduceEnd(ShaderIRReduction),
    Operator(ShaderIROperation),
    TranslatedIndex(ShaderIRIndex),
    Value(ShaderIR),
}

impl VirtualShaderIR {
    pub fn op(datatype: ShaderIRType, op: ShaderIREvaluation) -> VirtualShaderIR {
        VirtualShaderIR::Operator(ShaderIROperation { datatype, op })
    }

    pub fn index(index: ShaderIR, tensor: Tensor) -> VirtualShaderIR {
        VirtualShaderIR::TranslatedIndex(ShaderIRIndex { index, tensor })
    }

    pub fn value(ir: ShaderIR) -> VirtualShaderIR {
        VirtualShaderIR::Value(ir)
    }

    pub fn reduce(reduce_begin: ShaderIR, reduction: ShaderIREvaluation) -> VirtualShaderIR {
        VirtualShaderIR::ReduceEnd(ShaderIRReduction {
            reduce_begin,
            reduction,
        })
    }
}

impl ShaderIRBuilder for Tensor {
    fn build_shader_ir(&self) -> ShaderIR {
        // Magic initializers
        let magic_index = ShaderIR::new(ShaderIROp::MagicIndex, ShaderIRType::I32, &[], None);
        let magic_metadata = VirtualShaderIR::index(magic_index, self.clone());

        // Compute indices using two-level queue, constructed via postfix (reverse-polish notation)
        let mut inputs = Vec::new();
        let mut queue = Vec::from([magic_metadata]);
        while let Some(metadata) = queue.pop() {
            match metadata {
                VirtualShaderIR::Operator(operation) => {
                    let args = (0..operation.op.n_dependencies())
                        .map(|_| inputs.pop().unwrap())
                        .collect::<Vec<_>>();

                    let result = ShaderIR::new(
                        ShaderIROp::Evaluate,
                        operation.datatype,
                        &args[..],
                        Some(operation.op),
                    );
                    inputs.push(result);
                }
                VirtualShaderIR::TranslatedIndex(ref index_ir) => {
                    let ir_type = Into::<ShaderIRType>::into(index_ir.tensor.datatype());
                    let mut unrolled_ir = index_ir.tensor.unroll_ir(&index_ir, ir_type);
                    queue.append(&mut unrolled_ir);
                }
                VirtualShaderIR::Value(value) => inputs.push(value),
                VirtualShaderIR::ReduceEnd(reduce) => {
                    let reduce_value = inputs.pop().unwrap();
                    let result = ShaderIR::new(
                        ShaderIROp::ReduceEnd,
                        reduce_value.datatype(),
                        &[reduce.reduce_begin, reduce_value],
                        Some(reduce.reduction),
                    );

                    inputs.push(result);
                }
            }
        }

        assert!(inputs.len() == 1);
        let final_index = inputs.pop().unwrap();
        ShaderIR::new(
            ShaderIROp::Store,
            Into::<ShaderIRType>::into(self.datatype()),
            &[final_index],
            Some(ShaderIREvaluation::I32(self.id() as i32)),
        )
    }
}

impl Into<ShaderIRType> for TensorType {
    fn into(self) -> ShaderIRType {
        match &self {
            TensorType::F32 => ShaderIRType::F32,
            TensorType::I32 => ShaderIRType::I32,
            _ => panic!("{:?} is unsupported as ShaderIRType", self),
        }
    }
}

impl Into<ShaderIREvaluation> for UnaryType {
    fn into(self) -> ShaderIREvaluation {
        match &self {
            UnaryType::ABS => ShaderIREvaluation::ABS,
            UnaryType::CAST => ShaderIREvaluation::CAST,
            UnaryType::CEIL => ShaderIREvaluation::CEIL,
            UnaryType::EXP2 => ShaderIREvaluation::EXP2,
            UnaryType::FLOOR => ShaderIREvaluation::FLOOR,
            UnaryType::IDENTITY => ShaderIREvaluation::IDENTITY,
            UnaryType::LOG2 => ShaderIREvaluation::LOG2,
            UnaryType::SIN => ShaderIREvaluation::SIN,
            UnaryType::SQRT => ShaderIREvaluation::SQRT,
        }
    }
}

impl Into<ShaderIREvaluation> for BinaryType {
    fn into(self) -> ShaderIREvaluation {
        match &self {
            BinaryType::ADD => ShaderIREvaluation::ADD,
            BinaryType::DIVIDE => ShaderIREvaluation::DIVIDE,
            BinaryType::EQUAL => ShaderIREvaluation::EQUAL,
            BinaryType::LESSTHAN => ShaderIREvaluation::LESSTHAN,
            BinaryType::MAX => ShaderIREvaluation::MAX,
            BinaryType::MOD => ShaderIREvaluation::MOD,
            BinaryType::MULTIPLY => ShaderIREvaluation::MULTIPLY,
            BinaryType::SUB => ShaderIREvaluation::SUB,
        }
    }
}

impl Into<ShaderIREvaluation> for ReduceType {
    fn into(self) -> ShaderIREvaluation {
        match &self {
            ReduceType::MAX => ShaderIREvaluation::MAX,
            ReduceType::SUM => ShaderIREvaluation::ADD,
        }
    }
}

impl UnrollShaderIR for Tensor {
    fn unroll_ir(&self, index_ir: &ShaderIRIndex, ir_type: ShaderIRType) -> Vec<VirtualShaderIR> {
        match self.data() {
            TensorInput::ExplicitInput(spec) => vec![match spec {
                InputSpec::Scalar(_) => {
                    let bytes = self.load();
                    match ir_type {
                        ShaderIRType::F32 => VirtualShaderIR::value(ShaderIR::new(
                            ShaderIROp::Const,
                            ir_type,
                            &[],
                            Some(ShaderIREvaluation::F32(f32::from_le_bytes(
                                bytes.try_into().unwrap(),
                            ))),
                        )),
                        ShaderIRType::I32 => VirtualShaderIR::value(ShaderIR::new(
                            ShaderIROp::Const,
                            ir_type,
                            &[],
                            Some(ShaderIREvaluation::I32(i32::from_le_bytes(
                                bytes.try_into().unwrap(),
                            ))),
                        )),
                    }
                }
                InputSpec::Range(spec) => {
                    let start_ir = ShaderIR::new(
                        ShaderIROp::Const,
                        ShaderIRType::I32,
                        &[],
                        Some(ShaderIREvaluation::I32(spec.start as i32)),
                    );
                    let step_ir = ShaderIR::new(
                        ShaderIROp::Const,
                        ShaderIRType::I32,
                        &[],
                        Some(ShaderIREvaluation::I32(spec.step as i32)),
                    );

                    let sequence_ir = ShaderIR::new(
                        ShaderIROp::Evaluate,
                        ShaderIRType::I32,
                        &[step_ir, index_ir.index.clone()],
                        Some(ShaderIREvaluation::MULTIPLY),
                    );
                    let offset_ir = ShaderIR::new(
                        ShaderIROp::Evaluate,
                        ShaderIRType::I32,
                        &[sequence_ir, start_ir],
                        Some(ShaderIREvaluation::ADD),
                    );

                    VirtualShaderIR::value(ShaderIR::new(
                        ShaderIROp::Evaluate,
                        ir_type,
                        &[offset_ir.clone()],
                        Some(ShaderIREvaluation::CAST),
                    ))
                }
                InputSpec::Internal(_) | InputSpec::Safetensor(_) => {
                    VirtualShaderIR::value(ShaderIR::new(
                        ShaderIROp::Load,
                        ir_type,
                        &[index_ir.index.clone()],
                        Some(ShaderIREvaluation::I32(self.id() as i32)),
                    ))
                }
            }],
            TensorInput::NoOp(tensor) => vec![VirtualShaderIR::index(
                index_ir.index.clone(),
                tensor.clone(),
            )],
            TensorInput::OperationResult(result) => result.unroll_ir(index_ir, ir_type),
            _ => panic!("Unsupported ShaderIR generation for TensorInput"),
        }
    }
}

impl UnrollShaderIR for OperationSpec {
    fn unroll_ir(&self, inputs: &ShaderIRIndex, ir_type: ShaderIRType) -> Vec<VirtualShaderIR> {
        match self {
            OperationSpec::BinaryOp(spec) => spec.unroll_ir(inputs, ir_type),
            OperationSpec::ReduceOp(spec) => spec.unroll_ir(inputs, ir_type),
            OperationSpec::UnaryOp(spec) => spec.unroll_ir(inputs, ir_type),
        }
    }
}

impl UnrollShaderIR for UnarySpec {
    fn unroll_ir(&self, index_ir: &ShaderIRIndex, ir_type: ShaderIRType) -> Vec<VirtualShaderIR> {
        let index_ir = &index_ir.index;
        let evaltype = Into::<ShaderIREvaluation>::into(self.op);
        vec![
            VirtualShaderIR::op(ir_type, evaltype),
            VirtualShaderIR::index(
                self.input.viewtracker().shader_ir(index_ir),
                self.input.clone(),
            ),
        ]
    }
}

impl UnrollShaderIR for BinarySpec {
    fn unroll_ir(&self, index_ir: &ShaderIRIndex, ir_type: ShaderIRType) -> Vec<VirtualShaderIR> {
        let index_ir = &index_ir.index;
        let evaltype = Into::<ShaderIREvaluation>::into(self.op);
        vec![
            VirtualShaderIR::op(ir_type, evaltype),
            VirtualShaderIR::index(self.lhs.viewtracker().shader_ir(index_ir), self.lhs.clone()),
            VirtualShaderIR::index(self.rhs.viewtracker().shader_ir(index_ir), self.rhs.clone()),
        ]
    }
}

impl UnrollShaderIR for ReduceSpec {
    fn unroll_ir(&self, index_ir: &ShaderIRIndex, ir_type: ShaderIRType) -> Vec<VirtualShaderIR> {
        let index_ir = &index_ir.index;
        let evaltype = Into::<ShaderIREvaluation>::into(self.op);

        let input_view = self.input.view();

        let mut output_shape = input_view.shape.to_vec();
        for &axis in &self.axes {
            output_shape[axis as usize] = 1;
        }
        let output_view = TensorView::from_contiguous_shape(&output_shape[..]);

        // IR to normalize the index to 0 at AXIS
        let normalized_mapper = Into::<TensorViewTracker>::into(TensorView::as_defined(
            false,
            output_view.shape.clone(),
            input_view.stride.clone(),
            output_view.contiguous_stride.clone(),
        ));

        let reduce_shape = self
            .axes
            .iter()
            .map(|&axis| input_view.shape[axis as usize])
            .collect::<Vec<_>>();
        let reduce_strides = self
            .axes
            .iter()
            .map(|&axis| input_view.stride[axis as usize])
            .collect::<Vec<_>>();
        let reduce_contiguous_strides = TensorView::compute_contiguous_stride(&reduce_shape[..]);
        let iteration_mapper = Into::<TensorViewTracker>::into(TensorView::as_defined(
            false,
            reduce_shape.into_boxed_slice(),
            reduce_strides.into_boxed_slice(),
            reduce_contiguous_strides.into_boxed_slice(),
        ));
        let reduce_iterations = iteration_mapper.len();

        let normalized_ir = normalized_mapper.shader_ir(index_ir);
        let reduce_begin = ShaderIR::new(
            ShaderIROp::ReduceBegin,
            ir_type,
            &[normalized_ir.clone()], // ballmark of reduce region
            Some(ShaderIREvaluation::I32(reduce_iterations as i32)),
        );
        let adjusted_stride_ir = iteration_mapper.shader_ir(&reduce_begin);
        let loop_index_ir = ShaderIR::new(
            ShaderIROp::Evaluate,
            ShaderIRType::I32,
            &[normalized_ir.clone(), adjusted_stride_ir],
            Some(ShaderIREvaluation::ADD),
        );

        vec![
            VirtualShaderIR::reduce(reduce_begin, evaltype),
            VirtualShaderIR::index(
                self.input.viewtracker().shader_ir(&loop_index_ir),
                self.input.clone(),
            ),
        ]
    }
}

impl SerializeShaderIR for TensorViewTracker {
    fn shader_ir(&self, index_ir: &ShaderIR) -> ShaderIR {
        let zero = ShaderIR::new(
            ShaderIROp::Const,
            ShaderIRType::I32,
            &[],
            Some(ShaderIREvaluation::I32(0)),
        );

        self.serialized_history_fifo()
            .iter()
            .fold(index_ir.clone(), |previous_index_ir, view| {
                view.shape
                    .iter()
                    .zip(view.stride.iter().zip(view.contiguous_stride.iter()))
                    .fold(
                        zero.clone(),
                        |partial_index, (&shape, (&stride, &contiguous_stride))| {
                            let shape_ir = ShaderIR::new(
                                ShaderIROp::Const,
                                ShaderIRType::I32,
                                &[],
                                Some(ShaderIREvaluation::I32(shape as i32)),
                            );
                            let stride_ir = ShaderIR::new(
                                ShaderIROp::Const,
                                ShaderIRType::I32,
                                &[],
                                Some(ShaderIREvaluation::I32(stride as i32)),
                            );
                            let contiguous_stride_ir = ShaderIR::new(
                                ShaderIROp::Const,
                                ShaderIRType::I32,
                                &[],
                                Some(ShaderIREvaluation::I32(contiguous_stride as i32)),
                            );
                            // index / contiguous_stride % shape * stride
                            let x0 = ShaderIR::new(
                                ShaderIROp::Evaluate,
                                ShaderIRType::I32,
                                &[previous_index_ir.clone(), contiguous_stride_ir],
                                Some(ShaderIREvaluation::DIVIDE),
                            );
                            let x1 = ShaderIR::new(
                                ShaderIROp::Evaluate,
                                ShaderIRType::I32,
                                &[x0, shape_ir],
                                Some(ShaderIREvaluation::MOD),
                            );
                            let x2 = ShaderIR::new(
                                ShaderIROp::Evaluate,
                                ShaderIRType::I32,
                                &[x1, stride_ir],
                                Some(ShaderIREvaluation::MULTIPLY),
                            );

                            ShaderIR::new(
                                ShaderIROp::Evaluate,
                                ShaderIRType::I32,
                                &[partial_index, x2],
                                Some(ShaderIREvaluation::ADD),
                            )
                        },
                    )
            })
    }
}
