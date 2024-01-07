use crate::topograph::GraphView;
use crate::ir::mlir::{ShaderIR, ShaderIREvaluation, ShaderIROp, ShaderIRType};

impl ShaderIR {
    pub fn gen_wgsl(&self) -> String {
        self.linearize()
            .into_iter()
            .map(|ir| match &ir.0.op {
                ShaderIROp::Const => match &ir.0.evaltype {
                    Some(ShaderIREvaluation::F32(float)) => {
                        format!("let {} = {}f;", ir.variable(), float)
                    }
                    Some(ShaderIREvaluation::I32(integer)) => {
                        format!("let {} = {}i;", ir.variable(), integer)
                    }
                    _ => panic!(),
                },
                ShaderIROp::Evaluate => match ir.0.evaltype.as_ref().unwrap() {
                    ShaderIREvaluation::IDENTITY => {
                        format!("let {} = {};", ir.variable(), ir.inputs()[0].variable())
                    }
                    ShaderIREvaluation::EXP2 => format!(
                        "let {} = exp2({});",
                        ir.variable(),
                        ir.inputs()[0].variable()
                    ),
                    ShaderIREvaluation::LOG2 => format!(
                        "let {} = log2({});",
                        ir.variable(),
                        ir.inputs()[0].variable()
                    ),
                    ShaderIREvaluation::CAST => format!(
                        "let {} = {}({});",
                        ir.variable(),
                        match ir.datatype() {
                            ShaderIRType::F32 => "f32",
                            ShaderIRType::I32 => "i32",
                        },
                        ir.inputs()[0].variable()
                    ),
                    ShaderIREvaluation::SIN => {
                        format!(
                            "let {} = sin({});",
                            ir.variable(),
                            ir.inputs()[0].variable()
                        )
                    }
                    ShaderIREvaluation::SQRT => format!(
                        "let {} = sqrt({});",
                        ir.variable(),
                        ir.inputs()[0].variable()
                    ),
                    ShaderIREvaluation::ABS => {
                        format!(
                            "let {} = abs({});",
                            ir.variable(),
                            ir.inputs()[0].variable()
                        )
                    }
                    ShaderIREvaluation::FLOOR => format!(
                        "let {} = floor({});",
                        ir.variable(),
                        ir.inputs()[0].variable()
                    ),
                    ShaderIREvaluation::CEIL => format!(
                        "let {} = ceil({});",
                        ir.variable(),
                        ir.inputs()[0].variable()
                    ),
                    ShaderIREvaluation::ADD => format!(
                        "let {} = {} + {};",
                        ir.variable(),
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    ShaderIREvaluation::SUB => format!(
                        "let {} = {} - {};",
                        ir.variable(),
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    ShaderIREvaluation::MULTIPLY => format!(
                        "let {} = {} * {};",
                        ir.variable(),
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    ShaderIREvaluation::DIVIDE => format!(
                        "let {} = {} / {};",
                        ir.variable(),
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    ShaderIREvaluation::MAX => format!(
                        "let {} = max({}, {});",
                        ir.variable(),
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    ShaderIREvaluation::MOD => format!(
                        "let {} = {} % {};",
                        ir.variable(),
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    ShaderIREvaluation::EQUAL => format!(
                        "let {} = {}({} == {});",
                        ir.variable(),
                        match ir.datatype() {
                            ShaderIRType::F32 => "f32",
                            ShaderIRType::I32 => "i32",
                        },
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    ShaderIREvaluation::LESSTHAN => format!(
                        "let {} = {} < {};",
                        ir.variable(),
                        ir.inputs()[0].variable(),
                        ir.inputs()[1].variable()
                    ),
                    _ => panic!(),
                },
                ShaderIROp::Load => format!(
                    "let {} = tensor_{}[{}];",
                    ir.variable(),
                    match ir.0.evaltype {
                        Some(ShaderIREvaluation::I32(id)) => id,
                        _ => panic!(),
                    },
                    ir.inputs()[0].variable(),
                ),
                ShaderIROp::MagicIndex => format!("let {} = i32(index);", ir.variable()),
                ShaderIROp::ReduceBegin => {
                    format!(
                        "var {var_name}_acc: {acc_type};
for (var {var_name} = 0i; {var_name} < {length}i; {var_name}++) {{",
                        acc_type = match ir.0.datatype {
                            ShaderIRType::F32 => "f32",
                            ShaderIRType::I32 => "i32",
                        },
                        var_name = ir.variable(),
                        length = match ir.0.evaltype {
                            Some(ShaderIREvaluation::I32(integer)) => integer,
                            _ => panic!(),
                        },
                    )
                }
                ShaderIROp::ReduceEnd => format!(
                    "if {reduce} == 0i {{
{reduce}_acc = {element};
}} else {{
{reduce}_acc = {expression};
}}
}}
let {self_name} = {reduce}_acc;",
                    self_name = ir.variable(),
                    reduce = ir.inputs()[0].variable(),
                    element = ir.inputs()[1].variable(),
                    expression = match &ir.0.evaltype {
                        Some(ShaderIREvaluation::ADD) => format!(
                            "{}_acc + {}",
                            ir.inputs()[0].variable(),
                            ir.inputs()[1].variable()
                        ),
                        Some(ShaderIREvaluation::MAX) => format!(
                            "max({}_acc, {})",
                            ir.inputs()[0].variable(),
                            ir.inputs()[1].variable()
                        ),
                        _ => panic!(),
                    }
                ),
                ShaderIROp::Store => format!(
                    "tensor_{tensor_id}[index] = {value};",
                    tensor_id = match ir.0.evaltype {
                        Some(ShaderIREvaluation::I32(id)) => id,
                        _ => panic!(),
                    },
                    value = ir.inputs()[0].variable()
                ),
                _ => panic!(),
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}
