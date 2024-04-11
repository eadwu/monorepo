use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tensor::primitives::tensor::{ConvPadding, Tensor, TensorType};
use tensor::primitives::tensorview::TensorView;

use crate::onnx;

pub struct OpsetV21 {
    lookup: Rc<RefCell<HashMap<String, Tensor>>>,
}

impl onnx::runtime::OnnxRuntime for OpsetV21 {
    fn tensor(&self, proto_name: &str) -> Option<Tensor> {
        log::trace!("[ONNX] Fetching Tensor {}", proto_name);
        self.lookup
            .borrow()
            .get(proto_name)
            .map(|tensor| tensor.clone())
    }

    fn track_tensor(&self, name: &str, tensor: Tensor) {
        log::trace!("[ONNX] Tracking Tensor {}", name);
        self.lookup.borrow_mut().insert(name.to_string(), tensor);
    }

    fn Abs(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Abs]");

        let [X, Y] = &node_proto.io_interface()[..] else {
            panic!("Abs expects [X] -> [Y]");
        };

        let X_tensor = self.tensor(X).unwrap();
        let Y_tensor = X_tensor.Abs();
        self.track_tensor(Y, Y_tensor);
    }

    fn Add(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Add]");

        let [A, B, C] = &node_proto.io_interface()[..] else {
            panic!("Add expects [A] [B] -> [C]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = A_tensor.Add(&B_tensor);
        self.track_tensor(C, C_tensor);
    }

    fn And(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [And]");

        let [A, B, C] = &node_proto.io_interface()[..] else {
            panic!("And expects [A] [B] -> [C]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = A_tensor.And(&B_tensor);
        self.track_tensor(C, C_tensor);
    }

    fn Ceil(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Ceil]");

        let [X, Y] = &node_proto.io_interface()[..] else {
            panic!("Ceil expects [X] -> [Y]");
        };

        let X_tensor = self.tensor(X).unwrap();
        let Y_tensor = X_tensor.Ceil();
        self.track_tensor(Y, Y_tensor);
    }

    fn Clip(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Clip]");

        let [input, min, max, output] = &node_proto.io_interface()[..] else {
            panic!("Clip expects [input] [min] [max] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let min_tensor = self.tensor(min);
        let max_tensor = self.tensor(max);

        let output_tensor =
            min_tensor.map_or(input_tensor.clone(), |min| input_tensor.Minimum(&min));
        let output_tensor =
            max_tensor.map_or(output_tensor.clone(), |max| output_tensor.Maximum(&max));
        self.track_tensor(output, output_tensor)
    }

    fn Cos(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Cos]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Cos expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Cos();
        self.track_tensor(output, output_tensor);
    }

    fn Div(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Div]");

        let [dividend, divisor, output] = &node_proto.io_interface()[..] else {
            panic!("Div expects [A] [B] -> [C]")
        };

        let dividend_tensor = self.tensor(dividend).unwrap();
        let divisor_tensor = self.tensor(divisor).unwrap();
        let output_tensor = dividend_tensor.Divide(&divisor_tensor);
        self.track_tensor(output, output_tensor);
    }

    fn Equal(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Equal]");

        let [lhs, rhs, output] = &node_proto.io_interface()[..] else {
            panic!("Equal expects [A] [B] -> [C]")
        };

        let lhs_tensor = self.tensor(lhs).unwrap();
        let rhs_tensor = self.tensor(rhs).unwrap();
        let output_tensor = lhs_tensor.Equal(&rhs_tensor);
        self.track_tensor(output, output_tensor);
    }

    fn Erf(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Erf]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Erf expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Erf();
        self.track_tensor(output, output_tensor);
    }

    fn Exp(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Exp]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Exp expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Exp();
        self.track_tensor(output, output_tensor);
    }

    fn Floor(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Floor]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Floor expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Floor();
        self.track_tensor(output, output_tensor);
    }

    fn Greater(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Greater]");

        let [A, B, C] = &node_proto.io_interface()[..] else {
            panic!("Greater expects [A] [B] -> [C]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = A_tensor.Greater(&B_tensor);
        self.track_tensor(C, C_tensor);
    }

    fn GreaterOrEqual(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [GreaterOrEqual]");

        let [A, B, C] = &node_proto.io_interface()[..] else {
            panic!("GreaterOrEqual expects [A] [B] -> [C]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = A_tensor.GreaterOrEqual(&B_tensor);
        self.track_tensor(C, C_tensor);
    }

    fn Identity(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Identity]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Identity expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Identity();
        self.track_tensor(output, output_tensor);
    }

    fn Less(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Less]");

        let [A, B, C] = &node_proto.io_interface()[..] else {
            panic!("Less expects [A] [B] -> [C]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = A_tensor.LessThan(&B_tensor);
        self.track_tensor(C, C_tensor);
    }

    fn LessOrEqual(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [LessOrEqual]");

        let [A, B, C] = &node_proto.io_interface()[..] else {
            panic!("LessOrEqual expects [A] [B] -> [C]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = A_tensor.LessOrEqual(&B_tensor);
        self.track_tensor(C, C_tensor);
    }

    fn Log(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Log]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Log expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Log();
        self.track_tensor(output, output_tensor);
    }

    fn Mul(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Mul]");

        let [lhs, rhs, output] = &node_proto.io_interface()[..] else {
            panic!("Mul expects [lhs] [rhs] -> [output]")
        };

        let lhs_tensor = self.tensor(lhs).unwrap();
        let rhs_tensor = self.tensor(rhs).unwrap();
        let output_tensor = lhs_tensor.Multiply(&rhs_tensor);
        self.track_tensor(output, output_tensor);
    }

    fn Neg(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Neg]");

        let [X, Y] = &node_proto.io_interface()[..] else {
            panic!("Neg expects [X] -> [Y]")
        };

        let X_tensor = self.tensor(X).unwrap();
        let Y_tensor = X_tensor.Neg();
        self.track_tensor(Y, Y_tensor);
    }

    fn Not(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Not]");

        let [X, Y] = &node_proto.io_interface()[..] else {
            panic!("Not expects [X] -> [Y]")
        };

        let X_tensor = self.tensor(X).unwrap();
        let Y_tensor = X_tensor.Not();
        self.track_tensor(Y, Y_tensor);
    }

    fn Or(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Or]");

        let [A, B, C] = &node_proto.io_interface()[..] else {
            panic!("Or expects [A] [B] -> [C]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = A_tensor.Or(&B_tensor);
        self.track_tensor(C, C_tensor);
    }

    fn Pow(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Pow]");

        let [base, power, output] = &node_proto.io_interface()[..] else {
            panic!("Pow expects [base] [power] -> [output]")
        };

        let base_tensor = self.tensor(base).unwrap();
        let power_tensor = self.tensor(power).unwrap();
        let output_tensor = base_tensor.Pow(&power_tensor);
        self.track_tensor(output, output_tensor);
    }

    fn Sigmoid(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Sigmoid]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Sigmoid expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Sigmoid();
        self.track_tensor(output, output_tensor);
    }

    fn Sin(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Sin]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Sin expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Sin();
        self.track_tensor(output, output_tensor);
    }

    fn Sqrt(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Sqrt]");

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Sqrt expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Sqrt();
        self.track_tensor(output, output_tensor);
    }

    fn Sub(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Sub]");

        let [minuend, subtrahend, output] = &node_proto.io_interface()[..] else {
            panic!("Sub expects [minuend] [subtrahend] -> [output]")
        };

        let minuend_tensor = self.tensor(minuend).unwrap();
        let subtrahend_tensor = self.tensor(subtrahend).unwrap();
        let output_tensor = minuend_tensor.Sub(&subtrahend_tensor);
        self.track_tensor(output, output_tensor);
    }
}
