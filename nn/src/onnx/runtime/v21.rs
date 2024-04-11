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

    fn ArgMax(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let axis = attributes.get("axis").map(Into::<i64>::into).unwrap_or(1);
        let keepdims = attributes
            .get("keepdims")
            .map(Into::<bool>::into)
            .unwrap_or(true);
        let select_last_index = attributes
            .get("select_last_index")
            .map(Into::<bool>::into)
            .unwrap_or(false);

        log::trace!(
            "[ONNX] [ArgMax] [axis={}] [keepdims={}] [select_last_index={}]",
            axis,
            keepdims,
            select_last_index
        );

        let [data, reduced] = &node_proto.io_interface()[..] else {
            panic!("ArgMax expects [data] -> [reduced]")
        };

        let data_tensor = self.tensor(data).unwrap();
        let axis = ((axis as i32) + data_tensor.ndim()) % data_tensor.ndim();

        let reduced_tensor = data_tensor.ArgMax(axis, keepdims, select_last_index);
        self.track_tensor(reduced, reduced_tensor);
    }

    fn Cast(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let saturate = attributes
            .get("saturate")
            .map(Into::<bool>::into)
            .unwrap_or(true);
        let to = attributes
            .get("to")
            .map(Into::<i32>::into)
            .map(|type_int| onnx::tensor_proto::DataType::try_from(type_int).unwrap())
            .unwrap();

        log::trace!("[ONNX] [Cast] [saturate={}] [to={:?}]", saturate, to);

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Cast expects [input] -> [output]")
        };

        let tensor_type = match to {
            onnx::tensor_proto::DataType::Float => TensorType::F32,
            onnx::tensor_proto::DataType::Uint8 => TensorType::U32,
            onnx::tensor_proto::DataType::Int8 => TensorType::I32,
            onnx::tensor_proto::DataType::Uint16 => TensorType::U32,
            onnx::tensor_proto::DataType::Int16 => TensorType::I32,
            onnx::tensor_proto::DataType::Int32 => TensorType::I32,
            // onnx::tensor_proto::DataType::Int64 => (),
            // onnx::tensor_proto::DataType::String => (),
            onnx::tensor_proto::DataType::Bool => TensorType::U32,
            onnx::tensor_proto::DataType::Float16 => TensorType::F32,
            // onnx::tensor_proto::DataType::Double => (),
            onnx::tensor_proto::DataType::Uint32 => TensorType::U32,
            // onnx::tensor_proto::DataType::Uint64 => (),
            // onnx::tensor_proto::DataType::Complex64 => (),
            // onnx::tensor_proto::DataType::Complex128 => (),
            // onnx::tensor_proto::DataType::Bfloat16 => (),
            // onnx::tensor_proto::DataType::Float8e4m3fn => (),
            // onnx::tensor_proto::DataType::Float8e4m3fnuz => (),
            // onnx::tensor_proto::DataType::Float8e5m2 => (),
            // onnx::tensor_proto::DataType::Float8e5m2fnuz => (),
            onnx::tensor_proto::DataType::Uint4 => TensorType::U32,
            onnx::tensor_proto::DataType::Int4 => TensorType::I32,
            _ => panic!("Unable to convert to requested type"),
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = input_tensor.Cast(tensor_type);
        self.track_tensor(output, output_tensor);
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

    fn Concat(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let axis = attributes.get("axis").map(Into::<i64>::into).unwrap();
        log::trace!("[ONNX] [Concat] [axis={}]", axis);

        let [inputs @ .., concat_result] = &node_proto.io_interface()[..] else {
            panic!("Concat expects [inputs]... -> [concat_result]")
        };

        let reference_tensor = self.tensor(inputs.first().unwrap()).unwrap();
        let reference_shape = &reference_tensor.shape();
        let reference_dimension = reference_tensor.ndim() as i64;
        let input_tensors = inputs
            .iter()
            .map(|input_name| self.tensor(&input_name).unwrap())
            .collect::<Vec<_>>();

        let axis = ((axis + reference_dimension) % reference_dimension) as usize;

        // Ensure all shapes are compatible
        log::trace!("[ONNX] [Concat] Reference Shape: {:?}", reference_shape);
        input_tensors.iter().for_each(|tensor| {
            let shape = &tensor.shape();
            assert!(
                tensor.ndim() as i64 == reference_dimension,
                "Tensors must have the same shape, besides on the dimension of concatenation"
            );

            reference_shape
                .iter()
                .zip(shape.iter())
                .enumerate()
                .for_each(|(dimension, (&reference, &shape))| {
                    log::trace!("[Concat] Tensor Shape: {:?}", tensor.shape());

                    assert!(
                        dimension == axis || reference == shape,
                        "All dimensions should match besides the axis of concatenation"
                    );
                })
        });

        // TODO: Assumed correct
        let concat_result_tensor = input_tensors
            .into_iter()
            .reduce(|acc, tensor| {
                let current_axis_length = acc.shape()[axis];
                let tensor_axis_length = tensor.shape()[axis];

                let current_padding = acc
                    .view()
                    .shape
                    .iter()
                    .enumerate()
                    .map(|(dimension, _)| {
                        if dimension == axis {
                            (0, tensor_axis_length)
                        } else {
                            (0, 0)
                        }
                    })
                    .collect::<Vec<_>>();
                let tensor_padding = tensor
                    .view()
                    .shape
                    .iter()
                    .enumerate()
                    .map(|(dimension, _)| {
                        if dimension == axis {
                            (current_axis_length, 0)
                        } else {
                            (0, 0)
                        }
                    })
                    .collect::<Vec<_>>();

                let acc = acc.Pad(&current_padding[..]);
                let tensor = tensor.Pad(&tensor_padding[..]);

                acc.Add(&tensor)
            })
            .unwrap();
        self.track_tensor(concat_result, concat_result_tensor);
    }

    // TODO: CHECK
    fn Constant(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        // let sparse_value =
        //     attributes.get("sparse_value").map(Into::<onnx::SparseTensorProto>::into).unwrap();
        let value = attributes
            .get("value")
            .map(Into::<onnx::TensorProto>::into)
            .unwrap();
        // let value_float = attributes.get("value_float").map(Into::<f32>::into).unwrap();
        // let value_floats = attributes.get("value_floats").map(Into::<Vec<f32>>::into).unwrap();
        // let value_int = attributes.get("value_int").map(Into::<i64>::into).unwrap();
        // let value_ints = attributes.get("value_ints").map(Into::<Vec<i64>>::into).unwrap();
        // let value_string = attributes.get("value_string").map(Into::<String>::into).unwrap();
        // let value_strings = attributes.get("value_strings").map(Into::<Vec<String>>::into).unwrap();

        let [output] = &node_proto.io_interface()[..] else {
            panic!("Constant expects -> [output]")
        };

        let shape = value.dims.iter().map(|&x| x as i32).collect::<Vec<_>>();
        let data = value.load(None);

        let output_tensor = Tensor::from_contiguous(&data[..], &shape[..]);
        self.track_tensor(output, output_tensor);
    }

    // TODO: CHECK
    fn ConstantOfShape(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let value = attributes
            .get("value")
            .map(Into::<onnx::TensorProto>::into)
            .map(|tensor_proto| tensor_proto.load(None))
            .unwrap_or(vec![0.0]);

        assert!(
            value.len() == 1,
            "ConstantOfShape expects value to be of length 1, was {}, {:?}",
            value.len(),
            value
        );
        let value = value[0];
        log::trace!("[ONNX] [ConstantOfShape] [value={}]", value);

        let [input_shape, output] = &node_proto.io_interface()[..] else {
            panic!("ConstantOfShape expects [input_shape] -> [output]")
        };

        let input_shape_tensor = self.tensor(input_shape).unwrap();
        let input_shape = input_shape_tensor.load::<f32>();
        let input_shape = input_shape
            .into_iter()
            .map(|x| x as i32)
            .collect::<Vec<_>>();

        let output_tensor = Tensor::from_contiguous(&[value], &input_shape[..]);
        self.track_tensor(output, output_tensor);
    }

    // TODO: CHECK
    fn Conv(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let auto_pad = attributes
            .get("auto_pad")
            .map(Into::<String>::into)
            .unwrap_or("NOTSET".to_string());
        let dilations = attributes.get("dilations").map(Into::<Vec<i64>>::into);
        let group = attributes.get("group").map(Into::<i64>::into).unwrap_or(1);
        let kernel_shape = attributes.get("kernel_shape").map(Into::<Vec<i64>>::into);
        let pads = attributes.get("pads").map(Into::<Vec<i64>>::into);
        let strides = attributes.get("strides").map(Into::<Vec<i64>>::into);

        let [X, W, B @ ..] = &node_proto.input[..] else {
            panic!("Conv expects [X] [W] [B] -> [Y]")
        };
        let [Y] = &node_proto.output[..] else {
            panic!("Conv expects [X] [W] [B] -> [Y]")
        };

        let X_tensor = self.tensor(X).unwrap();
        let W_tensor = self.tensor(W).unwrap();
        let B_tensor = self.tensor(B.first().unwrap()).unwrap_or(Tensor::scalar(0));

        // NOTE: Hardcoded for this implementation
        let dilations = dilations.unwrap_or(vec![1]);
        let kernel_shape = W_tensor.shape().to_vec();
        let pads = pads.unwrap_or(
            (0..X_tensor.ndim())
                .flat_map(|_| [0, 0].into_iter())
                .collect::<Vec<_>>(),
        );

        // Only dilations of 1 are supported here
        dilations
            .iter()
            .for_each(|&dilation| assert!(dilation == 1));
        // Only group of size 1 is supported here
        assert!(group == 1);

        let strides = strides
            .unwrap_or(
                (0..X_tensor.ndim())
                    .into_iter()
                    .map(|_| 1)
                    .collect::<Vec<_>>(),
            )
            .iter()
            .map(|&stride| stride as i32)
            .collect::<Vec<_>>();

        let (padding_pre, padding_post) = pads.split_at(pads.len() / 2);

        assert!(
            padding_pre.len() == padding_post.len(),
            "Invalid padding length split - {} != {}",
            padding_pre.len(),
            padding_post.len()
        );

        // Padding is expected to be defined for every dimension, ONNX may only provide padding for features
        let padding = padding_pre
            .iter()
            .zip(padding_post.iter())
            .map(|(&pre, &post)| (pre as i32, post as i32))
            .collect::<Vec<_>>();
        let padding = if padding.len() != X_tensor.ndim() as usize {
            let missing_padding = X_tensor.ndim() as usize - padding.len();
            let extra_padding = (0..missing_padding)
                .into_iter()
                .map(|_| (0, 0))
                .collect::<Vec<_>>();
            extra_padding
                .into_iter()
                .chain(padding.into_iter())
                .collect::<Vec<_>>()
        } else {
            padding
        };

        let conv_padding = match auto_pad.as_str() {
            "SAME_UPPER" => ConvPadding::SameUpper,
            "SAME_LOWER" => ConvPadding::SameLower,
            "VALID" => ConvPadding::Valid,
            "NOTSET" => ConvPadding::Custom(&padding[..]),
            _ => unreachable!("Invalid Conv padding scheme, got {}", auto_pad),
        };

        log::trace!(
        "[ONNX] [Conv] [auto_pad={}] [dilations={:?}] [group={}] [kernel_shape={:?}] [pads={:?}] [strides={:?}]",
        &auto_pad,
        dilations,
        group,
        kernel_shape,
        pads,
        strides,
    );

        let Y_tensor = X_tensor
        .Conv(&W_tensor, &strides[..], conv_padding)
        // .Add(&B_tensor)
        ;
        self.track_tensor(Y, Y_tensor);
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

    fn Expand(&self, node_proto: &onnx::NodeProto) {
        log::trace!("[ONNX] [Expand]");

        let [input, shape, output] = &node_proto.io_interface()[..] else {
            panic!("Expand expects [input] [shape] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let shape_tensor = self.tensor(shape).unwrap();
        let shape_bytes = shape_tensor.load();
        let broadcast_shape = bytemuck::cast_slice::<u8, f32>(&shape_bytes[..])
            .iter()
            .map(|&x: &f32| x as i32)
            .collect::<Vec<_>>();

        let output_tensor =
            input_tensor.broadcast_to(&TensorView::from_contiguous_shape(&broadcast_shape[..]));
        self.track_tensor(output, output_tensor);
    }

    fn Flatten(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let axis = attributes.get("axis").map(Into::<i64>::into).unwrap_or(1);
        log::trace!("[ONNX] [Flatten] [axis={}]", axis);

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Flatten expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let input_dimension = input_tensor.ndim() as i64;

        let axis = ((axis + input_dimension) % input_dimension) as usize;
        let (d1, d2) = input_tensor.shape().split_at(axis);
        let output_shape = [
            d1.iter().fold(1, |acc, &x| acc * x),
            d2.iter().fold(1, |acc, &x| acc * x),
        ];

        // Important for Tensor to be contiguous so that the internal view is correct
        let output_tensor = input_tensor
            .contiguous()
            .reshape(&TensorView::from_contiguous_shape(&output_shape[..]));
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

    fn Gather(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let axis = attributes.get("axis").map(Into::<i64>::into).unwrap_or(0);
        log::trace!("[ONNX] [Gather] [axis={}]", axis);

        let [data, indices, output] = &node_proto.io_interface()[..] else {
            panic!("Gather expects [data] [indices] -> [output]")
        };

        let data_tensor = self.tensor(data).unwrap();
        let indices_tensor = self.tensor(indices).unwrap();

        let data_dimension = data_tensor.ndim() as i64;
        let axis = ((axis + data_dimension) % data_dimension) as i32;

        let output_tensor = data_tensor.Gather(axis, &indices_tensor);
        self.track_tensor(output, output_tensor);
    }

    fn Gemm(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let alpha = attributes
            .get("alpha")
            .map(Into::<f32>::into)
            .unwrap_or(1.0);
        let beta = attributes.get("beta").map(Into::<f32>::into).unwrap_or(1.0);
        let transA = attributes
            .get("transA")
            .map(Into::<bool>::into)
            .unwrap_or(false);
        let transB = attributes
            .get("transB")
            .map(Into::<bool>::into)
            .unwrap_or(false);
        log::trace!(
            "[ONNX] [Gemm] [alpha={}] [beta={}] [transA={}] [transB={}]",
            alpha,
            beta,
            transA,
            transB
        );

        let [A, B, C @ ..] = &node_proto.input[..] else {
            panic!("Gemm expects [A] [B] [C] -> [Y]")
        };
        let [Y] = &node_proto.output[..] else {
            panic!("Gemm expects [A] [B] [C] -> [Y]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let C_tensor = self.tensor(C.first().unwrap()).unwrap_or(Tensor::scalar(0));

        let A_tensor = if transA {
            A_tensor.transpose(&[])
        } else {
            A_tensor
        };
        let B_tensor = if transB {
            B_tensor.transpose(&[])
        } else {
            B_tensor
        };

        let alpha = Tensor::scalar(alpha);
        let beta = Tensor::scalar(beta);
        let Y_tensor = alpha
            .Multiply(&A_tensor.MatMul(&B_tensor))
            .Add(&beta.Multiply(&C_tensor));
        self.track_tensor(Y, Y_tensor);
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

    fn InstanceNormalization(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let epsilon = attributes
            .get("epsilon")
            .map(Into::<f32>::into)
            .unwrap_or(1e-05);
        log::trace!("[ONNX] [InstanceNormalization] [epsilon={}]", epsilon);

        let [input, scale, B, output] = &node_proto.io_interface()[..] else {
            panic!("InstanceNormalization expects [input] [scale] [B] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let scale_tensor = self.tensor(scale).unwrap();
        let B_tensor = self.tensor(B).unwrap();

        assert!(
            input_tensor.ndim() == 4,
            "InstanceNormalization is only defined for 4D inputs of shape NCHW"
        );

        let epsilon_tensor = Tensor::scalar(epsilon);
        let output_tensor = scale_tensor
            .Multiply(&input_tensor.InstanceNormalization(&epsilon_tensor))
            .Add(&B_tensor);
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

    fn MatMul(&self, node_proto: &onnx::NodeProto) {
        let [A, B, Y] = &node_proto.io_interface()[..] else {
            panic!("MatMul expects [A] [B] -> [Y]")
        };

        let A_tensor = self.tensor(A).unwrap();
        let B_tensor = self.tensor(B).unwrap();
        let Y_tensor = A_tensor.MatMul(&B_tensor);
        self.track_tensor(Y, Y_tensor);
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

    fn Pad(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let mode = attributes
            .get("mode")
            .map(Into::<String>::into)
            .unwrap_or("constant".to_string());
        log::trace!("[ONNX] [Pad] [mode={}]", &mode);

        assert!(
            mode == "constant",
            "Pad does not support non-`constant` mode"
        );

        let [data, pads, optional_inputs @ .., output] = &node_proto.io_interface()[..] else {
            panic!("Pad expects [data] [pads] [constant_value] [axes] -> [output]")
        };

        let data_tensor = self.tensor(data).unwrap();
        let pads_tensor = self.tensor(pads).unwrap();
        let constant_value_tensor = self
            .tensor(optional_inputs.first().unwrap())
            .unwrap_or(Tensor::scalar(0));

        let pads = pads_tensor.load::<f32>();

        let data_dimension = data_tensor.ndim();
        let axes = optional_inputs
            .get(1)
            .map(|axes| self.tensor(axes))
            .flatten()
            .map(|tensor| tensor.load::<f32>().to_vec())
            .unwrap_or((0..data_dimension).into_iter().map(|x| x as f32).collect());
        let axes = axes
            .into_iter()
            .map(|x| ((x + (data_dimension as f32)) / (data_dimension as f32)) as i32)
            .collect::<Vec<_>>();

        let axes_padding_lookup = axes
            .iter()
            .map(|&i| {
                let pads_index = (i as usize) * 2;
                let padding_begin = pads[pads_index];
                let padding_end = pads[pads_index + 1];
                (i, (padding_begin, padding_end))
            })
            .collect::<HashMap<_, _>>();
        let axes_padding_all = (0..data_dimension)
            .into_iter()
            .map(|axis| {
                axes_padding_lookup
                    .get(&axis)
                    .map_or((0.0, 0.0), |&axis_padding| axis_padding)
            })
            .collect::<Vec<_>>();

        // Offset would be negative padding
        let axes_offset = axes_padding_all
            .iter()
            .map(|&(padding_begin, padding_end)| {
                (
                    padding_begin.min(0.0).abs() as i32,
                    padding_end.min(0.0).abs() as i32,
                )
            })
            .collect::<Vec<_>>();
        let axes_padding = axes_padding_all
            .iter()
            .map(|&(padding_begin, padding_end)| {
                (padding_begin.max(0.0) as i32, padding_end.max(0.0) as i32)
            })
            .collect::<Vec<_>>();

        // let output_tensor = data_tensor
        //     .reshape(&data_tensor.view().offset(&axes_offset[..]))
        //     .contiguous()
        //     .Pad(&axes_padding[..]);
        // self.track_tensor(output, output_tensor);
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

    fn RandomNormalLike(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let dtype = attributes.get("dtype").map(Into::<i64>::into);
        let mean = attributes.get("mean").map(Into::<f32>::into).unwrap_or(0.0);
        let scale = attributes
            .get("scale")
            .map(Into::<f32>::into)
            .unwrap_or(1.0);
        let seed = attributes.get("seed").map(Into::<f32>::into);
        log::trace!(
            "[ONNX] [RandomNormalLike] [mean={}] [scale={}] [seed=]",
            mean,
            scale
        );

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("RandomNormalLike expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();
        let output_tensor = Tensor::randn(&input_tensor.shape(), Some(mean), Some(scale));
        self.track_tensor(output, output_tensor);
    }

    fn Range(&self, node_proto: &onnx::NodeProto) {
        let [start, limit, delta, output] = &node_proto.io_interface()[..] else {
            panic!("Range expects [start] [limit] [delta] -> [output]")
        };

        let start_tensor = self.tensor(start).unwrap();
        let limit_tensor = self.tensor(limit).unwrap();
        let delta_tensor = self.tensor(delta).unwrap();

        let start = start_tensor.load::<f32>().first().unwrap().clone();
        let limit = limit_tensor.load::<f32>().first().unwrap().clone();
        let delta = delta_tensor.load::<f32>().first().unwrap().clone();

        let number_of_elements = (limit - start) / delta;
        let n = number_of_elements.ceil().max(0.0) as i32;
        let data = (0..n)
            .collect::<Vec<_>>()
            .into_iter()
            .map(|i| start + ((i as f32) * delta))
            .collect::<Vec<_>>();

        let output_tensor = Tensor::from_contiguous(&data[..], &[n]);
        self.track_tensor(output, output_tensor);
    }

    fn ReduceMean(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let keepdims = attributes
            .get("keepdims")
            .map(Into::<bool>::into)
            .unwrap_or(true);
        let noop_with_empty_axes = attributes
            .get("noop_with_empty_axes")
            .map(Into::<bool>::into)
            .unwrap_or(false);
        log::trace!(
            "[ONNX] [ReduceMean] [keepdims={}] [noop_with_empty_axes={}]",
            keepdims,
            noop_with_empty_axes
        );

        let [data, axes @ ..] = &node_proto.input[..] else {
            panic!("ReduceMean expects [data] [axes] -> [reduced]")
        };
        let [reduced] = &node_proto.output[..] else {
            panic!("ReduceMean expects [data] [axes] -> [reduced]")
        };

        let data_tensor = self.tensor(data).unwrap();
        let axes = axes
            .first()
            .map(|axes| self.tensor(axes))
            .flatten()
            .map(|tensor| tensor.load())
            .map(|bytes| {
                bytemuck::cast_slice::<u8, f32>(&bytes[..])
                    .iter()
                    .map(|&x: &f32| x as i32)
                    .collect::<Vec<_>>()
            })
            .unwrap_or(
                (0..data_tensor.ndim())
                    .into_iter()
                    .map(|axis| axis as i32)
                    .collect::<Vec<_>>(),
            );

        let reduced_tensor = if noop_with_empty_axes {
            data_tensor.clone()
        } else {
            data_tensor.Mean(&axes[..], keepdims)
        };
        self.track_tensor(reduced, reduced_tensor);
    }

    fn Reshape(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let allowzero = attributes
            .get("allowzero")
            .map(Into::<bool>::into)
            .unwrap_or(false);
        log::trace!("[ONNX] [Reshape] [allowzero={}]", allowzero);

        assert!(!allowzero, "Reshape: 0-length dimensions are not allowed");

        let [data, shape, reshaped] = &node_proto.io_interface()[..] else {
            panic!("Reshape expects [data] [shape] -> [reshaped]")
        };

        let data_tensor = self.tensor(data).unwrap();
        let shape_tensor = self.tensor(shape).unwrap();

        let shape = shape_tensor.load::<f32>();
        // Map [] -> [1]
        let shape = if shape.len() == 0 { vec![1.0] } else { shape };
        let shape = &shape[..];

        let data_tensor_size = data_tensor.len();
        let shape_size = shape.iter().product::<f32>().abs() as i32;
        let inferred_size = data_tensor_size / shape_size;

        let shape = shape
            .into_iter()
            .enumerate()
            .map(|(axis, &dimension)| {
                let dimension = dimension as i64;

                assert!(
                    dimension >= -1,
                    "Reshape does not support negative dimensions"
                );

                if dimension == -1 {
                    inferred_size as i32
                } else if dimension == 0 {
                    if allowzero {
                        0
                    } else {
                        data_tensor.shape()[axis]
                    }
                } else {
                    dimension as i32
                }
            })
            .collect::<Vec<_>>();
        let view = TensorView::from_contiguous_shape(&shape[..]);

        assert!(
            data_tensor_size == view.len(),
            "Unable to reshape from {:?} -> {:?}",
            data_tensor.shape(),
            shape
        );

        let reshaped_tensor = data_tensor.reshape(&view);
        self.track_tensor(reshaped, reshaped_tensor);
    }

    // fn Resize(&self, node_proto: &onnx::NodeProto) {
    //     // TODO P1
    //     Tensor::scalar(1);
    //     panic!("Resize has not been implemented");
    // }

    fn Shape(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let start = attributes.get("start").map(Into::<i64>::into).unwrap_or(0);
        let end = attributes.get("end").map(Into::<i64>::into);

        let [data, shape] = &node_proto.io_interface()[..] else {
            panic!("Shape expects [data] -> [shape]")
        };

        let data_tensor = self.tensor(data).unwrap();

        let start = start as usize;
        let bound = data_tensor.ndim() as i64;
        let end = end.unwrap_or(bound - 1);
        let end = ((end + bound) % bound) as usize;
        log::trace!("[ONNX] [Shape] [start={}] [end={}]", start, end);

        let shape_slice = data_tensor.shape()[start..end]
            .iter()
            .map(|&x| x as f32)
            .collect::<Vec<_>>();
        let shape_tensor = Tensor::from_contiguous(&shape_slice[..], &[shape_slice.len() as i32]);
        self.track_tensor(shape, shape_tensor);
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

    fn Slice(&self, node_proto: &onnx::NodeProto) {
        let [data, starts, ends, optional_inputs @ .., output] = &node_proto.io_interface()[..]
        else {
            panic!("Slice expects [data] [starts] [ends] [axes] [steps] -> [output]")
        };

        let data_tensor = self.tensor(data).unwrap();
        let starts_tensor = self.tensor(starts).unwrap();
        let ends_tensor = self.tensor(ends).unwrap();

        let data_rank = data_tensor.ndim() as i32;

        let starts_bytes = starts_tensor.load();
        let starts = bytemuck::cast_slice::<u8, f32>(&starts_bytes[..])
            .iter()
            .map(|&x: &f32| x as i32)
            .map(|x| (x + data_rank) % data_rank)
            .map(|x| x as i32)
            .collect::<Vec<_>>();

        let ends_bytes = starts_tensor.load();
        let ends = bytemuck::cast_slice::<u8, f32>(&ends_bytes[..])
            .iter()
            .map(|&x: &f32| x as i32)
            .map(|x| (x + data_rank) % data_rank)
            .map(|x| x as i32)
            .collect::<Vec<_>>();

        let axes = optional_inputs
            .first()
            .map(|axes| self.tensor(axes))
            .flatten()
            .map(|tensor| tensor.load())
            .map(|bytes| {
                bytemuck::cast_slice::<u8, f32>(&bytes[..])
                    .iter()
                    .map(|&x: &f32| x as i32)
                    .map(|x| (x + data_rank) % data_rank)
                    .map(|x| x as i32)
                    .collect::<Vec<_>>()
            })
            .unwrap_or(
                (0..data_tensor.ndim())
                    .into_iter()
                    .map(|axis| axis as i32)
                    .collect::<Vec<_>>(),
            );

        let steps = optional_inputs
            .get(1)
            .map(|steps| self.tensor(steps))
            .flatten()
            .map(|tensor| tensor.load())
            .map(|bytes| {
                bytemuck::cast_slice::<u8, f32>(&bytes[..])
                    .iter()
                    .map(|&x: &f32| {
                        assert!(x >= 0.0, "Negative step sizes are not supported");
                        x as i32
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or(starts.iter().map(|_| 1).collect::<Vec<_>>());

        let output_tensor = data_tensor.Slice(&starts[..], &ends[..], &axes[..], &steps[..]);
        self.track_tensor(output, output_tensor);
    }

    fn Softmax(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let axis = attributes.get("axis").map(Into::<i64>::into).unwrap_or(-1);
        log::trace!("[ONNX] [Softmax] [axis={}]", axis);

        let [input, output] = &node_proto.io_interface()[..] else {
            panic!("Softmax expects [input] -> [output]")
        };

        let input_tensor = self.tensor(input).unwrap();

        let input_dimension = input_tensor.ndim() as i64;
        let axis = ((axis + input_dimension) % input_dimension) as i32;

        let output_tensor = input_tensor.Softmax(axis);
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

    fn Squeeze(&self, node_proto: &onnx::NodeProto) {
        let [data, axes @ ..] = &node_proto.input[..] else {
            panic!("Squeeze expects at least one argument of [data] [axes]")
        };
        let [squeezed] = &node_proto.output[..] else {
            panic!("Squeeze only has 1 output")
        };

        let data_tensor = self.tensor(data).unwrap();
        let data_tensor_dimensions = data_tensor.ndim() as f32;
        let axes = axes
            .first()
            .map(|axes| self.tensor(axes))
            .flatten()
            .map(|tensor| tensor.load())
            .map(|bytes| {
                let mut axes = bytemuck::cast_slice::<u8, f32>(&bytes[..])
                    .iter()
                    .map(|&x: &f32| (x + data_tensor_dimensions) % data_tensor_dimensions)
                    .collect::<Vec<_>>();

                axes.sort_by(|a, b| a.partial_cmp(b).unwrap());
                axes.into_iter().rev().map(|x| x as i32).collect::<Vec<_>>()
            })
            // If axes is not provided, all the single dimensions will be removed from the shape.
            .unwrap_or(
                data_tensor
                    .view()
                    .shape
                    .iter()
                    .enumerate()
                    .filter(|(_, &dimension)| dimension == 1)
                    .map(|(axis, _)| axis as i32)
                    .collect::<Vec<_>>(),
            );

        let squeezed_tensor = axes
            .iter()
            .fold(data_tensor, |acc: Tensor, &axis| acc.squeeze(axis));
        self.track_tensor(squeezed, squeezed_tensor)
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

    fn Transpose(&self, node_proto: &onnx::NodeProto) {
        let attributes = node_proto.attributes();
        let perm = attributes.get("perm").map(Into::<Vec<i64>>::into);

        let [data, transposed] = &node_proto.io_interface()[..] else {
            panic!("Transpose expects [data] -> [transposed]")
        };

        let data_tensor = self.tensor(data).unwrap();

        let perm = perm
            .unwrap_or(
                (0..(data_tensor.ndim() as i64))
                    .rev()
                    .into_iter()
                    .collect::<Vec<_>>(),
            )
            .into_iter()
            .map(|axis| axis as i32)
            .collect::<Vec<_>>();
        log::trace!("[ONNX] [Transpose] [perm={:?}]", perm);

        let transposed_tensor = data_tensor.transpose(&perm[..]);
        self.track_tensor(transposed, transposed_tensor);
    }

    fn Unsqueeze(&self, node_proto: &onnx::NodeProto) {
        let [data, axes, expanded] = &node_proto.io_interface()[..] else {
            panic!("Unsqueeze expects [data] [axes] -> [expanded]")
        };

        let data_tensor = self.tensor(data).unwrap();
        let data_tensor_dimensions = data_tensor.ndim() as f32;

        let axes_tensor = self.tensor(axes).unwrap();
        let axes_bytes = axes_tensor.load();
        let mut axes = bytemuck::cast_slice::<u8, f32>(&axes_bytes[..])
            .iter()
            .map(|&x: &f32| {
                // assert!(
                //     x >= -data_tensor_dimensions && x < data_tensor_dimensions,
                //     "Range of axes should be valid, constrained within -{range} <= x < {range}",
                //     range = data_tensor_dimensions
                // );
                (x + data_tensor_dimensions) % data_tensor_dimensions
            })
            .collect::<Vec<_>>();
        axes.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let axes = axes.into_iter().rev().map(|x| x as i32).collect::<Vec<_>>();

        let unsqueezed_tensor = axes
            .iter()
            .fold(data_tensor, |acc: Tensor, &axis| acc.unsqueeze(axis));
        self.track_tensor(expanded, unsqueezed_tensor)
    }

    fn Where(&self, node_proto: &onnx::NodeProto) {
        let [condition, x, y, output] = &node_proto.io_interface()[..] else {
            panic!("Where expects [condition] [x] [y] -> [output]")
        };

        let condition_tensor = self.tensor(condition).unwrap();
        let x_tensor = self.tensor(x).unwrap();
        let y_tensor = self.tensor(y).unwrap();

        // When True (nonzero), yield X, otherwise yield Y
        let zero = Tensor::scalar(0);
        let condition_zero = condition_tensor.Equal(&zero);
        let condition_nonzero = condition_zero.Not();

        let yield_x = condition_nonzero.Multiply(&x_tensor);
        let yield_y = condition_zero.Multiply(&y_tensor);

        let output_tensor = yield_x.Add(&yield_y);
        self.track_tensor(output, output_tensor);
    }
}
