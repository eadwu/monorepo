use std::collections::HashMap;

use crate::onnx;
use tensor::primitives::tensor::Tensor;

pub trait OnnxRuntime {
    fn tensor(&self, proto_name: &str) -> Option<Tensor>;

    fn track_tensor(&self, name: &str, tensor: Tensor);

    fn serialize_node(&self, proto: &onnx::NodeProto) {
        match proto.op_type.as_str() {
            "Abs" => self.Abs(proto),
            "Acos" => self.Acos(proto),
            "Acosh" => self.Acosh(proto),
            "Add" => self.Add(proto),
            "And" => self.And(proto),
            "ArgMax" => self.ArgMax(proto),
            "ArgMin" => self.ArgMin(proto),
            "Asin" => self.Asin(proto),
            "Asinh" => self.Asinh(proto),
            "Atan" => self.Atan(proto),
            "Atanh" => self.Atanh(proto),
            "AveragePool" => self.AveragePool(proto),
            "BatchNormalization" => self.BatchNormalization(proto),
            "BitShift" => self.BitShift(proto),
            "BitwiseAnd" => self.BitwiseAnd(proto),
            "BitwiseNot" => self.BitwiseNot(proto),
            "BitwiseOr" => self.BitwiseOr(proto),
            "BitwiseXor" => self.BitwiseXor(proto),
            "Cast" => self.Cast(proto),
            "Ceil" => self.Ceil(proto),
            "Col2Im" => self.Col2Im(proto),
            "Compress" => self.Compress(proto),
            "Concat" => self.Concat(proto),
            "ConcatFromSequence" => self.ConcatFromSequence(proto),
            "Constant" => self.Constant(proto),
            "ConstantOfShape" => self.ConstantOfShape(proto),
            "Conv" => self.Conv(proto),
            "ConvInteger" => self.ConvInteger(proto),
            "ConvTranspose" => self.ConvTranspose(proto),
            "Cos" => self.Cos(proto),
            "Cosh" => self.Cosh(proto),
            "CumSum" => self.CumSum(proto),
            "DFT" => self.DFT(proto),
            "DeformConv" => self.DeformConv(proto),
            "DepthToSpace" => self.DepthToSpace(proto),
            "DequantizeLinear" => self.DequantizeLinear(proto),
            "Det" => self.Det(proto),
            "Div" => self.Div(proto),
            "Dropout" => self.Dropout(proto),
            "Einsum" => self.Einsum(proto),
            "Equal" => self.Equal(proto),
            "Erf" => self.Erf(proto),
            "Exp" => self.Exp(proto),
            "Expand" => self.Expand(proto),
            "EyeLike" => self.EyeLike(proto),
            "Flatten" => self.Flatten(proto),
            "Floor" => self.Floor(proto),
            "GRU" => self.GRU(proto),
            "Gather" => self.Gather(proto),
            "GatherElements" => self.GatherElements(proto),
            "GatherND" => self.GatherND(proto),
            "Gemm" => self.Gemm(proto),
            "GlobalAveragePool" => self.GlobalAveragePool(proto),
            "GlobalLpPool" => self.GlobalLpPool(proto),
            "GlobalMaxPool" => self.GlobalMaxPool(proto),
            "Greater" => self.Greater(proto),
            "GridSample" => self.GridSample(proto),
            "Hardmax" => self.Hardmax(proto),
            "Identity" => self.Identity(proto),
            "If" => self.If(proto),
            "ImageDecoder" => self.ImageDecoder(proto),
            "InstanceNormalization" => self.InstanceNormalization(proto),
            "IsInf" => self.IsInf(proto),
            "IsNaN" => self.IsNaN(proto),
            "LRN" => self.LRN(proto),
            "LSTM" => self.LSTM(proto),
            "Less" => self.Less(proto),
            "Log" => self.Log(proto),
            "Loop" => self.Loop(proto),
            "LpNormalization" => self.LpNormalization(proto),
            "LpPool" => self.LpPool(proto),
            "MatMul" => self.MatMul(proto),
            "MatMulInteger" => self.MatMulInteger(proto),
            "Max" => self.Max(proto),
            "MaxPool" => self.MaxPool(proto),
            "MaxRoiPool" => self.MaxRoiPool(proto),
            "MaxUnpool" => self.MaxUnpool(proto),
            "Mean" => self.Mean(proto),
            "MelWeightMatrix" => self.MelWeightMatrix(proto),
            "Min" => self.Min(proto),
            "Mod" => self.Mod(proto),
            "Mul" => self.Mul(proto),
            "Multinomial" => self.Multinomial(proto),
            "Neg" => self.Neg(proto),
            "NonMaxSuppression" => self.NonMaxSuppression(proto),
            "NonZero" => self.NonZero(proto),
            "Not" => self.Not(proto),
            "OneHot" => self.OneHot(proto),
            "Optional" => self.Optional(proto),
            "OptionalGetElement" => self.OptionalGetElement(proto),
            "OptionalHasElement" => self.OptionalHasElement(proto),
            "Or" => self.Or(proto),
            "Pad" => self.Pad(proto),
            "Pow" => self.Pow(proto),
            "QLinearConv" => self.QLinearConv(proto),
            "QLinearMatMul" => self.QLinearMatMul(proto),
            "QuantizeLinear" => self.QuantizeLinear(proto),
            "RNN" => self.RNN(proto),
            "RandomNormal" => self.RandomNormal(proto),
            "RandomNormalLike" => self.RandomNormalLike(proto),
            "RandomUniform" => self.RandomUniform(proto),
            "RandomUniformLike" => self.RandomUniformLike(proto),
            "Reciprocal" => self.Reciprocal(proto),
            "ReduceMax" => self.ReduceMax(proto),
            "ReduceMean" => self.ReduceMean(proto),
            "ReduceMin" => self.ReduceMin(proto),
            "ReduceProd" => self.ReduceProd(proto),
            "ReduceSum" => self.ReduceSum(proto),
            "RegexFullMatch" => self.RegexFullMatch(proto),
            "Reshape" => self.Reshape(proto),
            "Resize" => self.Resize(proto),
            "ReverseSequence" => self.ReverseSequence(proto),
            "RoiAlign" => self.RoiAlign(proto),
            "Round" => self.Round(proto),
            "STFT" => self.STFT(proto),
            "Scan" => self.Scan(proto),
            "Scatter" => self.Scatter(proto),
            "ScatterElements" => self.ScatterElements(proto),
            "ScatterND" => self.ScatterND(proto),
            "SequenceAt" => self.SequenceAt(proto),
            "SequenceConstruct" => self.SequenceConstruct(proto),
            "SequenceEmpty" => self.SequenceEmpty(proto),
            "SequenceErase" => self.SequenceErase(proto),
            "SequenceInsert" => self.SequenceInsert(proto),
            "SequenceLength" => self.SequenceLength(proto),
            "Shape" => self.Shape(proto),
            "Sigmoid" => self.Sigmoid(proto),
            "Sign" => self.Sign(proto),
            "Sin" => self.Sin(proto),
            "Sinh" => self.Sinh(proto),
            "Size" => self.Size(proto),
            "Slice" => self.Slice(proto),
            "SpaceToDepth" => self.SpaceToDepth(proto),
            "Split" => self.Split(proto),
            "SplitToSequence" => self.SplitToSequence(proto),
            "Sqrt" => self.Sqrt(proto),
            "Squeeze" => self.Squeeze(proto),
            "StringConcat" => self.StringConcat(proto),
            "StringNormalizer" => self.StringNormalizer(proto),
            "StringSplit" => self.StringSplit(proto),
            "Sub" => self.Sub(proto),
            "Sum" => self.Sum(proto),
            "Tan" => self.Tan(proto),
            "Tanh" => self.Tanh(proto),
            "TfIdfVectorizer" => self.TfIdfVectorizer(proto),
            "Tile" => self.Tile(proto),
            "TopK" => self.TopK(proto),
            "Transpose" => self.Transpose(proto),
            "Trilu" => self.Trilu(proto),
            "Unique" => self.Unique(proto),
            "Unsqueeze" => self.Unsqueeze(proto),
            "Upsample" => self.Upsample(proto),
            "Where" => self.Where(proto),
            "Xor" => self.Xor(proto),
            "AffineGrid" => self.AffineGrid(proto),
            "Bernoulli" => self.Bernoulli(proto),
            "BlackmanWindow" => self.BlackmanWindow(proto),
            "CastLike" => self.CastLike(proto),
            "Celu" => self.Celu(proto),
            "CenterCropPad" => self.CenterCropPad(proto),
            "Clip" => self.Clip(proto),
            "DynamicQuantizeLinear" => self.DynamicQuantizeLinear(proto),
            "Elu" => self.Elu(proto),
            "Gelu" => self.Gelu(proto),
            "GreaterOrEqual" => self.GreaterOrEqual(proto),
            "GroupNormalization" => self.GroupNormalization(proto),
            "HammingWindow" => self.HammingWindow(proto),
            "HannWindow" => self.HannWindow(proto),
            "HardSigmoid" => self.HardSigmoid(proto),
            "HardSwish" => self.HardSwish(proto),
            "LayerNormalization" => self.LayerNormalization(proto),
            "LeakyRelu" => self.LeakyRelu(proto),
            "LessOrEqual" => self.LessOrEqual(proto),
            "LogSoftmax" => self.LogSoftmax(proto),
            "MeanVarianceNormalization" => self.MeanVarianceNormalization(proto),
            "Mish" => self.Mish(proto),
            "NegativeLogLikelihoodLoss" => self.NegativeLogLikelihoodLoss(proto),
            "PRelu" => self.PRelu(proto),
            "Range" => self.Range(proto),
            "ReduceL1" => self.ReduceL1(proto),
            "ReduceL2" => self.ReduceL2(proto),
            "ReduceLogSum" => self.ReduceLogSum(proto),
            "ReduceLogSumExp" => self.ReduceLogSumExp(proto),
            "ReduceSumSquare" => self.ReduceSumSquare(proto),
            "Relu" => self.Relu(proto),
            "Selu" => self.Selu(proto),
            "SequenceMap" => self.SequenceMap(proto),
            "Shrink" => self.Shrink(proto),
            "Softmax" => self.Softmax(proto),
            "SoftmaxCrossEntropyLoss" => self.SoftmaxCrossEntropyLoss(proto),
            "Softplus" => self.Softplus(proto),
            "Softsign" => self.Softsign(proto),
            "ThresholdedRelu" => self.ThresholdedRelu(proto),
            _ => panic!(
                "ONNX op_type not supported - got {}",
                proto.op_type.as_str()
            ),
        }
    }

    fn Abs(&self, node_proto: &onnx::NodeProto) {
        panic!("Abs has not been implemented");
    }

    fn Acos(&self, node_proto: &onnx::NodeProto) {
        panic!("Acos has not been implemented");
    }

    fn Acosh(&self, node_proto: &onnx::NodeProto) {
        panic!("Acosh has not been implemented");
    }

    fn Add(&self, node_proto: &onnx::NodeProto) {
        panic!("Add has not been implemented");
    }

    fn And(&self, node_proto: &onnx::NodeProto) {
        panic!("And has not been implemented");
    }

    fn ArgMax(&self, node_proto: &onnx::NodeProto) {
        panic!("ArgMax has not been implemented");
    }

    fn ArgMin(&self, node_proto: &onnx::NodeProto) {
        panic!("ArgMin has not been implemented");
    }

    fn Asin(&self, node_proto: &onnx::NodeProto) {
        panic!("Asin has not been implemented");
    }

    fn Asinh(&self, node_proto: &onnx::NodeProto) {
        panic!("Asinh has not been implemented");
    }

    fn Atan(&self, node_proto: &onnx::NodeProto) {
        panic!("Atan has not been implemented");
    }

    fn Atanh(&self, node_proto: &onnx::NodeProto) {
        panic!("Atanh has not been implemented");
    }

    fn AveragePool(&self, node_proto: &onnx::NodeProto) {
        panic!("AveragePool has not been implemented");
    }

    fn BatchNormalization(&self, node_proto: &onnx::NodeProto) {
        panic!("BatchNormalization has not been implemented");
    }

    fn BitShift(&self, node_proto: &onnx::NodeProto) {
        panic!("BitShift has not been implemented");
    }

    fn BitwiseAnd(&self, node_proto: &onnx::NodeProto) {
        panic!("BitwiseAnd has not been implemented");
    }

    fn BitwiseNot(&self, node_proto: &onnx::NodeProto) {
        panic!("BitwiseNot has not been implemented");
    }

    fn BitwiseOr(&self, node_proto: &onnx::NodeProto) {
        panic!("BitwiseOr has not been implemented");
    }

    fn BitwiseXor(&self, node_proto: &onnx::NodeProto) {
        panic!("BitwiseXor has not been implemented");
    }

    fn Cast(&self, node_proto: &onnx::NodeProto) {
        panic!("Cast has not been implemented");
    }

    fn Ceil(&self, node_proto: &onnx::NodeProto) {
        panic!("Ceil has not been implemented");
    }

    fn Col2Im(&self, node_proto: &onnx::NodeProto) {
        panic!("Col2Im has not been implemented");
    }

    fn Compress(&self, node_proto: &onnx::NodeProto) {
        panic!("Compress has not been implemented");
    }

    fn Concat(&self, node_proto: &onnx::NodeProto) {
        panic!("Concat has not been implemented");
    }

    fn ConcatFromSequence(&self, node_proto: &onnx::NodeProto) {
        panic!("ConcatFromSequence has not been implemented");
    }

    fn Constant(&self, node_proto: &onnx::NodeProto) {
        panic!("Constant has not been implemented");
    }

    fn ConstantOfShape(&self, node_proto: &onnx::NodeProto) {
        panic!("ConstantOfShape has not been implemented");
    }

    fn Conv(&self, node_proto: &onnx::NodeProto) {
        panic!("Conv has not been implemented");
    }

    fn ConvInteger(&self, node_proto: &onnx::NodeProto) {
        panic!("ConvInteger has not been implemented");
    }

    fn ConvTranspose(&self, node_proto: &onnx::NodeProto) {
        panic!("ConvTranspose has not been implemented");
    }

    fn Cos(&self, node_proto: &onnx::NodeProto) {
        panic!("Cos has not been implemented");
    }

    fn Cosh(&self, node_proto: &onnx::NodeProto) {
        panic!("Cosh has not been implemented");
    }

    fn CumSum(&self, node_proto: &onnx::NodeProto) {
        panic!("CumSum has not been implemented");
    }

    fn DFT(&self, node_proto: &onnx::NodeProto) {
        panic!("DFT has not been implemented");
    }

    fn DeformConv(&self, node_proto: &onnx::NodeProto) {
        panic!("DeformConv has not been implemented");
    }

    fn DepthToSpace(&self, node_proto: &onnx::NodeProto) {
        panic!("DepthToSpace has not been implemented");
    }

    fn DequantizeLinear(&self, node_proto: &onnx::NodeProto) {
        panic!("DequantizeLinear has not been implemented");
    }

    fn Det(&self, node_proto: &onnx::NodeProto) {
        panic!("Det has not been implemented");
    }

    fn Div(&self, node_proto: &onnx::NodeProto) {
        panic!("Div has not been implemented");
    }

    fn Dropout(&self, node_proto: &onnx::NodeProto) {
        panic!("Dropout has not been implemented");
    }

    fn Einsum(&self, node_proto: &onnx::NodeProto) {
        panic!("Einsum has not been implemented");
    }

    fn Equal(&self, node_proto: &onnx::NodeProto) {
        panic!("Equal has not been implemented");
    }

    fn Erf(&self, node_proto: &onnx::NodeProto) {
        panic!("Erf has not been implemented");
    }

    fn Exp(&self, node_proto: &onnx::NodeProto) {
        panic!("Exp has not been implemented");
    }

    fn Expand(&self, node_proto: &onnx::NodeProto) {
        panic!("Expand has not been implemented");
    }

    fn EyeLike(&self, node_proto: &onnx::NodeProto) {
        panic!("EyeLike has not been implemented");
    }

    fn Flatten(&self, node_proto: &onnx::NodeProto) {
        panic!("Flatten has not been implemented");
    }

    fn Floor(&self, node_proto: &onnx::NodeProto) {
        panic!("Floor has not been implemented");
    }

    fn GRU(&self, node_proto: &onnx::NodeProto) {
        panic!("GRU has not been implemented");
    }

    fn Gather(&self, node_proto: &onnx::NodeProto) {
        panic!("Gather has not been implemented");
    }

    fn GatherElements(&self, node_proto: &onnx::NodeProto) {
        panic!("GatherElements has not been implemented");
    }

    fn GatherND(&self, node_proto: &onnx::NodeProto) {
        panic!("GatherND has not been implemented");
    }

    fn Gemm(&self, node_proto: &onnx::NodeProto) {
        panic!("Gemm has not been implemented");
    }

    fn GlobalAveragePool(&self, node_proto: &onnx::NodeProto) {
        panic!("GlobalAveragePool has not been implemented");
    }

    fn GlobalLpPool(&self, node_proto: &onnx::NodeProto) {
        panic!("GlobalLpPool has not been implemented");
    }

    fn GlobalMaxPool(&self, node_proto: &onnx::NodeProto) {
        panic!("GlobalMaxPool has not been implemented");
    }

    fn Greater(&self, node_proto: &onnx::NodeProto) {
        panic!("Greater has not been implemented");
    }

    fn GridSample(&self, node_proto: &onnx::NodeProto) {
        panic!("GridSample has not been implemented");
    }

    fn Hardmax(&self, node_proto: &onnx::NodeProto) {
        panic!("Hardmax has not been implemented");
    }

    fn Identity(&self, node_proto: &onnx::NodeProto) {
        panic!("Identity has not been implemented");
    }

    fn If(&self, node_proto: &onnx::NodeProto) {
        panic!("If has not been implemented");
    }

    fn ImageDecoder(&self, node_proto: &onnx::NodeProto) {
        panic!("ImageDecoder has not been implemented");
    }

    fn InstanceNormalization(&self, node_proto: &onnx::NodeProto) {
        panic!("InstanceNormalization has not been implemented");
    }

    fn IsInf(&self, node_proto: &onnx::NodeProto) {
        panic!("IsInf has not been implemented");
    }

    fn IsNaN(&self, node_proto: &onnx::NodeProto) {
        panic!("IsNaN has not been implemented");
    }

    fn LRN(&self, node_proto: &onnx::NodeProto) {
        panic!("LRN has not been implemented");
    }

    fn LSTM(&self, node_proto: &onnx::NodeProto) {
        panic!("LSTM has not been implemented");
    }

    fn Less(&self, node_proto: &onnx::NodeProto) {
        panic!("Less has not been implemented");
    }

    fn Log(&self, node_proto: &onnx::NodeProto) {
        panic!("Log has not been implemented");
    }

    fn Loop(&self, node_proto: &onnx::NodeProto) {
        panic!("Loop has not been implemented");
    }

    fn LpNormalization(&self, node_proto: &onnx::NodeProto) {
        panic!("LpNormalization has not been implemented");
    }

    fn LpPool(&self, node_proto: &onnx::NodeProto) {
        panic!("LpPool has not been implemented");
    }

    fn MatMul(&self, node_proto: &onnx::NodeProto) {
        panic!("MatMul has not been implemented");
    }

    fn MatMulInteger(&self, node_proto: &onnx::NodeProto) {
        panic!("MatMulInteger has not been implemented");
    }

    fn Max(&self, node_proto: &onnx::NodeProto) {
        panic!("Max has not been implemented");
    }

    fn MaxPool(&self, node_proto: &onnx::NodeProto) {
        panic!("MaxPool has not been implemented");
    }

    fn MaxRoiPool(&self, node_proto: &onnx::NodeProto) {
        panic!("MaxRoiPool has not been implemented");
    }

    fn MaxUnpool(&self, node_proto: &onnx::NodeProto) {
        panic!("MaxUnpool has not been implemented");
    }

    fn Mean(&self, node_proto: &onnx::NodeProto) {
        panic!("Mean has not been implemented");
    }

    fn MelWeightMatrix(&self, node_proto: &onnx::NodeProto) {
        panic!("MelWeightMatrix has not been implemented");
    }

    fn Min(&self, node_proto: &onnx::NodeProto) {
        panic!("Min has not been implemented");
    }

    fn Mod(&self, node_proto: &onnx::NodeProto) {
        panic!("Mod has not been implemented");
    }

    fn Mul(&self, node_proto: &onnx::NodeProto) {
        panic!("Mul has not been implemented");
    }

    fn Multinomial(&self, node_proto: &onnx::NodeProto) {
        panic!("Multinomial has not been implemented");
    }

    fn Neg(&self, node_proto: &onnx::NodeProto) {
        panic!("Neg has not been implemented");
    }

    fn NonMaxSuppression(&self, node_proto: &onnx::NodeProto) {
        panic!("NonMaxSuppression has not been implemented");
    }

    fn NonZero(&self, node_proto: &onnx::NodeProto) {
        panic!("NonZero has not been implemented");
    }

    fn Not(&self, node_proto: &onnx::NodeProto) {
        panic!("Not has not been implemented");
    }

    fn OneHot(&self, node_proto: &onnx::NodeProto) {
        panic!("OneHot has not been implemented");
    }

    fn Optional(&self, node_proto: &onnx::NodeProto) {
        panic!("Optional has not been implemented");
    }

    fn OptionalGetElement(&self, node_proto: &onnx::NodeProto) {
        panic!("OptionalGetElement has not been implemented");
    }

    fn OptionalHasElement(&self, node_proto: &onnx::NodeProto) {
        panic!("OptionalHasElement has not been implemented");
    }

    fn Or(&self, node_proto: &onnx::NodeProto) {
        panic!("Or has not been implemented");
    }

    fn Pad(&self, node_proto: &onnx::NodeProto) {
        panic!("Pad has not been implemented");
    }

    fn Pow(&self, node_proto: &onnx::NodeProto) {
        panic!("Pow has not been implemented");
    }

    fn QLinearConv(&self, node_proto: &onnx::NodeProto) {
        panic!("QLinearConv has not been implemented");
    }

    fn QLinearMatMul(&self, node_proto: &onnx::NodeProto) {
        panic!("QLinearMatMul has not been implemented");
    }

    fn QuantizeLinear(&self, node_proto: &onnx::NodeProto) {
        panic!("QuantizeLinear has not been implemented");
    }

    fn RNN(&self, node_proto: &onnx::NodeProto) {
        panic!("RNN has not been implemented");
    }

    fn RandomNormal(&self, node_proto: &onnx::NodeProto) {
        panic!("RandomNormal has not been implemented");
    }

    fn RandomNormalLike(&self, node_proto: &onnx::NodeProto) {
        panic!("RandomNormalLike has not been implemented");
    }

    fn RandomUniform(&self, node_proto: &onnx::NodeProto) {
        panic!("RandomUniform has not been implemented");
    }

    fn RandomUniformLike(&self, node_proto: &onnx::NodeProto) {
        panic!("RandomUniformLike has not been implemented");
    }

    fn Reciprocal(&self, node_proto: &onnx::NodeProto) {
        panic!("Reciprocal has not been implemented");
    }

    fn ReduceMax(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceMax has not been implemented");
    }

    fn ReduceMean(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceMean has not been implemented");
    }

    fn ReduceMin(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceMin has not been implemented");
    }

    fn ReduceProd(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceProd has not been implemented");
    }

    fn ReduceSum(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceSum has not been implemented");
    }

    fn RegexFullMatch(&self, node_proto: &onnx::NodeProto) {
        panic!("RegexFullMatch has not been implemented");
    }

    fn Reshape(&self, node_proto: &onnx::NodeProto) {
        panic!("Reshape has not been implemented");
    }

    fn Resize(&self, node_proto: &onnx::NodeProto) {
        panic!("Resize has not been implemented");
    }

    fn ReverseSequence(&self, node_proto: &onnx::NodeProto) {
        panic!("ReverseSequence has not been implemented");
    }

    fn RoiAlign(&self, node_proto: &onnx::NodeProto) {
        panic!("RoiAlign has not been implemented");
    }

    fn Round(&self, node_proto: &onnx::NodeProto) {
        panic!("Round has not been implemented");
    }

    fn STFT(&self, node_proto: &onnx::NodeProto) {
        panic!("STFT has not been implemented");
    }

    fn Scan(&self, node_proto: &onnx::NodeProto) {
        panic!("Scan has not been implemented");
    }

    fn Scatter(&self, node_proto: &onnx::NodeProto) {
        panic!("Scatter has not been implemented");
    }

    fn ScatterElements(&self, node_proto: &onnx::NodeProto) {
        panic!("ScatterElements has not been implemented");
    }

    fn ScatterND(&self, node_proto: &onnx::NodeProto) {
        panic!("ScatterND has not been implemented");
    }

    fn SequenceAt(&self, node_proto: &onnx::NodeProto) {
        panic!("SequenceAt has not been implemented");
    }

    fn SequenceConstruct(&self, node_proto: &onnx::NodeProto) {
        panic!("SequenceConstruct has not been implemented");
    }

    fn SequenceEmpty(&self, node_proto: &onnx::NodeProto) {
        panic!("SequenceEmpty has not been implemented");
    }

    fn SequenceErase(&self, node_proto: &onnx::NodeProto) {
        panic!("SequenceErase has not been implemented");
    }

    fn SequenceInsert(&self, node_proto: &onnx::NodeProto) {
        panic!("SequenceInsert has not been implemented");
    }

    fn SequenceLength(&self, node_proto: &onnx::NodeProto) {
        panic!("SequenceLength has not been implemented");
    }

    fn Shape(&self, node_proto: &onnx::NodeProto) {
        panic!("Shape has not been implemented");
    }

    fn Sigmoid(&self, node_proto: &onnx::NodeProto) {
        panic!("Sigmoid has not been implemented");
    }

    fn Sign(&self, node_proto: &onnx::NodeProto) {
        panic!("Sign has not been implemented");
    }

    fn Sin(&self, node_proto: &onnx::NodeProto) {
        panic!("Sin has not been implemented");
    }

    fn Sinh(&self, node_proto: &onnx::NodeProto) {
        panic!("Sinh has not been implemented");
    }

    fn Size(&self, node_proto: &onnx::NodeProto) {
        panic!("Size has not been implemented");
    }

    fn Slice(&self, node_proto: &onnx::NodeProto) {
        panic!("Slice has not been implemented");
    }

    fn SpaceToDepth(&self, node_proto: &onnx::NodeProto) {
        panic!("SpaceToDepth has not been implemented");
    }

    fn Split(&self, node_proto: &onnx::NodeProto) {
        panic!("Split has not been implemented");
    }

    fn SplitToSequence(&self, node_proto: &onnx::NodeProto) {
        panic!("SplitToSequence has not been implemented");
    }

    fn Sqrt(&self, node_proto: &onnx::NodeProto) {
        panic!("Sqrt has not been implemented");
    }

    fn Squeeze(&self, node_proto: &onnx::NodeProto) {
        panic!("Squeeze has not been implemented");
    }

    fn StringConcat(&self, node_proto: &onnx::NodeProto) {
        panic!("StringConcat has not been implemented");
    }

    fn StringNormalizer(&self, node_proto: &onnx::NodeProto) {
        panic!("StringNormalizer has not been implemented");
    }

    fn StringSplit(&self, node_proto: &onnx::NodeProto) {
        panic!("StringSplit has not been implemented");
    }

    fn Sub(&self, node_proto: &onnx::NodeProto) {
        panic!("Sub has not been implemented");
    }

    fn Sum(&self, node_proto: &onnx::NodeProto) {
        panic!("Sum has not been implemented");
    }

    fn Tan(&self, node_proto: &onnx::NodeProto) {
        panic!("Tan has not been implemented");
    }

    fn Tanh(&self, node_proto: &onnx::NodeProto) {
        panic!("Tanh has not been implemented");
    }

    fn TfIdfVectorizer(&self, node_proto: &onnx::NodeProto) {
        panic!("TfIdfVectorizer has not been implemented");
    }

    fn Tile(&self, node_proto: &onnx::NodeProto) {
        panic!("Tile has not been implemented");
    }

    fn TopK(&self, node_proto: &onnx::NodeProto) {
        panic!("TopK has not been implemented");
    }

    fn Transpose(&self, node_proto: &onnx::NodeProto) {
        panic!("Transpose has not been implemented");
    }

    fn Trilu(&self, node_proto: &onnx::NodeProto) {
        panic!("Trilu has not been implemented");
    }

    fn Unique(&self, node_proto: &onnx::NodeProto) {
        panic!("Unique has not been implemented");
    }

    fn Unsqueeze(&self, node_proto: &onnx::NodeProto) {
        panic!("Unsqueeze has not been implemented");
    }

    fn Upsample(&self, node_proto: &onnx::NodeProto) {
        panic!("Upsample has not been implemented");
    }

    fn Where(&self, node_proto: &onnx::NodeProto) {
        panic!("Where has not been implemented");
    }

    fn Xor(&self, node_proto: &onnx::NodeProto) {
        panic!("Xor has not been implemented");
    }

    fn AffineGrid(&self, node_proto: &onnx::NodeProto) {
        panic!("AffineGrid has not been implemented");
    }

    fn Bernoulli(&self, node_proto: &onnx::NodeProto) {
        panic!("Bernoulli has not been implemented");
    }

    fn BlackmanWindow(&self, node_proto: &onnx::NodeProto) {
        panic!("BlackmanWindow has not been implemented");
    }

    fn CastLike(&self, node_proto: &onnx::NodeProto) {
        panic!("CastLike has not been implemented");
    }

    fn Celu(&self, node_proto: &onnx::NodeProto) {
        panic!("Celu has not been implemented");
    }

    fn CenterCropPad(&self, node_proto: &onnx::NodeProto) {
        panic!("CenterCropPad has not been implemented");
    }

    fn Clip(&self, node_proto: &onnx::NodeProto) {
        panic!("Clip has not been implemented");
    }

    fn DynamicQuantizeLinear(&self, node_proto: &onnx::NodeProto) {
        panic!("DynamicQuantizeLinear has not been implemented");
    }

    fn Elu(&self, node_proto: &onnx::NodeProto) {
        panic!("Elu has not been implemented");
    }

    fn Gelu(&self, node_proto: &onnx::NodeProto) {
        panic!("Gelu has not been implemented");
    }

    fn GreaterOrEqual(&self, node_proto: &onnx::NodeProto) {
        panic!("GreaterOrEqual has not been implemented");
    }

    fn GroupNormalization(&self, node_proto: &onnx::NodeProto) {
        panic!("GroupNormalization has not been implemented");
    }

    fn HammingWindow(&self, node_proto: &onnx::NodeProto) {
        panic!("HammingWindow has not been implemented");
    }

    fn HannWindow(&self, node_proto: &onnx::NodeProto) {
        panic!("HannWindow has not been implemented");
    }

    fn HardSigmoid(&self, node_proto: &onnx::NodeProto) {
        panic!("HardSigmoid has not been implemented");
    }

    fn HardSwish(&self, node_proto: &onnx::NodeProto) {
        panic!("HardSwish has not been implemented");
    }

    fn LayerNormalization(&self, node_proto: &onnx::NodeProto) {
        panic!("LayerNormalization has not been implemented");
    }

    fn LeakyRelu(&self, node_proto: &onnx::NodeProto) {
        panic!("LeakyRelu has not been implemented");
    }

    fn LessOrEqual(&self, node_proto: &onnx::NodeProto) {
        panic!("LessOrEqual has not been implemented");
    }

    fn LogSoftmax(&self, node_proto: &onnx::NodeProto) {
        panic!("LogSoftmax has not been implemented");
    }

    fn MeanVarianceNormalization(&self, node_proto: &onnx::NodeProto) {
        panic!("MeanVarianceNormalization has not been implemented");
    }

    fn Mish(&self, node_proto: &onnx::NodeProto) {
        panic!("Mish has not been implemented");
    }

    fn NegativeLogLikelihoodLoss(&self, node_proto: &onnx::NodeProto) {
        panic!("NegativeLogLikelihoodLoss has not been implemented");
    }

    fn PRelu(&self, node_proto: &onnx::NodeProto) {
        panic!("PRelu has not been implemented");
    }

    fn Range(&self, node_proto: &onnx::NodeProto) {
        panic!("Range has not been implemented");
    }

    fn ReduceL1(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceL1 has not been implemented");
    }

    fn ReduceL2(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceL2 has not been implemented");
    }

    fn ReduceLogSum(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceLogSum has not been implemented");
    }

    fn ReduceLogSumExp(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceLogSumExp has not been implemented");
    }

    fn ReduceSumSquare(&self, node_proto: &onnx::NodeProto) {
        panic!("ReduceSumSquare has not been implemented");
    }

    fn Relu(&self, node_proto: &onnx::NodeProto) {
        panic!("Relu has not been implemented");
    }

    fn Selu(&self, node_proto: &onnx::NodeProto) {
        panic!("Selu has not been implemented");
    }

    fn SequenceMap(&self, node_proto: &onnx::NodeProto) {
        panic!("SequenceMap has not been implemented");
    }

    fn Shrink(&self, node_proto: &onnx::NodeProto) {
        panic!("Shrink has not been implemented");
    }

    fn Softmax(&self, node_proto: &onnx::NodeProto) {
        panic!("Softmax has not been implemented");
    }

    fn SoftmaxCrossEntropyLoss(&self, node_proto: &onnx::NodeProto) {
        panic!("SoftmaxCrossEntropyLoss has not been implemented");
    }

    fn Softplus(&self, node_proto: &onnx::NodeProto) {
        panic!("Softplus has not been implemented");
    }

    fn Softsign(&self, node_proto: &onnx::NodeProto) {
        panic!("Softsign has not been implemented");
    }

    fn ThresholdedRelu(&self, node_proto: &onnx::NodeProto) {
        panic!("ThresholdedRelu has not been implemented");
    }
}
