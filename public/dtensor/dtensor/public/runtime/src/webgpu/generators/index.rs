use tensor::primitives::tensor::ScatterReduction;
use tensor::primitives::tensor::ViewType;

use crate::webgpu::generators::*;
use crate::webgpu::WORKGROUP_SIZE;

pub fn build_gather_shader(axis: ViewType) -> String {
    format!(
        "
{header}

{workgroup_stride}

const AXIS: u32 = {axis}u;

{input_interface}

{indices_interface}

{output_interface}

@compute {workgroup_size}
fn {entry_point}(
    @builtin(global_invocation_id) global_id: vec3u
) {{
    {index}

    // Guard against out-of-bounds work group sizes
    if index >= output_metadata.length {{
        return;
    }}

    {mapped_indices_index}

    var input_gather_shape: u32 = 1u;
    for (var i = AXIS + 1u; i < input_metadata.dimension; i++) {{
      input_gather_shape *= input_metadata.metadata[input_metadata.shape_offset + i];
    }}

    var mapped_index: u32 = 0u;

    // Batch dimensions are the same
    var mapped_index_temp = index;
    for (var i = 0u; i < AXIS; i++) {{
        let output_contiguous_stride = output_metadata.metadata[output_metadata.contiguous_stride_offset + i];
        let output_stride = output_metadata.metadata[output_metadata.stride_offset + i];
        let output_shape = output_metadata.metadata[output_metadata.shape_offset + i];

        let input_stride = input_metadata.metadata[input_metadata.stride_offset + i];
        let input_shape = input_metadata.metadata[input_metadata.shape_offset + i];

        let index_at_dimension = mapped_index_temp / output_contiguous_stride;
        mapped_index_temp -= (index_at_dimension % output_shape) * output_stride;
        mapped_index += (index_at_dimension % input_shape) * input_stride;
    }}

    // Axis index is given by indices
    let axis_index = u32(indices[mapped_indices_index]);
    let axis_stride = input_metadata.metadata[input_metadata.stride_offset + AXIS];
    let axis_shape = input_metadata.metadata[input_metadata.shape_offset + AXIS];
    mapped_index += (axis_index % axis_shape) * axis_stride;

    // Gather element dimension fungles
    mapped_index_temp = index % input_gather_shape;
    for (var i = AXIS + 1u; i < input_metadata.dimension; i++) {{
        // Output is contiguous so fungle this using input contiguous strides
        let contiguous_stride = input_metadata.metadata[input_metadata.contiguous_stride_offset + i];
        let stride = input_metadata.metadata[input_metadata.stride_offset + i];
        let shape = input_metadata.metadata[input_metadata.shape_offset + i];

        let index_at_dimension = mapped_index_temp / contiguous_stride;
        mapped_index_temp -= (index_at_dimension % shape) * contiguous_stride;
        mapped_index += (index_at_dimension % shape) * stride;
    }}

    output[index] = input[mapped_index];
}}
",
        header = shader_header(),
        workgroup_stride = WORKGROUP_SIZE.serialize_strides("WORKGROUP_STRIDE"),
        axis = axis.to_string(),
        input_interface = tensor_interface("0", "read", "input", "array<f32>", "input_metadata"),
        indices_interface =
            tensor_interface("1", "read", "indices", "array<f32>", "indices_metadata"),
        output_interface =
            tensor_interface("2", "read_write", "output", "array<f32>", "output_metadata"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        mapped_indices_index = compute_strided_offset(
            "mapped_indices_index",
            "index",
            "output_metadata",
            "indices_metadata"
        ),
    )
}

pub fn build_scatter_shader(axis: ViewType, reduction: ScatterReduction) -> String {
    let reduction_op = match reduction {
        ScatterReduction::None => |_, update| format!("{}", update),
        ScatterReduction::Add => |data, update| format!("{} + {}", data, update),
        ScatterReduction::Max => |data, update| format!("max({}, {})", data, update),
        ScatterReduction::Min => |data, update| format!("min({}, {})", data, update),
        ScatterReduction::Mul => |data, update| format!("{} * {}", data, update),
    };

    format!(
        "
{header}

{workgroup_stride}

const AXIS: u32 = {axis}u;

{indices_interface}

{updates_interface}

{output_interface}

@compute {workgroup_size}
fn {entry_point}(
  @builtin(global_invocation_id) global_id: vec3u
) {{
  {index}

  if index >= updates_metadata.length {{
      return;
  }}

  var mapped_index_temp = index;
  var mapped_index = 0u;

  // Map index, except with an override for AXIS
  for (var i = 0u; i < AXIS; i++) {{
    let indices_contiguous_stride = indices_metadata.metadata[indices_metadata.contiguous_stride_offset + i];
    let indices_shape = indices_metadata.metadata[indices_metadata.shape_offset + i];

    let output_stride = output_metadata.metadata[output_metadata.stride_offset + i];
    let output_shape = output_metadata.metadata[output_metadata.shape_offset + i];

    let index_at_dimension = mapped_index_temp / indices_contiguous_stride;
    mapped_index_temp -= (index_at_dimension % indices_shape) * indices_contiguous_stride;
    mapped_index += (index_at_dimension % output_shape) * output_stride;
  }}

  let axis_index = u32(indices[index]);
  let axis_stride = output_metadata.metadata[output_metadata.stride_offset + AXIS];
  mapped_index += axis_index * axis_stride;

  // Adjust temporary index as well
  let indices_contiguous_stride_at_axis = indices_metadata.metadata[indices_metadata.contiguous_stride_offset + AXIS];
  let indices_shape_at_axis = indices_metadata.metadata[indices_metadata.shape_offset + AXIS];
  let index_at_axis = mapped_index_temp / indices_contiguous_stride_at_axis;
  mapped_index_temp -= (index_at_axis % indices_shape_at_axis) * indices_contiguous_stride_at_axis;

  for (var i = AXIS + 1u; i < output_metadata.dimension; i++) {{
    let indices_contiguous_stride = indices_metadata.metadata[indices_metadata.contiguous_stride_offset + i];
    let indices_shape = indices_metadata.metadata[indices_metadata.shape_offset + i];

    let output_stride = output_metadata.metadata[output_metadata.stride_offset + i];
    let output_shape = output_metadata.metadata[output_metadata.shape_offset + i];

    let index_at_dimension = mapped_index_temp / indices_contiguous_stride;
    mapped_index_temp -= (index_at_dimension % indices_shape) * indices_contiguous_stride;
    mapped_index += (index_at_dimension % output_shape) * output_stride;
  }}

  // Barriers are needed because mapped indices may be the same both within
  // and outside the workgroup
  // Unfortunately, it appears that any synchronization provided by WebGPU does
  // not guarantee synchronization outside of the current workgroup
  // However, it does somewhat appear as if this _should_ work correctly on Vulkan
  // https://stackoverflow.com/questions/72035548/what-does-storagebarrier-in-webgpu-actually-do
  // https://github.com/gpuweb/gpuweb/pull/2297

  // Prevents multiple writes within the workgroup scope, not strictly necessary here
  workgroupBarrier();
  // Prevents multiple writes within the shader scope
  storageBarrier();

  output[mapped_index] = {operation_reduce};
}}
",
        header = shader_header(),
        workgroup_stride = WORKGROUP_SIZE.serialize_strides("WORKGROUP_STRIDE"),
        axis = axis,
        indices_interface =
            tensor_interface("0", "read", "indices", "array<f32>", "indices_metadata"),
        updates_interface =
            tensor_interface("1", "read", "updates", "array<f32>", "updates_metadata"),
        output_interface =
            tensor_interface("2", "read_write", "output", "array<f32>", "output_metadata"),
        workgroup_size = WORKGROUP_SIZE.serialize_decorator(),
        entry_point = "main",
        index = compute_index("index", "global_id", "WORKGROUP_STRIDE"),
        operation_reduce = reduction_op("output[mapped_index]", "updates[index]"),
    )
}
