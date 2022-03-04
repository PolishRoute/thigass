struct TriangleIndices {
    data: [[stride(4)]] array<u32>;
};

[[group(0), binding(0)]]
var<storage, write> v_indices: TriangleIndices;

[[stage(compute), workgroup_size(1)]]
fn cs_main([[builtin(global_invocation_id)]] global_id: vec3<u32>) {
    v_indices.data[global_id.x] = global_id.x;
}