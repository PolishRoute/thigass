struct PushConst {
    n: u32;
    width: u32;
    height: u32;
};

var<push_constant> config: PushConst;

struct TriangleIndices {
    data: [[stride(4)]] array<u32>;
};

struct TriangleVertices {
    data: [[stride(8)]] array<vec2<f32>>;
};

[[group(0), binding(0)]]
var<storage, read> v_indices: TriangleIndices;

[[group(1), binding(0)]]
var<storage, read> v_vertices: TriangleVertices;

[[stage(vertex)]]
fn vs_main([[builtin(vertex_index)]] in_vertex_index: u32) -> [[builtin(position)]] vec4<f32> {
    let p2 = v_vertices.data[v_indices.data[in_vertex_index] ];
    let p = vec3<f32>(p2.xy, 1.0);

    let m = mat2x3<f32>(
        2.0 / f32(config.width), 0.0, -1.0,
        0.0, 2.0 / f32(config.height), -1.0
    );
    let ox = p * m;

    return vec4<f32>(ox.x, ox.y, 0.0, 1.0);
}

[[stage(fragment)]]
fn fs_main() -> [[location(0)]] vec4<f32> {
    return vec4<f32>(1.0, 1.0, 0.0, 1.0);
}