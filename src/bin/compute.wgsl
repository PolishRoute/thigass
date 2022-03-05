struct TriangleIndices {
    data: [[stride(4)]] array<u32>;
};

struct TriangleVertices {
    data: [[stride(8)]] array<vec2<f32>>;
};

[[group(0), binding(0)]]
var<storage, write> v_indices: TriangleIndices;

[[group(1), binding(0)]]
var<storage, write> v_vertices: TriangleVertices;

[[stage(compute), workgroup_size(1)]]
fn cs_main([[builtin(global_invocation_id)]] global_id: vec3<u32>) {
    let idx = global_id.x;

    let width = 800.0;
    let height = 600.0;
    let o = vec2<f32>(width / 2.0, height / 2.0);

    let tau = 3.14 * 2.0;
    let r = 100.0;//min(f32(width) / 2.0, f32(height) / 2.0);

    let n = 10u;

    var i = 0u;

    v_vertices.data[0u] = o;
    var vertex_idx = 1u;


    loop {
        if (i > n) {
            break;
        }

        let a1 = f32(i) / f32(n) * tau;
        let a2 = f32((i + 1u) % n) / f32(n) * tau;

        let p1 = vec2<f32>(
            o.x + r * sin(a1),
            o.y + r * cos(a1)
        );
        let p2 = vec2<f32>(
            o.x + r * sin(a2),
            o.y + r * cos(a2)
        );

        v_indices.data[3u * i + 0u] = 0u;
        v_indices.data[3u * i + 1u] = i * 2u + 1u;
        v_indices.data[3u * i + 2u] = i * 2u + 2u;

        v_vertices.data[vertex_idx] = p2;
        vertex_idx = vertex_idx + 1u;
        v_vertices.data[vertex_idx] = p1;
        vertex_idx = vertex_idx + 1u;

        i = i + 1u;
    }
}