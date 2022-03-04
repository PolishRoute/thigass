struct PushConst {
    n: u32;
    width: u32;
    height: u32;
};

var<push_constant> config: PushConst;

[[stage(vertex)]]
fn vs_main([[builtin(vertex_index)]] in_vertex_index: u32) -> [[builtin(position)]] vec4<f32> {
    let o = vec3<f32>(f32(config.width) * 0.5, f32(config.height) * 0.5, 1.0);

    let tau = 3.14 * 2.0;
    let r = min(f32(config.width) / 2.0, f32(config.height) / 2.0);
    let alpha = f32(in_vertex_index) / f32(config.n) * tau;
    let x = o.x + r * sin(alpha);
    let y = o.y + r * cos(alpha);
    let p = vec3<f32>(x, y, 1.0);

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