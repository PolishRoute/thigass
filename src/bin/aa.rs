use std::borrow::Cow;
use std::num::NonZeroU32;
use std::sync::Arc;
use wgpu::{BindingType, BufferBindingType, FrontFace, IndexFormat, Label, PolygonMode, PrimitiveTopology, PushConstantRange, ShaderStages};
use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::Window,
};

async fn run(event_loop: EventLoop<()>, window: Window) {
    let n = 20;

    let size = window.inner_size();
    let instance = wgpu::Instance::new(wgpu::Backends::all());
    let surface = unsafe { instance.create_surface(&window) };
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            force_fallback_adapter: false,
            // Request an adapter which can render to our surface
            compatible_surface: Some(&surface),
        })
        .await
        .expect("Failed to find an appropriate adapter");


    // Create the logical device and command queue
    let (device, queue) = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: None,
                features: wgpu::Features::PUSH_CONSTANTS | wgpu::Features::BUFFER_BINDING_ARRAY | wgpu::Features::STORAGE_RESOURCE_BINDING_ARRAY,
                // Make sure we use the texture resolution limits from the adapter, so we can support images the size of the swapchain.
                limits: wgpu::Limits {
                    max_push_constant_size: 12,
                    ..wgpu::Limits::downlevel_defaults()
                        .using_resolution(adapter.limits())
                },
            },
            None,
        )
        .await
        .expect("Failed to create device");

    let compute_shader = device.create_shader_module(&wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_str!("compute.wgsl"))),
    });

    let compute_pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: None,
        layout: None,
        module: &compute_shader,
        entry_point: "cs_main",
    });

    let triangles_indices_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: std::mem::size_of::<u32>() as u64 * 3 * n as u64,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::STORAGE,
        mapped_at_creation: false,
    });

    let triangles_vertices_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: "Vertex Buffer for triangles".into(),
        size: 2 * std::mem::size_of::<u32>() as u64 * n as u64,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::STORAGE,
        mapped_at_creation: false,
    });

    let compute_bind_group_layout = compute_pipeline.get_bind_group_layout(0);
    let compute_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &compute_bind_group_layout,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: triangles_indices_buffer.as_entire_binding(),
        }],
    });

    let compute_bind_group_layout2 = compute_pipeline.get_bind_group_layout(1);
    let compute_bind_group2 = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &compute_bind_group_layout2,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: triangles_vertices_buffer.as_entire_binding(),
        }],
    });

    // Load the shaders from disk
    let render_shader = device.create_shader_module(&wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_str!("shader.wgsl"))),
    });


    let bind_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: None,
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX,
                ty: BindingType::Buffer {
                    ty: BufferBindingType::Storage { read_only: true },
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(12 * n as u64),
                },
                count: None,
            }
        ],
    });

    let bind_layout2 = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: None,
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX,
                ty: BindingType::Buffer {
                    ty: BufferBindingType::Storage { read_only: true },
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(8 * n as u64),
                },
                count: None,
            }
        ],
    });


    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[&bind_layout, &bind_layout2],
        push_constant_ranges: &[PushConstantRange {
            stages: ShaderStages::VERTEX,
            range: 0..12,
        }],
    });

    let swapchain_format = surface.get_preferred_format(&adapter).unwrap();

    let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: None,
        layout: Some(&pipeline_layout),
        vertex: wgpu::VertexState {
            module: &render_shader,
            entry_point: "vs_main",
            buffers: &[],
        },
        fragment: Some(wgpu::FragmentState {
            module: &render_shader,
            entry_point: "fs_main",
            targets: &[swapchain_format.into()],
        }),
        primitive: wgpu::PrimitiveState {
            topology: PrimitiveTopology::TriangleList,
            polygon_mode: PolygonMode::Fill,
            cull_mode: None,
            ..wgpu::PrimitiveState::default()
        },
        depth_stencil: None,
        multisample: wgpu::MultisampleState::default(),
        multiview: None,
    });

    let render_bind_group_layout = render_pipeline.get_bind_group_layout(0);
    let render_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &render_bind_group_layout,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: triangles_indices_buffer.as_entire_binding(),
        }],
    });

    let render_bind_group_layout2 = render_pipeline.get_bind_group_layout(1);
    let render_bind_group2 = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &render_bind_group_layout2,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: triangles_vertices_buffer.as_entire_binding(),
        }],
    });

    let mut config = wgpu::SurfaceConfiguration {
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        format: swapchain_format,
        width: size.width,
        height: size.height,
        present_mode: wgpu::PresentMode::Mailbox,
    };

    surface.configure(&device, &config);

    event_loop.run(move |event, _, control_flow| {
        // Have the closure take ownership of the resources.
        // `event_loop.run` never returns, therefore we must do this to ensure
        // the resources are properly cleaned up.
        let _ = (&instance, &adapter, &render_shader, &pipeline_layout);

        *control_flow = ControlFlow::Wait;
        match event {
            Event::WindowEvent {
                event: WindowEvent::Resized(size),
                ..
            } => {
                // Reconfigure the surface with the new size
                config.width = size.width;
                config.height = size.height;
                surface.configure(&device, &config);
            }
            Event::RedrawRequested(_) => {
                let frame = surface
                    .get_current_texture()
                    .expect("Failed to acquire next swap chain texture");
                let view = frame
                    .texture
                    .create_view(&wgpu::TextureViewDescriptor::default());

                let mut encoder =
                    device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
                {
                    let mut cpass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                        label: None
                    });
                    cpass.set_pipeline(&compute_pipeline);
                    cpass.set_bind_group(0, &compute_bind_group, &[]);
                    cpass.set_bind_group(1, &compute_bind_group2, &[]);
                    cpass.dispatch(1, 1, 1);
                }

                {
                    let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                        label: None,
                        color_attachments: &[wgpu::RenderPassColorAttachment {
                            view: &view,
                            resolve_target: None,
                            ops: wgpu::Operations {
                                load: wgpu::LoadOp::Clear(wgpu::Color::GREEN),
                                store: true,
                            },
                        }],
                        depth_stencil_attachment: None,
                    });
                    rpass.set_pipeline(&render_pipeline);
                    rpass.set_bind_group(0, &render_bind_group, &[]);
                    rpass.set_bind_group(1, &render_bind_group2, &[]);
                    rpass.set_push_constants(ShaderStages::VERTEX, 0, bytemuck::cast_slice(&[
                        n,
                        window.inner_size().width,
                        window.inner_size().height
                    ]));
                    rpass.draw(0..n + 1, 0..1);
                }

                queue.submit(Some(encoder.finish()));

                dump_buffer::<u32>(&device, &triangles_indices_buffer);
                dump_buffer::<[f32; 2]>(&device, &triangles_vertices_buffer);

                frame.present();
            }
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => *control_flow = ControlFlow::Exit,
            _ => {}
        }
    });
}

fn dump_buffer<T: bytemuck::Pod + std::fmt::Debug>(device: &wgpu::Device, buffer: &wgpu::Buffer) {
    let buffer_slice = buffer.slice(..);
    let future = buffer_slice.map_async(wgpu::MapMode::Read);
    device.poll(wgpu::Maintain::Wait);
    pollster::block_on(future).unwrap();
    let data = buffer_slice.get_mapped_range();
    let x = bytemuck::cast_slice::<_, T>(&data);
    println!("{:?} = {:?}", &buffer, x);
    drop(data);
    buffer.unmap();
}

fn main() {
    env_logger::init();

    let event_loop = EventLoop::new();
    let window = winit::window::Window::new(&event_loop).unwrap();
    pollster::block_on(run(event_loop, window));
}
