mod cpu;
mod memory;
mod ppu;
use sdl2::pixels::PixelFormatEnum;
use std::rc::Rc;
use std::cell::RefCell;
use cpu::Cpu;
use env_logger::Env;
use memory::Mem;
use ppu::Ppu;
use sdl2::{
    self,
    event::Event,
    keyboard::Keycode,
    rect::Rect,
};
const WINDOW_WIDTH: u32 = 480;
const WINDOW_HEIGHT: u32 = 432;
const WINDOW_NAME: &str = "Rust SDL2";

fn main() {
    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();
    let rom = std::fs::read("./roms/pred.gb").unwrap();

    let mem = Rc::new(RefCell::new(Mem::setup(rom)));
    let mut cpu = Cpu::new(Rc::clone(&mem));
    let mut ppu = Ppu::new(Rc::clone(&mem));

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(WINDOW_NAME, WINDOW_WIDTH, WINDOW_HEIGHT)
        .position_centered()
        .build()
        .unwrap();
    let mut canvas: sdl2::render::Canvas<sdl2::video::Window> =
        window.into_canvas().build().unwrap();
    let texture_creator = canvas.texture_creator();
    let mut texture = texture_creator
        .create_texture(
            PixelFormatEnum::RGB24,
            sdl2::render::TextureAccess::Streaming,
            160,
            144,
        )
        .unwrap();
    texture
        .with_lock(None, |buffer: &mut [u8], pitch: usize| {
            for y in 0..144 {
                for x in 0..160 {
                    let offset = y * pitch + x;
                    buffer[offset] = 0x80;
                    buffer[offset + 1] = 0x80;
                    buffer[offset + 2] = 0x80;
                }
            }
        })
        .unwrap();
    canvas.clear();
    canvas
        .copy(&texture, None, Rect::new(0, 0, 160 * 2, 144 * 2))
        .unwrap();
    canvas.present();
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        cpu.cycles = 0;
        cpu.next();

        let pixels = ppu.next(cpu.cycles * 4);
        if let Some((ly, updated_line)) = pixels {
            texture
                .with_lock(None, |buffer: &mut [u8], pitch: usize| {
                    for x in 0..160 {
                        let offset: u32 = ly as u32 * pitch as u32 + x * 3;
                        buffer[offset as usize] = updated_line[x as usize];
                        buffer[(offset + 1) as usize] = updated_line[x as usize];
                        buffer[(offset + 2) as usize] = updated_line[x as usize];
                    }
                })
                .unwrap();
            canvas
                .copy(&texture, None, Rect::new(0, 0, 160 * 2, 144 * 2))
                .unwrap();
            canvas.present();
        }
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => {
                    break 'running;
                }
                _ => {}
            }
        }
    }
}
