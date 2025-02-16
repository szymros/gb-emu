mod cpu;
mod joypad;
mod memory;
mod ppu;
use cpu::Cpu;
use joypad::Joypad;
use memory::Mem;
use ppu::Ppu;
use sdl2::pixels::PixelFormatEnum;
use sdl2::{self, event::Event, rect::Rect};
use std::cell::RefCell;
use std::rc::Rc;
const WINDOW_WIDTH: u32 = 480;
const WINDOW_HEIGHT: u32 = 432;
const WINDOW_NAME: &str = "Rust SDL2";

fn main() {
    let rom = std::fs::read("./roms/pred.gb").unwrap();

    let mem = Rc::new(RefCell::new(Mem::setup(rom)));
    let mut cpu = Cpu::new(Rc::clone(&mem));
    let mut ppu = Ppu::new(Rc::clone(&mem));
    let mut joypad = Joypad::new(Rc::clone(&mem));

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
            PixelFormatEnum::RGB332,
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
        let cycles = cpu.next();

        match ppu.next(cycles * 4) {
            Some(updated_frame) => {
                texture
                    .with_lock(None, |buffer: &mut [u8], _: usize| {
                        buffer.copy_from_slice(&updated_frame);
                    })
                    .unwrap();
                canvas
                    .copy(&texture, None, Rect::new(0, 0, 160 * 2, 144 * 2))
                    .unwrap();
                canvas.present();
            }
            None => (),
        }
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => {
                    break 'running;
                }

                Event::KeyDown {
                    keycode: Some(keycode),
                    ..
                } => joypad.keydown(keycode),

                Event::KeyUp {
                    keycode: Some(keycode),
                    ..
                } => joypad.keyup(keycode),
                _ => {}
            }
        }
    }
}
