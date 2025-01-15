mod cartridge;
use log::info;
use sdl2::pixels::PixelFormatEnum;
use std::io::prelude::*;
use std::rc::Rc;
use std::{borrow::BorrowMut, cell::RefCell};
mod cpu;
mod memory;
mod opcodes;
mod ppu;
use cpu::{Cpu, Flags, Registers};
use env_logger::Env;
use memory::Mem;
use ppu::Ppu;
use sdl2::{
    self,
    event::Event,
    keyboard::Keycode,
    rect::{Point, Rect},
};
const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;
const WINDOW_NAME: &str = "Rust SDL2";

fn main() {
    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();
    let mut bytes = std::fs::read("./roms/dmg-acid2.gb").unwrap();
    println!("{}", bytes.len());

    let mut array = bytes;
    let mut mem = Rc::new(RefCell::new(Mem::setup(array)));
    let mut gb = Cpu::new(Rc::clone(&mem));
    let mut ppu = Ppu::new(Rc::clone(&mem));

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(WINDOW_NAME, WIDTH, HEIGHT)
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
                    buffer[offset] = x as u8;
                    buffer[offset + 1] = y as u8;
                    buffer[offset + 2] = 0;
                }
            }
        })
        .unwrap();
    canvas.clear();
    canvas.copy(&texture, None, Rect::new(0, 0, 160, 144));
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut m = Rc::clone(&mem);
    let mut num = 0;
    'running: loop {
        gb.next();
        let pixels = ppu.check_mode(16);
        let ly = m.borrow().read_byte(0xFF44);
        match pixels {
            Some(new_line) => {
                texture
                    .with_lock(None, |buffer: &mut [u8], pitch: usize| {
                        for x in 0..160 {
                            let offset = ly * pitch as u8 + x;
                            buffer[offset as usize] = new_line[x as usize];
                            buffer[(offset + 1) as usize] = new_line[x as usize];
                            buffer[(offset + 2) as usize] = new_line[x as usize];
                        }
                    })
                    .unwrap();
                canvas.copy(&texture, None, Rect::new(0, 0, 160, 144));
            }
            None => (),
        }
        canvas.present();
        num += 1;
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        canvas.present();
    }
}
