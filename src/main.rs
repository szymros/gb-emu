mod cartridge;
mod cpu;
mod opcodes;
use cpu::{Cpu, Flags, Registers};
use env_logger::Env;
use opcodes::{Arithemtic, Operation, R8};
use sdl2::{self, event::Event, keyboard::Keycode, pixels::Color};
use std::{convert::TryInto, time::Duration};
const WIDTH: u32 = 800;
const HEIGHT: u32 = 600;
const WINDOW_NAME: &str = "Rust SDL2";
const WINDDOW_BORDER: i32 = 20;

fn main() {
    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();
    let mut bytes = std::fs::read("./11-op a,(hl).gb").unwrap();
    // let arr: [u8; 32768] = bytes.try_into().unwrap();
    let mut array = [0u8; 0xFFFF + 1]; // Initialize an array of 65535 elements with zeros
    let len = bytes.len().min(0xFFFF + 1); // Take the minimum of the Vec length and 65535
    array[..len].copy_from_slice(&bytes[..len]); // Copy elements from the Vec into the array
    let mut gb = Cpu::new(array);
    // gb.set_thingy();
    // let mut i = 0;
    // loop {
    //     if i == 4000000 {
    //         break;
    //     }
    //     i += 1;
    //     gb.log();
    //     gb.next();
    // }

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(WINDOW_NAME, WIDTH, HEIGHT)
        .position_centered()
        .build()
        .unwrap();
    let mut canvas: sdl2::render::Canvas<sdl2::video::Window> =
        window.into_canvas().build().unwrap();
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();

    'running: loop {
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

        // canvas.clear();
        // canvas.present();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 30));
        // The rest of the game loop goes here...
    }
}
