mod cpu;
mod joypad;
mod memory;
mod ppu;
mod timer;
use cpu::Cpu;
use joypad::Joypad;
use memory::Mem;
use minifb::{Key, Window, WindowOptions};
use ppu::Ppu;
use std::cell::RefCell;
use std::env;
use std::rc::Rc;
use timer::Timer;

const WINDOW_WIDTH: u32 = 480;
const WINDOW_HEIGHT: u32 = 432;

fn main() {
    let args: Vec<String> = env::args().collect();
    let rom_path = &args[1];
    let rom = std::fs::read(rom_path).unwrap();

    let mem = Rc::new(RefCell::new(Mem::setup(rom)));
    let mut cpu = Cpu::new(Rc::clone(&mem));
    let mut ppu = Ppu::new(Rc::clone(&mem));
    let mut joypad = Joypad::new(Rc::clone(&mem));
    let mut timer = Timer::new(Rc::clone(&mem), 0);

    let mut buffer: Vec<u32> = vec![0; 160 * 144];
    let mut window = Window::new(
        "gbemu",
        WINDOW_WIDTH as usize,
        WINDOW_HEIGHT as usize,
        WindowOptions::default(),
    )
    .unwrap();
    window.set_target_fps(60);
    window.set_key_repeat_rate(0.01);
    window.set_key_repeat_delay(0.01);

    loop {
        if !window.is_open() || window.is_key_down(Key::Escape) {
            break;
        }
        let cycles = cpu.next();
        timer.update_timer(cycles as u8);

        match ppu.next(cycles * 4) {
            Some(updated_frame) => {
                buffer.copy_from_slice(&updated_frame);
                window.update_with_buffer(&buffer, 160, 144).unwrap()
            }
            None => (),
        }

        let keys = vec![
            Key::A,
            Key::S,
            Key::Q,
            Key::W,
            Key::Up,
            Key::Down,
            Key::Left,
            Key::Right,
        ];
        for key in &keys {
            if window.is_key_down(*key) {
                joypad.keydown(*key);
            } else {
                joypad.keyup(*key)
            }
        }
    }
}
