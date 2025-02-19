mod cpu;
mod joypad;
mod memory;
mod ppu;
use cpu::Cpu;
use joypad::Joypad;
use memory::Mem;
use minifb::{Key, Window, WindowOptions};
use ppu::Ppu;
use std::cell::RefCell;
use std::rc::Rc;

const WINDOW_WIDTH: u32 = 480;
const WINDOW_HEIGHT: u32 = 432;

fn main() {
    let rom = std::fs::read("./roms/pred.gb").unwrap();

    let mem = Rc::new(RefCell::new(Mem::setup(rom)));
    let mut cpu = Cpu::new(Rc::clone(&mem));
    let mut ppu = Ppu::new(Rc::clone(&mem));
    let mut joypad = Joypad::new(Rc::clone(&mem));

    let mut buffer: Vec<u32> = vec![0; 160 * 144];
    let mut window = Window::new(
        "gbemu",
        WINDOW_WIDTH as usize,
        WINDOW_HEIGHT as usize,
        WindowOptions::default(),
    )
    .unwrap();
    window.set_target_fps(60);
    window.set_key_repeat_rate(0.001);
    window.set_key_repeat_delay(0.001);
    'running: loop {
        if !window.is_open() {
            break;
        }
        if window.is_key_down(Key::Escape) {
            break 'running;
        }
        let cycles = cpu.next();

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
            }
            if window.is_key_released(*key){
                joypad.keyup(*key)
            }
        }
    }
}
