use std::{cell::RefCell, rc::Rc};

use sdl2::keyboard::Keycode;

use crate::memory::Mem;

const JOYPAD_REG_ADDRESS: u16 = 0xFF00;

enum JoypadKey {
    RIGHT = 0,
    LEFT = 2,
    UP = 4,
    DOWN = 8,

    A = 16,
    B = 32,
    SELECT = 64,
    START = 128,
}

impl JoypadKey {
    fn from_keycode(keycode: Keycode) -> Option<JoypadKey> {
        match keycode {
            Keycode::UP => Some(JoypadKey::UP),
            Keycode::LEFT => Some(JoypadKey::LEFT),
            Keycode::DOWN => Some(JoypadKey::DOWN),
            Keycode::RIGHT => Some(JoypadKey::RIGHT),

            Keycode::Q => Some(JoypadKey::START),
            Keycode::W => Some(JoypadKey::SELECT),
            Keycode::A => Some(JoypadKey::B),
            Keycode::S => Some(JoypadKey::A),
            _ => None,
        }
    }
}

pub struct Joypad {
    mem: Rc<RefCell<Mem>>,
    inputs: u8,
}

impl Joypad {
    pub fn new(mem: Rc<RefCell<Mem>>) -> Self {
        return Joypad { mem, inputs: 0xCF };
    }
    pub fn keydown(&mut self, keycode: Keycode) {
        if let Some(key) = JoypadKey::from_keycode(keycode) {
            let current_input = self.mem.borrow().read_byte(JOYPAD_REG_ADDRESS);
            self.inputs &= !(key as u8);
            let mode = (current_input >> 4) & 3;
            let new_input = match mode {
                2 => mode | self.inputs & 0x0F,
                1 => mode | self.inputs >> 4,
                _ => 0x30 | 0x0F,
            };
            println!("keydown current {} new {}", current_input, new_input);
            self.mem.as_ref().borrow_mut().write_byte(
                JOYPAD_REG_ADDRESS,
                new_input,
            );
            let current_interrupt = self.mem.borrow().read_byte(0xFF0F);
            self.mem
                .as_ref()
                .borrow_mut()
                .write_byte(0xFF0F, current_interrupt | 0x10);
        };
    }

    pub fn keyup(&mut self, keycode: Keycode) {
        if let Some(key) = JoypadKey::from_keycode(keycode) {
            self.inputs |= key as u8;
            let current_input = self.mem.borrow().read_byte(JOYPAD_REG_ADDRESS);
            let mode = (current_input >> 4) & 3;
            let new_input = match mode {
                2 => mode | self.inputs & 0x0F,
                1 => mode | self.inputs >> 4,
                _ => 0x30 | 0x0F,
            };
            println!("keyup current {} new {}", current_input, new_input);
            self.mem
                .as_ref()
                .borrow_mut()
                .write_byte(JOYPAD_REG_ADDRESS, new_input);
        };
    }
}
