use std::{cell::RefCell, rc::Rc};
use minifb::Key;


use crate::memory::Mem;

#[derive(Clone)]
pub struct Joypad {
    dpad: u8,
    buttons: u8,
    mem: Rc<RefCell<Mem>>,
}

impl Joypad {
    pub fn new(mem: Rc<RefCell<Mem>>) -> Self {
        return Joypad {
            dpad: 0xF,
            buttons: 0xF,
            mem,
        };
    }

    pub fn keydown(&mut self, keycode: Key) {
        match keycode {
            Key::Up => self.dpad &= 0b1011,
            Key::Left => self.dpad &= 0b1101,
            Key::Down => self.dpad &= 0b0111,
            Key::Right => self.dpad &= 0b1110,

            Key::Q => self.buttons &= 0b0111,
            Key::W => self.buttons &= 0b1011,
            Key::A => self.buttons &= 0b1101,
            Key::S => self.buttons &= 0b1110,
            _ => (),
        };
        let current_interrupt = self.mem.borrow().read_byte(0xFF0F);
        self.mem
            .as_ref()
            .borrow_mut()
            .write_byte(0xFF0F, current_interrupt | 0x10);
        self.update();
    }

    fn update(&mut self) {
        let mode = self.mem.borrow().joyselect & 0x30;
        if mode == 0x10 {
            self.mem.borrow_mut().io[0] = (self.buttons & 0x0F) | mode;
        }
        if mode == 0x20 {
            self.mem.borrow_mut().io[0] = (self.dpad & 0x0F) | mode;
        }
    }

    pub fn keyup(&mut self, keycode: Key) {
        match keycode {
            Key::Up => self.dpad |= 4,
            Key::Left => self.dpad |= 2,
            Key::Down => self.dpad |= 8,
            Key::Right => self.dpad |= 1,

            Key::Q => self.buttons |= 8,
            Key::W => self.buttons |= 4,
            Key::A => self.buttons |= 2,
            Key::S => self.buttons |= 1,
            _ => (),
        };
        self.update();
    }
}
