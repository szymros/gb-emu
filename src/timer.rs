use crate::memory::Mem;
use std::{cell::RefCell, rc::Rc};

const DIV_ADDRESS:usize = 0x04;
const TIMER_COUNTER_ADDRESS: usize = 0x05;
const TIMER_CONTROLL_ADDRESS: usize = 0x07;
const TIMER_MODULO_ADDRESS: usize = 0x05;
const INTERRUPT_FLAG_ADDRESS: usize = 0x0F;

pub struct Timer {
    counter: u8,
    mem: Rc<RefCell<Mem>>,
}

impl Timer {
    pub fn new(mem: Rc<RefCell<Mem>>, counter: u8) -> Self {
        return Timer { counter, mem };
    }
    pub fn update_timer(&mut self, cycles: u8) {
        let div = self.mem.borrow().io[DIV_ADDRESS].wrapping_add(cycles*4);
        self.mem.borrow_mut().io[DIV_ADDRESS] = div;
        let timer_controll = self.mem.borrow().io[TIMER_CONTROLL_ADDRESS];
        if timer_controll & 3 == 3 {
            let new_cycles = self.counter.overflowing_add(cycles);
            let timer_increment = match timer_controll & 2 {
                0 => new_cycles.1,
                1 => 4 >= self.counter,
                2 => 16 >= self.counter,
                3 => 64 >= self.counter,
                _ => 0 >= self.counter,
            };
            if timer_increment {
                let new_timer = self.mem.borrow().io[TIMER_COUNTER_ADDRESS].overflowing_add(1);
                let timer_modulo = self.mem.borrow().io[TIMER_MODULO_ADDRESS];
                if new_timer.1 {
                    self.mem.borrow_mut().io[TIMER_COUNTER_ADDRESS] = timer_modulo;
                    let current_interrupt = self.mem.borrow().io[INTERRUPT_FLAG_ADDRESS];
                    self.mem.borrow_mut().io[INTERRUPT_FLAG_ADDRESS] = current_interrupt | 4;
                }
            }
        }
    }
}
