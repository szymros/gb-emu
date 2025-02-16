use crate::memory::Mem;
use std::{cell::RefCell, rc::Rc, u16, u8};

const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

const LD_R8_R8_SRC_MASK: u8 = 0b111;
const LD_R8_R8_DST_MASK: u8 = 0b111000;
const R16_BLOCK1_MASK: u8 = 0b110000;
const R8CB_MASK: u8 = 0b111;
const BIT3CB_MASK: u8 = 0b111000;
const TGT3_MASK: u8 = 0b00111000;

#[derive(Copy, Clone)]
pub struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: Flags,
    h: u8,
    l: u8,
    stack_pointer: u16,
}

impl Registers {
    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }
    fn set_bc(&mut self, value: u16) {
        self.b = (value >> 8) as u8;
        self.c = value as u8;
    }

    fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }
    fn set_de(&mut self, value: u16) {
        self.d = (value >> 8) as u8;
        self.e = value as u8;
    }

    fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }
    fn set_hl(&mut self, value: u16) {
        self.h = (value >> 8) as u8;
        self.l = (value) as u8;
    }

    fn get_af(&self) -> u16 {
        return (self.a as u16) << 8 | self.f.to_u8() as u16;
    }
    fn set_af(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f.zero = value as u8 & 0x80 == 0x80;
        self.f.substract = value as u8 & 0x40 == 0x40;
        self.f.half_carry = value as u8 & 0x20 == 0x20;
        self.f.carry = value as u8 & 0x10 == 0x10;
    }
}

#[derive(Copy, Clone)]
pub struct Flags {
    zero: bool,
    substract: bool,
    half_carry: bool,
    carry: bool,
}

impl Flags {
    fn from_u8(value: u8) -> Self {
        Flags {
            zero: value >> ZERO_FLAG_BYTE_POSITION & 0b1 != 0,
            substract: value >> SUBTRACT_FLAG_BYTE_POSITION & 0b1 != 0,
            half_carry: value >> HALF_CARRY_FLAG_BYTE_POSITION & 0b1 != 0,
            carry: value >> CARRY_FLAG_BYTE_POSITION & 0b1 != 0,
        }
    }
    fn to_u8(&self) -> u8 {
        (if self.zero { 1 } else { 0 }) << ZERO_FLAG_BYTE_POSITION
            | (if self.substract { 1 } else { 0 }) << SUBTRACT_FLAG_BYTE_POSITION
            | (if self.half_carry { 1 } else { 0 }) << HALF_CARRY_FLAG_BYTE_POSITION
            | (if self.carry { 1 } else { 0 }) << CARRY_FLAG_BYTE_POSITION
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum R8 {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    HL_PTR = 6,
    A = 7,
}
impl R8 {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match (byte & mask) >> mask.trailing_zeros() {
            0 => Some(R8::B),
            1 => Some(R8::C),
            2 => Some(R8::D),
            3 => Some(R8::E),
            4 => Some(R8::H),
            5 => Some(R8::L),
            6 => Some(R8::HL_PTR),
            7 => Some(R8::A),
            _ => None,
        }
    }
}

pub struct Cpu {
    pub(crate) registers: Registers,
    pub(crate) program_counter: u16,
    pub memory: Rc<RefCell<Mem>>,
    pub cycles: u16,
    halted: bool,
    interrupts_enabled: bool,
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<Mem>>) -> Cpu {
        let registers = Registers {
            a: 0x01,
            b: 0x00,
            c: 0x13,

            d: 0x00,
            e: 0xD8,
            f: Flags::from_u8(0xB0),

            h: 0x01,
            l: 0x4D,
            stack_pointer: 0xFFFE,
        };
        return Cpu {
            registers,
            program_counter: 0x0100,
            memory,
            cycles: 0,
            halted: false,
            interrupts_enabled: false,
        };
    }

    pub fn log(&mut self) {
        // A:00 F:11 B:22 C:33 D:44 E:55 H:66 L:77 SP:8888 PC:9999 PCMEM:AA,BB,CC,DD
        let pcmem1 = self.memory.borrow().read_byte(self.program_counter);
        let pcmem2 = self.memory.borrow().read_byte(self.program_counter + 1);
        let pcmem3 = self.memory.borrow_mut().read_byte(self.program_counter + 2);
        let pcmem4 = self.memory.borrow_mut().read_byte(self.program_counter + 3);
        println!("A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}",
            self.registers.a,self.registers.f.to_u8(), self.registers.b, self.registers.c,  self.registers.d, self.registers.e, self.registers.h,self.registers.l,
            self.registers.stack_pointer,self.program_counter, pcmem1,pcmem2,pcmem3,pcmem4);
    }

    fn handle_interrupt(&mut self) {
        if !self.interrupts_enabled && !self.halted {
            return;
        };
        let interrupt_enable = self.memory.borrow().read_byte(0xFFFF);
        let interrupt_flag = self.memory.borrow().read_byte(0xFF0F);
        let interrupts = interrupt_flag & interrupt_enable;
        if interrupts == 0x00 {
            return;
        }
        let interrupt_index = interrupts.trailing_zeros();
        let new_flag = interrupt_flag & !(1 << interrupt_index);
        self.memory.borrow_mut().write_byte(0xFF0F, new_flag);
        self.interrupts_enabled = false;
        self.halted = false;
        self.push(self.program_counter);
        self.program_counter = 0x40 + interrupt_index as u16 * 8;
        self.cycles += 5
    }

    fn ld_imm16_ptr_sp(&mut self) {
        let imm16 = self.memory.borrow().read_word(self.program_counter + 1);
        self.memory
            .borrow_mut()
            .write_byte(imm16, self.registers.stack_pointer as u8);
        self.memory.borrow_mut().write_byte(
            imm16.wrapping_add(1),
            (self.registers.stack_pointer >> 8) as u8,
        );
    }

    fn add_hl_r16(&mut self, r16_value: u16) {
        let hl_value = self.registers.get_hl();
        let (result, overflow) = hl_value.overflowing_add(r16_value);
        self.registers.f.substract = false;
        self.registers.f.carry = overflow;
        self.registers.f.half_carry = ((hl_value & 0xFFF) + (r16_value & 0xFFF)) & 0x1000 == 0x1000;
        self.registers.set_hl(result);
    }

    fn ld_r8_r8(&mut self, source: R8, target: R8) {
        let value = match source {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.borrow().read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        match target {
            R8::B => self.registers.b = value,
            R8::C => self.registers.c = value,
            R8::D => self.registers.d = value,
            R8::E => self.registers.e = value,
            R8::H => self.registers.h = value,
            R8::L => self.registers.l = value,
            R8::HL_PTR => self
                .memory
                .borrow_mut()
                .write_byte(self.registers.get_hl(), value),
            R8::A => self.registers.a = value,
        };
    }

    fn add_and_set_flags(&mut self, target: u8, value: u8, carry: bool, half_carry: bool) -> u8 {
        let (result, overflow) = target.overflowing_add(value);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        if carry {
            self.registers.f.carry = overflow;
        }
        if half_carry {
            self.registers.f.half_carry = ((target & 0b1111) + (value & 0b1111)) & 0x10 == 0x10;
        }
        return result;
    }

    fn adc_and_set_flags(&mut self, target: u8, value: u8) -> u8 {
        let c = if self.registers.f.carry { 1 } else { 0 };
        let result_with_carry = target.wrapping_add(value).wrapping_add(c);
        self.registers.f.zero = result_with_carry == 0;
        self.registers.f.substract = false;
        self.registers.f.carry = target as u16 + value as u16 + c as u16 > 0xFF;
        self.registers.f.half_carry =
            ((target & 0b1111) + (value & 0b1111) + (c & 0b1111)) > 0b1111;

        return result_with_carry;
    }

    fn sub_and_set_flags(&mut self, target: u8, value: u8, carry: bool, half_carry: bool) -> u8 {
        let (result, overflow) = target.overflowing_sub(value);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = true;
        if half_carry {
            self.registers.f.half_carry = (target & 0b1111) < (value & 0b1111);
        }
        if carry {
            self.registers.f.carry = overflow
        }
        return result;
    }

    fn sbc_and_set_flags(&mut self, target: u8, value: u8) -> u8 {
        let c = if self.registers.f.carry { 1 } else { 0 };
        let result = target.wrapping_sub(value).wrapping_sub(c);
        self.registers.f.carry = (target as u16) < (value as u16 + c as u16);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = (target & 0b1111) < ((value & 0b1111) + c);
        return result;
    }

    fn and_and_set_flags(&mut self, target: u8, value: u8) -> u8 {
        let result = target & value;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = true;
        self.registers.f.carry = false;
        return result;
    }

    fn xor_and_set_flags(&mut self, target: u8, value: u8) -> u8 {
        let result = target ^ value;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        return result;
    }

    fn or_and_set_flags(&mut self, target: u8, value: u8) -> u8 {
        let result = target | value;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        return result;
    }

    fn cp_and_set_flags(&mut self, target: u8, value: u8) {
        let result = target.wrapping_sub(value);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = (target & 0b1111) < value & 0b1111;
        self.registers.f.carry = value > target;
    }

    // rotate left
    fn rl(&mut self, value: u8) -> u8 {
        let new_carry = value & 0x80 == 0x80;
        let rotated = (value << 1) | (if self.registers.f.carry { 1 } else { 0 });
        self.registers.f.carry = new_carry;
        self.registers.f.zero = rotated == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        return rotated;
    }

    // rotate left circular
    fn rlc(&mut self, value: u8) -> u8 {
        let new_carry = value & 0x80 == 0x80;
        let rotated = (value << 1) | (if new_carry { 1 } else { 0 });
        self.registers.f.carry = new_carry;
        self.registers.f.zero = rotated == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        return rotated;
    }

    // rotate right
    fn rr(&mut self, value: u8) -> u8 {
        let carry = value & 0x01 == 0x01;
        let rotated = (value >> 1) | (if self.registers.f.carry { 0x80 } else { 0 });
        self.registers.f.carry = carry;
        self.registers.f.zero = rotated == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        return rotated;
    }

    // rotate right circural
    fn rrc(&mut self, value: u8) -> u8 {
        let carry = value & 0x01 == 0x01;
        let rotated = (value >> 1) | (if carry { 0x80 } else { 0 });
        self.registers.f.carry = carry;
        self.registers.f.zero = rotated == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        return rotated;
    }

    // adjust a to be binary decimal encoded
    fn daa(&mut self) {
        let mut a = self.registers.a;
        if !self.registers.f.substract {
            if self.registers.f.carry || a > 0x99 {
                a = a.wrapping_add(0x60);
                self.registers.f.carry = true;
            }
            if self.registers.f.half_carry || (a & 0x0f) > 0x09 {
                a = a.wrapping_add(0x06);
            }
        } else {
            if self.registers.f.carry {
                a = a.wrapping_sub(0x60);
            }
            if self.registers.f.half_carry {
                a = a.wrapping_sub(0x06);
            }
        }
        self.registers.a = a;
        self.registers.f.zero = a == 0;
        self.registers.f.half_carry = false;
    }

    // complement a
    fn cpl(&mut self) {
        self.registers.a = !self.registers.a;
        self.registers.f.half_carry = true;
        self.registers.f.substract = true;
    }

    //set carry flag;
    fn scf(&mut self) {
        self.registers.f.carry = true;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
    }

    // complement carry Flag
    fn ccf(&mut self) {
        self.registers.f.carry = !self.registers.f.carry;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
    }

    fn jr_imm8(&mut self) {
        let offset = self.memory.borrow().read_byte(self.program_counter + 1);
        let n = offset as i8;
        self.program_counter = (((self.program_counter + 2) as u16) as i16 + n as i16) as u16;
    }

    fn push(&mut self, value: u16) {
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
        self.memory
            .borrow_mut()
            .write_byte(self.registers.stack_pointer, ((value & 0xFF00) >> 8) as u8);
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
        self.memory
            .borrow_mut()
            .write_byte(self.registers.stack_pointer, (value & 0xFF) as u8);
    }

    fn pop(&mut self) -> u16 {
        let lower_byte = self.memory.borrow().read_byte(self.registers.stack_pointer) as u16;
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1);
        let higher_byte = self.memory.borrow().read_byte(self.registers.stack_pointer) as u16;
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1);
        return (higher_byte << 8) | lower_byte;
    }

    fn ret(&mut self) {
        let program_counter = self.pop();
        self.program_counter = program_counter;
    }

    fn ei(&mut self) {
        self.interrupts_enabled = true;
        self.memory.borrow_mut().interrupt_enabled = 1;
    }

    fn di(&mut self) {
        self.interrupts_enabled = false;
        self.memory.borrow_mut().interrupt_enabled = 0;
    }

    fn jp(&mut self, address: u16) {
        self.program_counter = address;
    }

    fn call(&mut self, address: u16) {
        self.push(self.program_counter + 1);
        self.jp(address);
    }

    fn add_sp_imm8(&mut self, imm8: u8) -> u16 {
        let target = self.registers.stack_pointer;
        let result = (target).wrapping_add((imm8 as i8) as u16);
        let (_, overflow) = (target as u8).overflowing_add(imm8);
        self.registers.f.zero = false;
        self.registers.f.substract = false;
        self.registers.f.carry = overflow;
        self.registers.f.half_carry = ((target & 0b1111) + (imm8 as u16 & 0b1111)) & 0x10 == 0x10;
        return result;
    }

    fn sra(&mut self, value: u8) -> u8 {
        let c = value & 0x01 == 0x01;
        let shifted = ((value as i8) >> 1) as u8;
        self.registers.f.zero = shifted == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = c;
        return shifted;
    }

    fn sla(&mut self, value: u8) -> u8 {
        let c = value & 0x80 == 0x80;
        let shifted = ((value as i8) << 1) as u8;
        self.registers.f.zero = shifted == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = c;
        return shifted;
    }

    fn srl(&mut self, value: u8) -> u8 {
        let c = value & 0x01 == 0x01;
        let shifted = value >> 1;
        self.registers.f.zero = shifted == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = c;
        return shifted;
    }

    fn swap(&mut self, value: u8) -> u8 {
        let higher_bits = value >> 4;
        let lower_bits = value << 4;
        let result = higher_bits | lower_bits;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        return result;
    }

    fn bit(&mut self, register: R8, index: u8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.borrow().read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let t = (value >> index) & 0x01 == 0x01;
        self.registers.f.zero = !t;
        self.registers.f.substract = false;
        self.registers.f.half_carry = true;
    }

    fn set(&mut self, register: R8, index: u8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.borrow().read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = value | (0x01 << index);
        match register {
            R8::B => self.registers.b = result,
            R8::C => self.registers.c = result,
            R8::D => self.registers.d = result,
            R8::E => self.registers.e = result,
            R8::H => self.registers.h = result,
            R8::L => self.registers.l = result,
            R8::HL_PTR => self
                .memory
                .borrow_mut()
                .write_byte(self.registers.get_hl(), result),
            R8::A => self.registers.a = result,
        };
    }

    fn res(&mut self, register: R8, index: u8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.borrow().read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = value & !(0x01 << index);
        match register {
            R8::B => self.registers.b = result,
            R8::C => self.registers.c = result,
            R8::D => self.registers.d = result,
            R8::E => self.registers.e = result,
            R8::H => self.registers.h = result,
            R8::L => self.registers.l = result,
            R8::HL_PTR => self
                .memory
                .borrow_mut()
                .write_byte(self.registers.get_hl(), result),
            R8::A => self.registers.a = result,
        };
    }

    fn execute_instruction(&mut self, byte: u8) -> u16{
        let mut cycles = 0;
        match byte {
            //nop
            0x00 => {
                self.program_counter += 1;
                cycles += 1;
            }
            //stop
            0x10 => {
                self.program_counter += 2;
                cycles += 1;
            }
            // halt
            0x76 => {
                self.halted = true;
                self.program_counter += 1;
            }

            // ---------------------- BLOCK 1 ----------------------
            // ld dst, src

            // ld r16, imm16
            0x01 => {
                let imm16 = self.memory.borrow().read_word(self.program_counter + 1);
                self.registers.set_bc(imm16);
                self.program_counter += 3;
                cycles += 3;
            }
            0x11 => {
                let imm16 = self.memory.borrow().read_word(self.program_counter + 1);
                self.registers.set_de(imm16);
                self.program_counter += 3;
                cycles += 3;
            }
            0x21 => {
                let imm16 = self.memory.borrow().read_word(self.program_counter + 1);
                self.registers.set_hl(imm16);
                self.program_counter += 3;
                cycles += 3;
            }
            0x31 => {
                let imm16 = self.memory.borrow().read_word(self.program_counter + 1);
                self.registers.stack_pointer = imm16;
                self.program_counter += 3;
                cycles += 3;
            }
            // ld r16mem, a
            0x02 => {
                let address = self.registers.get_bc();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.a);
                self.program_counter += 1;
                cycles += 2;
            }
            0x12 => {
                let address = self.registers.get_de();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.a);
                self.program_counter += 1;
                cycles += 2;
            }
            0x22 => {
                let address = self.registers.get_hl();
                self.registers.set_hl(address.wrapping_add(1));
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.a);
                self.program_counter += 1;
                cycles += 2;
            }
            0x32 => {
                let address = self.registers.get_hl();
                self.registers.set_hl(address.wrapping_sub(1));
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.a);
                self.program_counter += 1;
                cycles += 2;
            }

            // ld a, r16mem
            0x0A => {
                let address = self.registers.get_bc();
                self.registers.a = self.memory.borrow().read_byte(address);
                self.program_counter += 1;
                cycles += 2;
            }
            0x1A => {
                let address = self.registers.get_de();
                self.registers.a = self.memory.borrow().read_byte(address);
                self.program_counter += 1;
                cycles += 2;
            }
            0x2A => {
                let address = self.registers.get_hl();
                self.registers.set_hl(address.wrapping_add(1));
                self.registers.a = self.memory.borrow().read_byte(address);
                self.program_counter += 1;
                cycles += 2;
            }
            0x3A => {
                let address = self.registers.get_hl();
                self.registers.set_hl(address.wrapping_sub(1));
                self.registers.a = self.memory.borrow().read_byte(address);
                self.program_counter += 1;
                cycles += 2;
            }

            // ld imm16, sp
            8 => {
                self.ld_imm16_ptr_sp();
                self.program_counter += 3;
                cycles += 5;
            }

            // inc r16
            0x03 => {
                self.registers
                    .set_bc(self.registers.get_bc().wrapping_add(1));
                self.program_counter += 1;
                cycles += 2;
            }
            0x13 => {
                self.registers
                    .set_de(self.registers.get_de().wrapping_add(1));
                self.program_counter += 1;
                cycles += 2;
            }
            0x23 => {
                self.registers
                    .set_hl(self.registers.get_hl().wrapping_add(1));
                self.program_counter += 1;
                cycles += 2;
            }
            0x33 => {
                self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1);
                self.program_counter += 1;
                cycles += 2;
            }
            // dec r16
            0x0B => {
                self.registers
                    .set_bc(self.registers.get_bc().wrapping_sub(1));
                self.program_counter += 1;
                cycles += 2;
            }
            0x1B => {
                self.registers
                    .set_de(self.registers.get_de().wrapping_sub(1));
                self.program_counter += 1;
                cycles += 2;
            }
            0x2B => {
                self.registers
                    .set_hl(self.registers.get_hl().wrapping_sub(1));
                self.program_counter += 1;
                cycles += 2;
            }
            0x3B => {
                self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
                self.program_counter += 1;
                cycles += 2;
            }
            // add hl, r16
            0x09 => {
                self.add_hl_r16(self.registers.get_bc());
                self.program_counter += 1;
                cycles += 2;
            }
            0x19 => {
                self.add_hl_r16(self.registers.get_de());
                self.program_counter += 1;
                cycles += 2;
            }
            0x29 => {
                self.add_hl_r16(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x39 => {
                self.add_hl_r16(self.registers.stack_pointer);
                self.program_counter += 1;
                cycles += 2;
            }

            // inc r8
            0x04 => {
                self.registers.b = self.add_and_set_flags(self.registers.b, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x0C => {
                self.registers.c = self.add_and_set_flags(self.registers.c, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x14 => {
                self.registers.d = self.add_and_set_flags(self.registers.d, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x1C => {
                self.registers.e = self.add_and_set_flags(self.registers.e, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x24 => {
                self.registers.h = self.add_and_set_flags(self.registers.h, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x2C => {
                self.registers.l = self.add_and_set_flags(self.registers.l, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x3C => {
                self.registers.a = self.add_and_set_flags(self.registers.a, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x34 => {
                let address = self.registers.get_hl();
                let val = self.memory.borrow().read_byte(address);
                let result = self.add_and_set_flags(val, 1, false, true);
                self.memory.borrow_mut().write_byte(address, result);
                self.program_counter += 1;
                cycles += 3;
            }

            // dec r8
            0x05 => {
                self.registers.b = self.sub_and_set_flags(self.registers.b, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x0D => {
                self.registers.c = self.sub_and_set_flags(self.registers.c, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x15 => {
                self.registers.d = self.sub_and_set_flags(self.registers.d, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x1D => {
                self.registers.e = self.sub_and_set_flags(self.registers.e, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x25 => {
                self.registers.h = self.sub_and_set_flags(self.registers.h, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x2D => {
                self.registers.l = self.sub_and_set_flags(self.registers.l, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x3D => {
                self.registers.a = self.sub_and_set_flags(self.registers.a, 1, false, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x35 => {
                let address = self.registers.get_hl();
                let val = self.memory.borrow().read_byte(address);
                let result = self.sub_and_set_flags(val, 1, false, true);
                self.memory.borrow_mut().write_byte(address, result);
                self.program_counter += 1;
                cycles += 3;
            }

            // ld r8, imm8
            0x06 => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.b = imm8;
                self.program_counter += 2;
                cycles += 2;
            }
            0x0E => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.c = imm8;
                self.program_counter += 2;
                cycles += 2;
            }
            0x16 => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.d = imm8;
                self.program_counter += 2;
                cycles += 2;
            }
            0x1E => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.e = imm8;
                self.program_counter += 2;
                cycles += 2;
            }
            0x26 => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.h = imm8;
                self.program_counter += 2;
                cycles += 2;
            }
            0x2E => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.l = imm8;
                self.program_counter += 2;
                cycles += 2;
            }
            0x3E => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.a = imm8;
                self.program_counter += 2;
                cycles += 2;
            }
            0x36 => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                let address = self.registers.get_hl();
                self.memory.borrow_mut().write_byte(address, imm8);
                self.program_counter += 2;
                cycles += 3;
            }

            0x07 => {
                self.registers.a = self.rlc(self.registers.a);
                self.registers.f.zero = false;
                self.program_counter += 1;
                cycles += 1;
            }
            0x17 => {
                self.registers.a = self.rl(self.registers.a);
                self.registers.f.zero = false;
                self.program_counter += 1;
                cycles += 1;
            }
            0x27 => {
                self.daa();
                self.program_counter += 1;
                cycles += 1;
            }
            0x37 => {
                self.scf();
                self.program_counter += 1;
                cycles += 1;
            }
            0x0F => {
                self.registers.a = self.rrc(self.registers.a);
                self.registers.f.zero = false;
                self.program_counter += 1;
                cycles += 1;
            }
            0x1F => {
                self.registers.a = self.rr(self.registers.a);
                self.registers.f.zero = false;
                self.program_counter += 1;
                cycles += 1;
            }
            0x2F => {
                self.cpl();
                self.program_counter += 1;
                cycles += 1;
            }
            0x3F => {
                self.ccf();
                self.program_counter += 1;
                cycles += 1;
            }

            // jr imm8
            0x18 => {
                self.jr_imm8();
                cycles += 3;
            }

            // jr cond imm8
            0x20 => {
                if self.registers.f.zero == false {
                    self.jr_imm8()
                } else {
                    self.program_counter += 2;
                }
                cycles += 3;
            }
            0x28 => {
                if self.registers.f.zero == true {
                    self.jr_imm8()
                } else {
                    self.program_counter += 2;
                }
                cycles += 3;
            }
            0x30 => {
                if self.registers.f.carry == false {
                    self.jr_imm8()
                } else {
                    self.program_counter += 2;
                }
                cycles += 3;
            }
            0x38 => {
                if self.registers.f.carry == true {
                    self.jr_imm8()
                } else {
                    self.program_counter += 2;
                }
                cycles += 3;
            }

            // ------------------------------ Block 2 load r8 r8 ------------------------------
            // ld r8 r8, 0 1 r8 dst r8 src
            //0x40..=0x6F | 0x70..=0x75 | 0x77..=0x7F => {
            //    self.ld_r8_r8(
            //        R8::from_masked_u8(byte, LD_R8_R8_SRC_MASK).unwrap(),
            //        R8::from_masked_u8(byte, LD_R8_R8_DST_MASK).unwrap(),
            //    );
            //    self.program_counter += 1;
            //    cycles += 1;
            //}
            0x40 => {
                self.registers.b = self.registers.b;
                self.program_counter += 1;
                cycles += 1;
            }
            0x41 => {
                self.registers.b = self.registers.c;
                self.program_counter += 1;
                cycles += 1;
            }
            0x42 => {
                self.registers.b = self.registers.d;
                self.program_counter += 1;
                cycles += 1;
            }
            0x43 => {
                self.registers.b = self.registers.e;
                self.program_counter += 1;
                cycles += 1;
            }
            0x44 => {
                self.registers.b = self.registers.h;
                self.program_counter += 1;
                cycles += 1;
            }
            0x45 => {
                self.registers.b = self.registers.l;
                self.program_counter += 1;
                cycles += 1;
            }
            0x46 => {
                self.registers.b = self.memory.borrow().read_byte(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x47 => {
                self.registers.b = self.registers.a;
                self.program_counter += 1;
                cycles += 1;
            }
            0x48 => {
                self.registers.c = self.registers.b;
                self.program_counter += 1;
                cycles += 1;
            }
            0x49 => {
                self.registers.c = self.registers.c;
                self.program_counter += 1;
                cycles += 1;
            }
            0x4A => {
                self.registers.c = self.registers.d;
                self.program_counter += 1;
                cycles += 1;
            }
            0x4B => {
                self.registers.c = self.registers.e;
                self.program_counter += 1;
                cycles += 1;
            }
            0x4C => {
                self.registers.c = self.registers.h;
                self.program_counter += 1;
                cycles += 1;
            }
            0x4D => {
                self.registers.c = self.registers.l;
                self.program_counter += 1;
                cycles += 1;
            }
            0x4E => {
                self.registers.c = self.memory.borrow().read_byte(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x4F => {
                self.registers.c = self.registers.a;
                self.program_counter += 1;
                cycles += 1;
            }
            0x50 => {
                self.registers.d = self.registers.b;
                self.program_counter += 1;
                cycles += 1;
            }
            0x51 => {
                self.registers.d = self.registers.c;
                self.program_counter += 1;
                cycles += 1;
            }
            0x52 => {
                self.registers.d = self.registers.d;
                self.program_counter += 1;
                cycles += 1;
            }
            0x53 => {
                self.registers.d = self.registers.e;
                self.program_counter += 1;
                cycles += 1;
            }
            0x54 => {
                self.registers.d = self.registers.h;
                self.program_counter += 1;
                cycles += 1;
            }
            0x55 => {
                self.registers.d = self.registers.l;
                self.program_counter += 1;
                cycles += 1;
            }
            0x56 => {
                self.registers.d = self.memory.borrow().read_byte(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x57 => {
                self.registers.d = self.registers.a;
                self.program_counter += 1;
                cycles += 1;
            }
            0x58 => {
                self.registers.e = self.registers.b;
                self.program_counter += 1;
                cycles += 1;
            }
            0x59 => {
                self.registers.e = self.registers.c;
                self.program_counter += 1;
                cycles += 1;
            }
            0x5A => {
                self.registers.e = self.registers.d;
                self.program_counter += 1;
                cycles += 1;
            }
            0x5B => {
                self.registers.e = self.registers.e;
                self.program_counter += 1;
                cycles += 1;
            }
            0x5C => {
                self.registers.e = self.registers.h;
                self.program_counter += 1;
                cycles += 1;
            }
            0x5D => {
                self.registers.e = self.registers.l;
                self.program_counter += 1;
                cycles += 1;
            }
            0x5E => {
                self.registers.e = self.memory.borrow().read_byte(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x5F => {
                self.registers.e = self.registers.a;
                self.program_counter += 1;
                cycles += 1;
            }
            0x60 => {
                self.registers.h = self.registers.b;
                self.program_counter += 1;
                cycles += 1;
            }
            0x61 => {
                self.registers.h = self.registers.c;
                self.program_counter += 1;
                cycles += 1;
            }
            0x62 => {
                self.registers.h = self.registers.d;
                self.program_counter += 1;
                cycles += 1;
            }
            0x63 => {
                self.registers.h = self.registers.e;
                self.program_counter += 1;
                cycles += 1;
            }
            0x64 => {
                self.registers.h = self.registers.h;
                self.program_counter += 1;
                cycles += 1;
            }
            0x65 => {
                self.registers.h = self.registers.l;
                self.program_counter += 1;
                cycles += 1;
            }
            0x66 => {
                self.registers.h = self.memory.borrow().read_byte(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x67 => {
                self.registers.h = self.registers.a;
                self.program_counter += 1;
                cycles += 1;
            }
            0x68 => {
                self.registers.l = self.registers.b;
                self.program_counter += 1;
                cycles += 1;
            }
            0x69 => {
                self.registers.l = self.registers.c;
                self.program_counter += 1;
                cycles += 1;
            }
            0x6A => {
                self.registers.l = self.registers.d;
                self.program_counter += 1;
                cycles += 1;
            }
            0x6B => {
                self.registers.l = self.registers.e;
                self.program_counter += 1;
                cycles += 1;
            }
            0x6C => {
                self.registers.l = self.registers.h;
                self.program_counter += 1;
                cycles += 1;
            }
            0x6D => {
                self.registers.l = self.registers.l;
                self.program_counter += 1;
                cycles += 1;
            }
            0x6E => {
                self.registers.l = self.memory.borrow().read_byte(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x6F => {
                self.registers.l = self.registers.a;
                self.program_counter += 1;
                cycles += 1;
            }

            0x70 => {
                let address = self.registers.get_hl();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.b);
                self.program_counter += 1;
                cycles += 2;
            }
            0x71 => {
                let address = self.registers.get_hl();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.c);
                self.program_counter += 1;
                cycles += 2;
            }
            0x72 => {
                let address = self.registers.get_hl();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.d);
                self.program_counter += 1;
                cycles += 2;
            }
            0x73 => {
                let address = self.registers.get_hl();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.e);
                self.program_counter += 1;
                cycles += 2;
            }
            0x74 => {
                let address = self.registers.get_hl();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.h);
                self.program_counter += 1;
                cycles += 2;
            }
            0x75 => {
                let address = self.registers.get_hl();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.l);
                self.program_counter += 1;
                cycles += 2;
            }
            0x77 => {
                let address = self.registers.get_hl();
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.a);
                self.program_counter += 1;
                cycles += 2;
            }
            0x78 => {
                self.registers.a = self.registers.b;
                self.program_counter += 1;
                cycles += 1;
            }
            0x79 => {
                self.registers.a = self.registers.c;
                self.program_counter += 1;
                cycles += 1;
            }
            0x7A => {
                self.registers.a = self.registers.d;
                self.program_counter += 1;
                cycles += 1;
            }
            0x7B => {
                self.registers.a = self.registers.e;
                self.program_counter += 1;
                cycles += 1;
            }
            0x7C => {
                self.registers.a = self.registers.h;
                self.program_counter += 1;
                cycles += 1;
            }
            0x7D => {
                self.registers.a = self.registers.l;
                self.program_counter += 1;
                cycles += 1;
            }
            0x7E => {
                self.registers.a = self.memory.borrow().read_byte(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 2;
            }
            0x7F => {
                self.registers.a = self.registers.a;
                self.program_counter += 1;
                cycles += 1;
            }
            // ------------- Block 3 Arithmetic ------------

            // add a, r8	1	0	0	0	0	Operand (r8)
            0x80 => {
                self.registers.a =
                    self.add_and_set_flags(self.registers.a, self.registers.b, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x81 => {
                self.registers.a =
                    self.add_and_set_flags(self.registers.a, self.registers.c, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x82 => {
                self.registers.a =
                    self.add_and_set_flags(self.registers.a, self.registers.d, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x83 => {
                self.registers.a =
                    self.add_and_set_flags(self.registers.a, self.registers.e, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x84 => {
                self.registers.a =
                    self.add_and_set_flags(self.registers.a, self.registers.h, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x85 => {
                self.registers.a =
                    self.add_and_set_flags(self.registers.a, self.registers.l, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x86 => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.registers.a = self.add_and_set_flags(self.registers.a, value, true, true);
                self.program_counter += 1;
            }
            0x87 => {
                self.registers.a =
                    self.add_and_set_flags(self.registers.a, self.registers.a, true, true);
                self.program_counter += 1;
                cycles += 1;
            }

            // adc a, r8	1	0	0	0	1	Operand (r8)
            0x88 => {
                self.registers.a = self.adc_and_set_flags(self.registers.a, self.registers.b);
                self.program_counter += 1;
                cycles += 1;
            }
            0x89 => {
                self.registers.a = self.adc_and_set_flags(self.registers.a, self.registers.c);
                self.program_counter += 1;
                cycles += 1;
            }
            0x8A => {
                self.registers.a = self.adc_and_set_flags(self.registers.a, self.registers.d);
                self.program_counter += 1;
                cycles += 1;
            }
            0x8B => {
                self.registers.a = self.adc_and_set_flags(self.registers.a, self.registers.e);
                self.program_counter += 1;
                cycles += 1;
            }
            0x8C => {
                self.registers.a = self.adc_and_set_flags(self.registers.a, self.registers.h);
                self.program_counter += 1;
                cycles += 1;
            }
            0x8D => {
                self.registers.a = self.adc_and_set_flags(self.registers.a, self.registers.l);
                self.program_counter += 1;
                cycles += 1;
            }
            0x8E => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.registers.a = self.adc_and_set_flags(self.registers.a, value);
                self.program_counter += 1;
                cycles += 1;
            }
            0x8F => {
                self.registers.a = self.adc_and_set_flags(self.registers.a, self.registers.a);
                self.program_counter += 1;
                cycles += 1;
            }

            // sub a, r8	1	0	0	1	0	Operand (r8)
            0x90 => {
                self.registers.a =
                    self.sub_and_set_flags(self.registers.a, self.registers.b, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x91 => {
                self.registers.a =
                    self.sub_and_set_flags(self.registers.a, self.registers.c, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x92 => {
                self.registers.a =
                    self.sub_and_set_flags(self.registers.a, self.registers.d, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x93 => {
                self.registers.a =
                    self.sub_and_set_flags(self.registers.a, self.registers.e, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x94 => {
                self.registers.a =
                    self.sub_and_set_flags(self.registers.a, self.registers.h, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x95 => {
                self.registers.a =
                    self.sub_and_set_flags(self.registers.a, self.registers.l, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x96 => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.registers.a = self.sub_and_set_flags(self.registers.a, value, true, true);
                self.program_counter += 1;
                cycles += 1;
            }
            0x97 => {
                self.registers.a =
                    self.sub_and_set_flags(self.registers.a, self.registers.a, true, true);
                self.program_counter += 1;
                cycles += 1;
            }

            // sbc a, r8	1	0	0	1	1	Operand (r8)
            0x98 => {
                self.registers.a = self.sbc_and_set_flags(self.registers.a, self.registers.b);
                self.program_counter += 1;
                cycles += 1;
            }
            0x99 => {
                self.registers.a = self.sbc_and_set_flags(self.registers.a, self.registers.c);
                self.program_counter += 1;
                cycles += 1;
            }
            0x9A => {
                self.registers.a = self.sbc_and_set_flags(self.registers.a, self.registers.d);
                self.program_counter += 1;
                cycles += 1;
            }
            0x9B => {
                self.registers.a = self.sbc_and_set_flags(self.registers.a, self.registers.e);
                self.program_counter += 1;
                cycles += 1;
            }
            0x9C => {
                self.registers.a = self.sbc_and_set_flags(self.registers.a, self.registers.h);
                self.program_counter += 1;
                cycles += 1;
            }
            0x9D => {
                self.registers.a = self.sbc_and_set_flags(self.registers.a, self.registers.l);
                self.program_counter += 1;
                cycles += 1;
            }
            0x9E => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.registers.a = self.sbc_and_set_flags(self.registers.a, value);
                self.program_counter += 1;
                cycles += 1;
            }
            0x9F => {
                self.registers.a = self.sbc_and_set_flags(self.registers.a, self.registers.a);
                self.program_counter += 1;
                cycles += 1;
            }

            // and a, r8	1	0	1	0	0	Operand (r8)
            0xA0 => {
                self.registers.a = self.and_and_set_flags(self.registers.a, self.registers.b);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA1 => {
                self.registers.a = self.and_and_set_flags(self.registers.a, self.registers.c);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA2 => {
                self.registers.a = self.and_and_set_flags(self.registers.a, self.registers.d);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA3 => {
                self.registers.a = self.and_and_set_flags(self.registers.a, self.registers.e);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA4 => {
                self.registers.a = self.and_and_set_flags(self.registers.a, self.registers.h);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA5 => {
                self.registers.a = self.and_and_set_flags(self.registers.a, self.registers.l);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA6 => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.registers.a = self.and_and_set_flags(self.registers.a, value);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA7 => {
                self.registers.a = self.and_and_set_flags(self.registers.a, self.registers.a);
                self.program_counter += 1;
                cycles += 1;
            }

            // xor a, r8	1	0	1	0	1	Operand (r8)
            0xA8 => {
                self.registers.a = self.xor_and_set_flags(self.registers.a, self.registers.b);
                self.program_counter += 1;
                cycles += 1;
            }
            0xA9 => {
                self.registers.a = self.xor_and_set_flags(self.registers.a, self.registers.c);
                self.program_counter += 1;
                cycles += 1;
            }
            0xAA => {
                self.registers.a = self.xor_and_set_flags(self.registers.a, self.registers.d);
                self.program_counter += 1;
                cycles += 1;
            }
            0xAB => {
                self.registers.a = self.xor_and_set_flags(self.registers.a, self.registers.e);
                self.program_counter += 1;
                cycles += 1;
            }
            0xAC => {
                self.registers.a = self.xor_and_set_flags(self.registers.a, self.registers.h);
                self.program_counter += 1;
                cycles += 1;
            }
            0xAD => {
                self.registers.a = self.xor_and_set_flags(self.registers.a, self.registers.l);
                self.program_counter += 1;
                cycles += 1;
            }
            0xAE => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.registers.a = self.xor_and_set_flags(self.registers.a, value);
                self.program_counter += 1;
                cycles += 1;
            }
            0xAF => {
                self.registers.a = self.xor_and_set_flags(self.registers.a, self.registers.a);
                self.program_counter += 1;
                cycles += 1;
            }

            // or a, r8	1	0	1	1	0	Operand (r8)
            0xB0 => {
                self.registers.a = self.or_and_set_flags(self.registers.a, self.registers.b);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB1 => {
                self.registers.a = self.or_and_set_flags(self.registers.a, self.registers.c);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB2 => {
                self.registers.a = self.or_and_set_flags(self.registers.a, self.registers.d);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB3 => {
                self.registers.a = self.or_and_set_flags(self.registers.a, self.registers.e);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB4 => {
                self.registers.a = self.or_and_set_flags(self.registers.a, self.registers.h);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB5 => {
                self.registers.a = self.or_and_set_flags(self.registers.a, self.registers.l);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB6 => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.registers.a = self.or_and_set_flags(self.registers.a, value);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB7 => {
                self.registers.a = self.or_and_set_flags(self.registers.a, self.registers.a);
                self.program_counter += 1;
                cycles += 1;
            }

            // cp a, r8	1	0	1	1	1	Operand (r8)
            0xB8 => {
                self.cp_and_set_flags(self.registers.a, self.registers.b);
                self.program_counter += 1;
                cycles += 1;
            }
            0xB9 => {
                self.cp_and_set_flags(self.registers.a, self.registers.c);
                self.program_counter += 1;
                cycles += 1;
            }
            0xBA => {
                self.cp_and_set_flags(self.registers.a, self.registers.d);
                self.program_counter += 1;
                cycles += 1;
            }
            0xBB => {
                self.cp_and_set_flags(self.registers.a, self.registers.e);
                self.program_counter += 1;
                cycles += 1;
            }
            0xBC => {
                self.cp_and_set_flags(self.registers.a, self.registers.h);
                self.program_counter += 1;
                cycles += 1;
            }
            0xBD => {
                self.cp_and_set_flags(self.registers.a, self.registers.l);
                self.program_counter += 1;
                cycles += 1;
            }
            0xBE => {
                let address = self.registers.get_hl();
                let value = self.memory.borrow().read_byte(address);
                self.cp_and_set_flags(self.registers.a, value);
                self.program_counter += 1;
                cycles += 1;
            }
            0xBF => {
                self.cp_and_set_flags(self.registers.a, self.registers.a);
                self.program_counter += 1;
                cycles += 1;
            }

            // ---------------------------------- Block 4 ----------------------------------
            // add a, imm8
            0xC6 => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.add_and_set_flags(self.registers.a, value, true, true);
                self.registers.a = result;
                self.program_counter += 2;
                cycles += 2;
            }

            // adc a, imm8
            0xCE => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.adc_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
                cycles += 2;
            }

            // sub a, imm8
            0xD6 => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.sub_and_set_flags(self.registers.a, value, true, true);
                self.registers.a = result;
                self.program_counter += 2;
                cycles += 2;
            }

            // sbc a, imm8
            0xDE => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.sbc_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
                cycles += 2;
            }

            // and a, imm8
            0xE6 => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.and_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
                cycles += 2;
            }

            // xor a, imm8
            0xEE => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.xor_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
                cycles += 2;
            }

            // or a, imm8
            0xF6 => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.or_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
                cycles += 2;
            }

            // cp a, imm8
            0xFE => {
                let value = self.memory.borrow().read_byte(self.program_counter + 1);
                self.cp_and_set_flags(self.registers.a, value);
                self.program_counter += 2;
                cycles += 2;
            }

            // ret cond
            0xC0 => {
                if self.registers.f.zero == false {
                    self.ret();
                    cycles += 5;
                } else {
                    self.program_counter += 1;
                    cycles += 2;
                }
            }
            0xC8 => {
                if self.registers.f.zero == true {
                    self.ret();
                    cycles += 5;
                } else {
                    self.program_counter += 1;
                    cycles += 2;
                }
            }
            0xD0 => {
                if self.registers.f.carry == false {
                    self.ret();
                    cycles += 5;
                } else {
                    self.program_counter += 1;
                    cycles += 2;
                }
            }
            0xD8 => {
                if self.registers.f.carry == true {
                    self.ret();
                    cycles += 5;
                } else {
                    self.program_counter += 1;
                    cycles += 2;
                }
            }
            // ret
            0xC9 => {
                self.ret();
                cycles += 4;
            }
            // RetI,
            0xD9 => {
                self.ei();
                self.ret();
                cycles += 4;
            }
            // JpCondImm16(Condition),
            0xC2 => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.zero == false {
                    self.jp(address);
                    cycles += 4;
                } else {
                    self.program_counter += 1;
                    cycles += 3;
                }
            }
            0xCA => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.zero == true {
                    self.jp(address);
                    cycles += 4;
                } else {
                    self.program_counter += 1;
                    cycles += 3;
                }
            }
            0xD2 => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.carry == false {
                    cycles += 4;
                    self.jp(address)
                } else {
                    self.program_counter += 1;
                    cycles += 3;
                }
            }
            0xDA => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.carry == true {
                    cycles += 4;
                    self.jp(address)
                } else {
                    self.program_counter += 1;
                    cycles += 3;
                }
            }
            // JpImm16,
            0xC3 => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.jp(address);
                cycles += 4;
            }
            // JpHl,
            0xE9 => {
                self.jp(self.registers.get_hl());
                cycles += 1;
            }
            // CallCondImm16(Condition),
            0xC4 => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.zero == false {
                    cycles += 6;
                    self.call(address)
                } else {
                    cycles += 3;
                    self.program_counter += 1;
                }
            }
            0xCC => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.zero == true {
                    cycles += 6;
                    self.call(address)
                } else {
                    cycles += 3;
                    self.program_counter += 1;
                }
            }
            0xD4 => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.carry == false {
                    cycles += 6;
                    self.call(address)
                } else {
                    self.program_counter += 1;
                    cycles += 3;
                }
            }
            0xDC => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                if self.registers.f.carry == true {
                    cycles += 6;
                    self.call(address)
                } else {
                    self.program_counter += 1;
                    cycles += 3;
                }
            }
            // CallImm16,
            0xCD => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.program_counter += 2;
                self.call(address);
                cycles += 6;
            }
            // RstTgt3,
            0xC7 | 0xCF | 0xD7 | 0xDF | 0xE7 | 0xEF | 0xF7 | 0xFF => {
                let address = (byte & TGT3_MASK) >> TGT3_MASK.trailing_zeros();
                self.call(address as u16 * 8);
                cycles += 4;
            }
            // PopR16(R16Stk),
            0xC1 => {
                let value = self.pop();
                self.registers.set_bc(value);
                self.program_counter += 1;
                cycles += 3;
            }
            0xD1 => {
                let value = self.pop();
                self.registers.set_de(value);
                self.program_counter += 1;
                cycles += 3;
            }
            0xE1 => {
                let value = self.pop();
                self.registers.set_hl(value);
                self.program_counter += 1;
                cycles += 3;
            }
            0xF1 => {
                let value = self.pop();
                self.registers.set_af(value);
                self.program_counter += 1;
                cycles += 3;
            }
            // PushR16(R16Stk),
            0xC5 => {
                self.push(self.registers.get_bc());
                self.program_counter += 1;
                cycles += 4;
            }
            0xD5 => {
                self.push(self.registers.get_de());
                self.program_counter += 1;
                cycles += 4;
            }
            0xE5 => {
                self.push(self.registers.get_hl());
                self.program_counter += 1;
                cycles += 4;
            }
            0xF5 => {
                self.push(self.registers.get_af());
                self.program_counter += 1;
                cycles += 4;
            }
            // LdhCAddrA,
            0xE2 => {
                self.memory
                    .borrow_mut()
                    .write_byte(0xFF00 + self.registers.c as u16, self.registers.a);
                self.program_counter += 1;
                cycles += 2;
            }
            // LdhImm8AddrA,
            0xE0 => {
                let offset = self.memory.borrow().read_byte(self.program_counter + 1);
                self.memory
                    .borrow_mut()
                    .write_byte(0xFF00 + offset as u16, self.registers.a);
                self.program_counter += 2;
                cycles += 3;
            }
            // LdImm16AddrA,
            0xEA => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.memory
                    .borrow_mut()
                    .write_byte(address, self.registers.a);
                self.program_counter += 3;
                cycles += 4;
            }
            // LdhACAddr,
            0xF2 => {
                self.registers.a = self
                    .memory
                    .borrow()
                    .read_byte(0xFF00 + self.registers.c as u16);
                self.program_counter += 1;
                cycles += 3;
            }
            // LdhAImm8Addr,
            0xF0 => {
                let offset = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.a = self.memory.borrow().read_byte(0xFF00 | offset as u16);
                self.program_counter += 2;
                cycles += 3;
            }
            // LdAImm16Addr,
            0xFA => {
                let address = self.memory.borrow().read_word(self.program_counter + 1);
                self.registers.a = self.memory.borrow().read_byte(address);
                self.program_counter += 3;
                cycles += 4;
            }
            // AddSpImm8,
            0xE8 => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                self.registers.stack_pointer = self.add_sp_imm8(imm8);
                self.program_counter += 2;
                cycles += 4;
            }
            // LdHlSpImm8,
            0xF8 => {
                let imm8 = self.memory.borrow().read_byte(self.program_counter + 1);
                let result = self.add_sp_imm8(imm8);
                self.registers.set_hl(result);
                self.program_counter += 2;
                cycles += 3;
            }
            // LdSpHl,
            0xF9 => {
                self.registers.stack_pointer = self.registers.get_hl();
                self.program_counter += 1;
                cycles += 2;
            }
            // Di,
            0xF3 => {
                self.di();
                self.interrupts_enabled = false;
                self.program_counter += 1;
                cycles += 1;
            }
            // Ei,
            0xFB => {
                self.ei();
                self.interrupts_enabled = true;
                self.program_counter += 1;
                cycles += 1;
            }

            // ---------------------- CB PREFIXED OPCODES ----------------------
            0xCB => {
                let prefixed = self.memory.borrow().read_byte(self.program_counter + 1);
                match prefixed {
                    0x0 => {
                        self.registers.b = self.rlc(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x01 => {
                        self.registers.c = self.rlc(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x02 => {
                        self.registers.d = self.rlc(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x03 => {
                        self.registers.e = self.rlc(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x04 => {
                        self.registers.h = self.rlc(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x05 => {
                        self.registers.l = self.rlc(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x06 => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.rlc(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x07 => {
                        self.registers.a = self.rlc(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x08 => {
                        self.registers.b = self.rrc(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x09 => {
                        self.registers.c = self.rrc(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x0A => {
                        self.registers.d = self.rrc(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x0B => {
                        self.registers.e = self.rrc(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x0C => {
                        self.registers.h = self.rrc(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x0D => {
                        self.registers.l = self.rrc(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x0E => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.rrc(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x0F => {
                        self.registers.a = self.rrc(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x10 => {
                        self.registers.b = self.rl(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x11 => {
                        self.registers.c = self.rl(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x12 => {
                        self.registers.d = self.rl(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x13 => {
                        self.registers.e = self.rl(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x14 => {
                        self.registers.h = self.rl(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x15 => {
                        self.registers.l = self.rl(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x16 => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.rl(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x17 => {
                        self.registers.a = self.rl(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x18 => {
                        self.registers.b = self.rr(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x19 => {
                        self.registers.c = self.rr(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x1A => {
                        self.registers.d = self.rr(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x1B => {
                        self.registers.e = self.rr(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x1C => {
                        self.registers.h = self.rr(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x1D => {
                        self.registers.l = self.rr(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x1E => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.rr(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x1F => {
                        self.registers.a = self.rr(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x20 => {
                        self.registers.b = self.sla(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x21 => {
                        self.registers.c = self.sla(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x22 => {
                        self.registers.d = self.sla(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x23 => {
                        self.registers.e = self.sla(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x24 => {
                        self.registers.h = self.sla(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x25 => {
                        self.registers.l = self.sla(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x26 => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.sla(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x27 => {
                        self.registers.a = self.sla(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x28 => {
                        self.registers.b = self.sra(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x29 => {
                        self.registers.c = self.sra(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x2A => {
                        self.registers.d = self.sra(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x2B => {
                        self.registers.e = self.sra(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x2C => {
                        self.registers.h = self.sra(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x2D => {
                        self.registers.l = self.sra(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x2E => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.sra(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x2F => {
                        self.registers.a = self.sra(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x30 => {
                        self.registers.b = self.swap(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x31 => {
                        self.registers.c = self.swap(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x32 => {
                        self.registers.d = self.swap(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x33 => {
                        self.registers.e = self.swap(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x34 => {
                        self.registers.h = self.swap(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x35 => {
                        self.registers.l = self.swap(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x36 => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.swap(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x37 => {
                        self.registers.a = self.swap(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x38 => {
                        self.registers.b = self.srl(self.registers.b);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x39 => {
                        self.registers.c = self.srl(self.registers.c);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x3A => {
                        self.registers.d = self.srl(self.registers.d);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x3B => {
                        self.registers.e = self.srl(self.registers.e);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x3C => {
                        self.registers.h = self.srl(self.registers.h);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x3D => {
                        self.registers.l = self.srl(self.registers.l);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x3E => {
                        let address = self.registers.get_hl();
                        let value = self.memory.borrow().read_byte(address);
                        let result = self.srl(value);
                        self.memory.borrow_mut().write_byte(address, result);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x3F => {
                        self.registers.a = self.srl(self.registers.a);
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x40..=0x7F => {
                        self.bit(
                            R8::from_masked_u8(prefixed, R8CB_MASK).unwrap(),
                            (prefixed & BIT3CB_MASK) >> BIT3CB_MASK.trailing_zeros(),
                        );
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0x80..=0xBF => {
                        self.res(
                            R8::from_masked_u8(prefixed, R8CB_MASK).unwrap(),
                            (prefixed & BIT3CB_MASK) >> BIT3CB_MASK.trailing_zeros(),
                        );
                        self.program_counter += 2;
                        cycles += 2;
                    }
                    0xC0..=0xFF => {
                        self.set(
                            R8::from_masked_u8(prefixed, R8CB_MASK).unwrap(),
                            (prefixed & BIT3CB_MASK) >> BIT3CB_MASK.trailing_zeros(),
                        );
                        self.program_counter += 2;
                        cycles += 2;
                    }
                };
            }
            _ => {}
        }
        return cycles
    }

    pub fn next(&mut self) -> u16{
        self.handle_interrupt();
        let cycles = if !self.halted {
            let current_byte = self.memory.borrow().read_byte(self.program_counter);
            self.execute_instruction(current_byte)
        } else {
            1
        };
        let div = self.memory.borrow().read_byte(0xFF04);
        self.memory
            .borrow_mut()
            .write_byte(0xFF04, div.wrapping_add(cycles as u8 * 4));
        return cycles;
    }
}
