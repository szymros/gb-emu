use log::info;
use std::{result, u16, u8};

const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

#[derive(Copy, Clone)]
pub struct Registers {
    pub(crate) a: u8,
    pub(crate) b: u8,
    pub(crate) c: u8,
    pub(crate) d: u8,
    pub(crate) e: u8,
    pub(crate) f: Flags,
    pub(crate) h: u8,
    pub(crate) l: u8,
    pub(crate) stack_pointer: u16,
    pub(crate) program_counter: u16,
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

    fn get_af(&mut self) -> u16 {
        return (self.a as u16) << 8 | self.f.to_u8() as u16;
    }
    fn set_af(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f.zero = value as u8 & 0b10000000 == 0b10000000;
        self.f.substract = value as u8 & 0b01000000 == 0b01000000;
        self.f.half_carry = value as u8 & 0b00100000 == 0b00100000;
        self.f.carry = value as u8 & 0b00010000 == 0b00010000;
    }
}

#[derive(Copy, Clone)]
pub struct Flags {
    pub(crate) zero: bool,
    pub(crate) substract: bool,
    pub(crate) half_carry: bool,
    pub(crate) carry: bool,
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

#[derive(Copy, Clone)]
struct Bus {
    pub memory: [u8; 0xFFFF + 1],
}
impl Bus {
    fn read_byte(&self, address: u16) -> u8 {
        return self.memory[address as usize];
    }
    fn read_word(&self, address: u16) -> u16 {
        return (self.read_byte(address) as u16)
            | ((self.read_byte(address.wrapping_add(1)) as u16) << 8);
    }
    fn write_byte(&mut self, address: u16, byte: u8) {
        self.memory[address as usize] = byte;
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
#[derive(Copy, Clone)]
enum R16Mem {
    BC = 0,
    DE = 1,
    HLI = 2,
    HLD = 3,
}
impl R16Mem {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match (byte & mask) >> mask.trailing_zeros() {
            0 => Some(R16Mem::BC),
            1 => Some(R16Mem::DE),
            2 => Some(R16Mem::HLI),
            3 => Some(R16Mem::HLD),
            _ => None,
        }
    }
}
#[derive(Copy, Clone)]
enum R16 {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
}
impl R16 {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match (byte & mask) >> mask.trailing_zeros() {
            0 => Some(R16::BC),
            1 => Some(R16::DE),
            2 => Some(R16::HL),
            3 => Some(R16::SP),
            _ => None,
        }
    }
}

enum Condition {
    NotZero = 0,
    Zero = 1,
    NoCarry = 2,
    Carry = 3,
}
impl Condition {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match (byte & mask) >> mask.trailing_zeros() {
            0 => Some(Condition::NotZero),
            1 => Some(Condition::Zero),
            2 => Some(Condition::NoCarry),
            3 => Some(Condition::Carry),
            _ => None,
        }
    }
}
enum R16Stk {
    BC = 0,
    DE = 1,
    HL = 2,
    AF = 3,
}
impl R16Stk {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match (byte & mask) >> mask.trailing_zeros() {
            0 => Some(R16Stk::BC),
            1 => Some(R16Stk::DE),
            2 => Some(R16Stk::HL),
            3 => Some(R16Stk::AF),
            _ => None,
        }
    }
}
enum LdParam {
    R16(R16),
    R16Mem(R16Mem),
    Imm16,
    SP,
}

#[derive(Copy, Clone)]
pub struct Cpu {
    pub(crate) registers: Registers,
    pub(crate) program_counter: u16,
    pub memory: Bus,
    halted: bool,
    interrupts_enabled: bool,
}

impl Cpu {
    pub fn new(mem: [u8; 0xFFFF + 1]) -> Self {
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
            program_counter: 0x0100,
        };
        let memory = Bus { memory: mem };
        return Cpu {
            registers,
            program_counter: 0x0100,
            memory,
            halted: false,
            interrupts_enabled: false,
        };
    }
    pub fn log(self) {
        // A:00 F:11 B:22 C:33 D:44 E:55 H:66 L:77 SP:8888 PC:9999 PCMEM:AA,BB,CC,DD
        println!("A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}",
            self.registers.a,self.registers.f.to_u8(), self.registers.b, self.registers.c,  self.registers.d, self.registers.e, self.registers.h,self.registers.l,
            self.registers.stack_pointer,self.program_counter, self.memory.read_byte(self.program_counter),
            self.memory.read_byte(self.program_counter+1),self.memory.read_byte(self.program_counter+2), self.memory.read_byte(self.program_counter+3));
    }

    pub fn set_thingy(&mut self) {
        self.memory.write_byte(0xFF44, 0x90);
    }
    fn handle_interrupt(&mut self) {
        // println!("Handling interrupt");
        // info!("Handling interrupt");
        if !self.interrupts_enabled && !self.halted {
            return;
        };
        let interrupt_enable = self.memory.read_byte(0xFFFF);
        let interrupt_flag = self.memory.read_byte(0xFF0F);
        let interrupts = interrupt_flag & interrupt_enable;
        if interrupts == 0x00 {
            return;
        }
        let interrupt_index = interrupts.trailing_zeros();
        let new_flag = interrupt_flag & !(1 << interrupt_index);
        self.memory.write_byte(0xFF0F, new_flag);
        self.interrupts_enabled = false;
        self.halted = false;
        // info!("calling {}", interrupt_index as u16 * 8);
        self.push(self.program_counter);
        self.program_counter = 0x40 + interrupt_index as u16 * 8;
        // self.call(0x40 + interrupt_index as u16 * 8);
    }

    // ld r16, imm16
    fn ld_r16_imm16(&mut self, register: R16, imm16: u16) {
        match register {
            R16::BC => self.registers.set_bc(imm16),
            R16::DE => self.registers.set_de(imm16),
            R16::HL => self.registers.set_hl(imm16),
            R16::SP => self.registers.stack_pointer = imm16,
        }
    }
    // ld r16mem, a
    fn ld_r16mem_a(&mut self, register: R16Mem) {
        let address: u16 = match register {
            R16Mem::BC => self.registers.get_bc(),
            R16Mem::DE => self.registers.get_de(),
            R16Mem::HLI => {
                let hl = self.registers.get_hl();
                self.registers.set_hl(hl.wrapping_add(1));
                hl
            }
            R16Mem::HLD => {
                let hl = self.registers.get_hl();
                self.registers.set_hl(hl.wrapping_sub(1));
                hl
            }
        };
        self.memory.write_byte(address, self.registers.a);
    }
    // ld imm16, sp
    fn ld_imm16_ptr_sp(&mut self) {
        let imm16 = self.memory.read_word(self.program_counter + 1);
        self.memory
            .write_byte(imm16, self.registers.stack_pointer as u8);
        self.memory.write_byte(
            imm16.wrapping_add(1),
            (self.registers.stack_pointer >> 8) as u8,
        );
    }
    // ld a, r16mem
    fn ld_a_r16mem(&mut self, register: R16Mem) {
        let address = match register {
            R16Mem::BC => self.registers.get_bc(),
            R16Mem::DE => self.registers.get_de(),
            R16Mem::HLI => {
                let hl = self.registers.get_hl();
                self.registers.set_hl(hl.wrapping_add(1));
                hl
            }
            R16Mem::HLD => {
                let hl = self.registers.get_hl();
                self.registers.set_hl(hl.wrapping_sub(1));
                hl
            }
        };
        self.registers.a = self.memory.read_byte(address);
    }

    fn inc_r16(&mut self, register: R16) {
        match register {
            R16::BC => self
                .registers
                .set_bc(self.registers.get_bc().wrapping_add(1)),
            R16::DE => self
                .registers
                .set_de(self.registers.get_de().wrapping_add(1)),
            R16::HL => self
                .registers
                .set_hl(self.registers.get_hl().wrapping_add(1)),
            R16::SP => self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1),
        }
    }

    fn dec_r16(&mut self, register: R16) {
        match register {
            R16::BC => self
                .registers
                .set_bc(self.registers.get_bc().wrapping_sub(1)),
            R16::DE => self
                .registers
                .set_de(self.registers.get_de().wrapping_sub(1)),
            R16::HL => self
                .registers
                .set_hl(self.registers.get_hl().wrapping_sub(1)),
            R16::SP => self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1),
        }
    }

    fn add_hl_r16(&mut self, register: R16) {
        let r16_value = match register {
            R16::BC => self.registers.get_bc(),
            R16::DE => self.registers.get_de(),
            R16::HL => self.registers.get_hl(),
            R16::SP => self.registers.stack_pointer,
        };
        let hl_value = self.registers.get_hl();
        let (result, overflow) = hl_value.overflowing_add(r16_value);
        self.registers.f.substract = false;
        self.registers.f.carry = overflow;
        // half carry is set when bits from lower half overflew to upper half
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
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        match target {
            R8::B => self.registers.b = value,
            R8::C => self.registers.c = value,
            R8::D => self.registers.d = value,
            R8::E => self.registers.e = value,
            R8::H => self.registers.h = value,
            R8::L => self.registers.l = value,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), value),
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
        // half carry is set when bits from lower half overflew to upper half
        if half_carry {
            self.registers.f.half_carry = ((target & 0b1111) + (value & 0b1111)) & 0x10 == 0x10;
        }
        return result;
    }

    fn adc_and_set_flags(&mut self, target: u8, value: u8) -> u8 {
        // let result = target.wrapping_add(value);
        let c = if self.registers.f.carry { 1 } else { 0 };
        let result_with_carry = target.wrapping_add(value).wrapping_add(c);
        // let (result_with_carry, overflow) = result.overflowing_add(c);
        self.registers.f.zero = result_with_carry == 0;
        self.registers.f.substract = false;
        // self.registers.f.carry = overflow;
        self.registers.f.carry = target as u16 + value as u16 + c as u16 > 0xFF;
        // half carry is set when bits from lower half overflew to upper half
        self.registers.f.half_carry =
            ((target & 0b1111) + (value & 0b1111) + (c & 0b1111)) > 0b1111;

        // self.registers.f.half_carry = ((target & 0b1111) + (imm8 as u16 & 0b1111)) & 0x10 == 0x10;
        return result_with_carry;
    }

    fn sub_and_set_flags(&mut self, target: u8, value: u8, carry: bool, half_carry: bool) -> u8 {
        let (result, overflow) = target.overflowing_sub(value);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = true;
        if half_carry {
            self.registers.f.half_carry = ((target & 0b1111) < (value & 0b1111));
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

    fn cp_and_set_flags(&mut self, target: u8, value: u8) -> u8 {
        let result = target.wrapping_sub(value);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = ((target & 0b1111) < (value & 0b1111));
        self.registers.f.carry = value > target;
        return result;
    }

    fn inc_r8(&mut self, register: R8) {
        match register {
            R8::B => self.registers.b = self.add_and_set_flags(self.registers.b, 1, false, true),
            R8::C => self.registers.c = self.add_and_set_flags(self.registers.c, 1, false, true),
            R8::D => self.registers.d = self.add_and_set_flags(self.registers.d, 1, false, true),
            R8::E => self.registers.e = self.add_and_set_flags(self.registers.e, 1, false, true),
            R8::H => self.registers.h = self.add_and_set_flags(self.registers.h, 1, false, true),
            R8::L => self.registers.l = self.add_and_set_flags(self.registers.l, 1, false, true),
            R8::HL_PTR => {
                let address = self.registers.get_hl();
                let value = self.memory.read_byte(address);
                let result = self.add_and_set_flags(value, 1, false, true);
                self.memory.write_byte(address, result);
            }
            R8::A => self.registers.a = self.add_and_set_flags(self.registers.a, 1, false, true),
        };
    }
    fn dec_r8(&mut self, register: R8) {
        match register {
            R8::B => self.registers.b = self.sub_and_set_flags(self.registers.b, 1, false, true),
            R8::C => self.registers.c = self.sub_and_set_flags(self.registers.c, 1, false, true),
            R8::D => self.registers.d = self.sub_and_set_flags(self.registers.d, 1, false, true),
            R8::E => self.registers.e = self.sub_and_set_flags(self.registers.e, 1, false, true),
            R8::H => self.registers.h = self.sub_and_set_flags(self.registers.h, 1, false, true),
            R8::L => self.registers.l = self.sub_and_set_flags(self.registers.l, 1, false, true),
            R8::HL_PTR => {
                let address = self.registers.get_hl();
                let value = self.memory.read_byte(address);
                let result = self.sub_and_set_flags(value, 1, false, true);
                self.memory.write_byte(address, result);
            }
            R8::A => self.registers.a = self.sub_and_set_flags(self.registers.a, 1, false, true),
        };
    }

    fn ld_r8_imm8(&mut self, register: R8) {
        let imm8 = self.memory.read_byte(self.program_counter + 1);
        match register {
            R8::B => self.registers.b = imm8,
            R8::C => self.registers.c = imm8,
            R8::D => self.registers.d = imm8,
            R8::E => self.registers.e = imm8,
            R8::H => self.registers.h = imm8,
            R8::L => self.registers.l = imm8,
            R8::HL_PTR => {
                let address = self.registers.get_hl();
                self.memory.write_byte(address, imm8);
            }
            R8::A => self.registers.a = imm8,
        }
    }

    // rotate left
    fn rl(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let carry = value & 0x80 == 0x80;
        let rotated = (value << 1) | (if self.registers.f.carry { 1 } else { 0 });
        self.registers.f.carry = carry;
        self.registers.f.zero = rotated == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        match register {
            R8::B => self.registers.b = rotated,
            R8::C => self.registers.c = rotated,
            R8::D => self.registers.d = rotated,
            R8::E => self.registers.e = rotated,
            R8::H => self.registers.h = rotated,
            R8::L => self.registers.l = rotated,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), rotated),
            R8::A => self.registers.a = rotated,
        };
    }

    // rotate left circular
    fn rlc(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let carry = value & 0x80 == 0x80;
        let rotated = (value << 1) | (if carry { 1 } else { 0 });
        self.registers.f.carry = carry;
        self.registers.f.zero = rotated == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        match register {
            R8::B => self.registers.b = rotated,
            R8::C => self.registers.c = rotated,
            R8::D => self.registers.d = rotated,
            R8::E => self.registers.e = rotated,
            R8::H => self.registers.h = rotated,
            R8::L => self.registers.l = rotated,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), rotated),
            R8::A => self.registers.a = rotated,
        };
    }

    // rotate right
    fn rr(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let carry = value & 0x01 == 0x01;
        let rotated = (value >> 1) | (if self.registers.f.carry { 0x80 } else { 0 });
        self.registers.f.carry = carry;
        match register {
            R8::A => self.registers.f.zero = false,
            _ => self.registers.f.zero = rotated == 0,
        }
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        match register {
            R8::B => self.registers.b = rotated,
            R8::C => self.registers.c = rotated,
            R8::D => self.registers.d = rotated,
            R8::E => self.registers.e = rotated,
            R8::H => self.registers.h = rotated,
            R8::L => self.registers.l = rotated,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), rotated),
            R8::A => self.registers.a = rotated,
        };
    }

    // rotate right circural
    fn rrc(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let carry = value & 0x01 == 0x01;
        let rotated = (value >> 1) | (if carry { 0x80 } else { 0 });
        self.registers.f.carry = carry;
        self.registers.f.zero = rotated == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        match register {
            R8::B => self.registers.b = rotated,
            R8::C => self.registers.c = rotated,
            R8::D => self.registers.d = rotated,
            R8::E => self.registers.e = rotated,
            R8::H => self.registers.h = rotated,
            R8::L => self.registers.l = rotated,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), rotated),
            R8::A => self.registers.a = rotated,
        };
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

    // jump relative
    fn jr_imm8(&mut self) {
        let offset = self.memory.read_byte(self.program_counter + 1);
        let n = offset as i8;
        self.program_counter = (((self.program_counter + 2) as u16) as i16 + n as i16) as u16;
    }

    fn jr_imm8_cond(&mut self, cond: Condition) {
        let c = match cond {
            Condition::NotZero => self.registers.f.zero == false,
            Condition::Zero => self.registers.f.zero == true,
            Condition::NoCarry => self.registers.f.carry == false,
            Condition::Carry => self.registers.f.carry == true,
        };
        if c {
            self.jr_imm8();
        } else {
            self.program_counter += 2;
        }
    }

    // stack grows downwoard
    fn push(&mut self, value: u16) {
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
        self.memory
            .write_byte(self.registers.stack_pointer, ((value & 0xFF00) >> 8) as u8);
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
        self.memory
            .write_byte(self.registers.stack_pointer, (value & 0xFF) as u8);
    }

    fn push_r16(&mut self, register: R16Stk) {
        let value = match register {
            R16Stk::BC => self.registers.get_bc(),
            R16Stk::DE => self.registers.get_de(),
            R16Stk::HL => self.registers.get_hl(),
            R16Stk::AF => self.registers.get_af(),
        };
        self.push(value);
    }

    // does this need to take registe?
    fn pop(&mut self) -> u16 {
        let lower_byte = self.memory.read_byte(self.registers.stack_pointer) as u16;
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1);
        let higher_byte = self.memory.read_byte(self.registers.stack_pointer) as u16;
        self.registers.stack_pointer = self.registers.stack_pointer.wrapping_add(1);
        return (higher_byte << 8) | lower_byte;
    }

    fn pop_r16(&mut self, register: R16Stk) {
        let value = self.pop();
        match register {
            R16Stk::BC => self.registers.set_bc(value),
            R16Stk::DE => self.registers.set_de(value),
            R16Stk::HL => self.registers.set_hl(value),
            R16Stk::AF => self.registers.set_af(value),
        };
    }

    fn ret(&mut self) {
        let program_counter = self.pop();
        self.program_counter = program_counter;
    }

    fn ret_cond(&mut self, cond: Condition) {
        let c = match cond {
            Condition::NotZero => self.registers.f.zero == false,
            Condition::Zero => self.registers.f.zero == true,
            Condition::NoCarry => self.registers.f.carry == false,
            Condition::Carry => self.registers.f.carry == true,
        };
        if c {
            self.ret()
        } else {
            self.program_counter += 1;
        }
    }
    // enable interrupt flag which is located at FFFF
    fn ei(&mut self) {
        self.interrupts_enabled = true;
        // self.memory.write_byte(0xFFFF, 1);
    }

    fn di(&mut self) {
        self.interrupts_enabled = false;
        // self.memory.write_byte(0xFFFF, 0);
    }

    fn reti(&mut self) {
        self.ei();
        self.ret();
    }

    fn jp(&mut self, address: u16) {
        self.program_counter = address;
    }

    fn jp_cond(&mut self, address: u16, cond: Condition) {
        let c = match cond {
            Condition::NotZero => self.registers.f.zero == false,
            Condition::Zero => self.registers.f.zero == true,
            Condition::NoCarry => self.registers.f.carry == false,
            Condition::Carry => self.registers.f.carry == true,
        };
        if c {
            self.jp(address);
        } else {
            self.program_counter += 1;
        }
    }

    fn call(&mut self, address: u16) {
        self.push(self.program_counter + 1);
        self.jp(address);
    }

    fn call_cond(&mut self, address: u16, cond: Condition) {
        let c = match cond {
            Condition::NotZero => self.registers.f.zero == false,
            Condition::Zero => self.registers.f.zero == true,
            Condition::NoCarry => self.registers.f.carry == false,
            Condition::Carry => self.registers.f.carry == true,
        };
        if c {
            self.call(address);
        } else {
            self.program_counter += 1;
        }
    }

    fn rst_tgt3(&mut self, address: u8) {
        self.call(address as u16 * 8);
    }

    fn ldh_cptr_a(&mut self) {
        self.memory
            .write_byte(0xFF00 + self.registers.c as u16, self.registers.a);
    }

    fn ldh_imm8addr_a(&mut self, offset: u8) {
        self.memory
            .write_byte(0xFF00 + offset as u16, self.registers.a);
    }

    fn ld_imm16addr_a(&mut self, address: u16) {
        self.memory.write_byte(address, self.registers.a);
    }

    fn ldh_a_caddr(&mut self) {
        self.registers.a = self.memory.read_byte(0xFF00 + self.registers.c as u16);
    }

    fn ldh_a_imm8addr(&mut self, offset: u8) {
        self.registers.a = self.memory.read_byte(0xFF00 | offset as u16);
    }

    fn ld_a_imm16addr(&mut self, address: u16) {
        self.registers.a = self.memory.read_byte(address);
    }

    // add signed
    fn add_sp_imm8(&mut self, imm8: u8) -> u16 {
        let target = self.registers.stack_pointer;
        let result = (target).wrapping_add((imm8 as i8) as u16);
        let (_, overflow) = (target as u8).overflowing_add(imm8);
        self.registers.f.zero = false;
        self.registers.f.substract = false;
        self.registers.f.carry = overflow;
        // half carry is set when bits from lower half overflew to upper half
        // self.registers.f.half_carry =
        //     ((target as i16 & 0xFFF) + (imm8 as i16 & 0xFFF)) & 0x1000 == 0x1000;
        // self.registers.f.half_carry = ((target & 0b1111) + (result as u16 & 0b1111)) > 0b1111;
        self.registers.f.half_carry = ((target & 0b1111) + (imm8 as u16 & 0b1111)) & 0x10 == 0x10;
        return result;
    }

    fn ld_hl_sp_plus_imm8(&mut self, imm8: u8) {
        let target = self.registers.stack_pointer;
        let (result, overflow) = (target as i16).overflowing_add(imm8 as i16);
        self.registers.f.zero = false;
        self.registers.f.substract = false;
        self.registers.f.carry = overflow;
        // half carry is set when bits from lower half overflew to upper half
        self.registers.f.half_carry =
            ((target as i16 & 0xFFF) + (result as i16 & 0xFFF)) & 0x1000 == 0x1000;
        self.registers.set_hl(result as u16);
    }

    fn ld_sp_hl(&mut self) {
        self.registers.stack_pointer = self.registers.get_hl();
    }

    fn sra(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let c = value & 0x01 == 0x01;
        // for signed types shift is arithmetic
        let shifted = ((value as i8) >> 1) as u8;
        self.registers.f.zero = shifted == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = c;
        match register {
            R8::B => self.registers.b = shifted,
            R8::C => self.registers.c = shifted,
            R8::D => self.registers.d = shifted,
            R8::E => self.registers.e = shifted,
            R8::H => self.registers.h = shifted,
            R8::L => self.registers.l = shifted,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), shifted),
            R8::A => self.registers.a = shifted,
        };
    }

    fn sla(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let c = value & 0x80 == 0x80;
        // for signed types shift is arithmetic
        let shifted = ((value as i8) << 1) as u8;
        self.registers.f.zero = shifted == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = c;
        match register {
            R8::B => self.registers.b = shifted,
            R8::C => self.registers.c = shifted,
            R8::D => self.registers.d = shifted,
            R8::E => self.registers.e = shifted,
            R8::H => self.registers.h = shifted,
            R8::L => self.registers.l = shifted,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), shifted),
            R8::A => self.registers.a = shifted,
        };
    }

    fn srl(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let c = value & 0x01 == 0x01;
        let shifted = value >> 1;
        self.registers.f.zero = shifted == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = c;
        match register {
            R8::B => self.registers.b = shifted,
            R8::C => self.registers.c = shifted,
            R8::D => self.registers.d = shifted,
            R8::E => self.registers.e = shifted,
            R8::H => self.registers.h = shifted,
            R8::L => self.registers.l = shifted,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), shifted),
            R8::A => self.registers.a = shifted,
        };
    }

    fn swap(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let higher_bits = value & 0xF0;
        let lower_bits = value & 0xF;
        let result = higher_bits | lower_bits;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        match register {
            R8::B => self.registers.b = result,
            R8::C => self.registers.c = result,
            R8::D => self.registers.d = result,
            R8::E => self.registers.e = result,
            R8::H => self.registers.h = result,
            R8::L => self.registers.l = result,
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), result),
            R8::A => self.registers.a = result,
        };
    }

    fn bit(&mut self, register: R8, index: u8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
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
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
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
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), result),
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
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
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
            R8::HL_PTR => self.memory.write_byte(self.registers.get_hl(), result),
            R8::A => self.registers.a = result,
        };
    }
    // ParamMasks::Arithmetic => 0b111,
    // ParamMasks::LoadR8R8Src => 0b111,
    // ParamMasks::LoadR8R8Dst => 0b111000,
    // ParamMasks::R16Block1 => 0b110000,
    // ParamMasks::R8Block1 => 0b111000,
    // ParamMasks::CondBlock4 => 0b11000000,
    // ParamMasks::R8CB => 0b111,
    // ParamMasks::Bit3CB => 0b111000,

    fn add_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.add_and_set_flags(self.registers.a, value, true, true);
        self.registers.a = result;
    }

    fn adc_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.adc_and_set_flags(self.registers.a, value);
        self.registers.a = result;
    }

    fn sub_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.sub_and_set_flags(self.registers.a, value, true, true);
        self.registers.a = result;
    }
    fn sbc_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.sbc_and_set_flags(self.registers.a, value);
        self.registers.a = result;
    }
    fn and_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.and_and_set_flags(self.registers.a, value);
        self.registers.a = result;
    }
    fn or_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.or_and_set_flags(self.registers.a, value);
        self.registers.a = result;
    }
    fn xor_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.xor_and_set_flags(self.registers.a, value);
        self.registers.a = result;
    }
    fn cp_a_r8(&mut self, register: R8) {
        let value = match register {
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HL_PTR => self.memory.read_byte(self.registers.get_hl()),
            R8::A => self.registers.a,
        };
        let result = self.cp_and_set_flags(self.registers.a, value);
    }

    fn execute_instruction(&mut self, byte: u8) {
        let Arithmetic = 0b111;
        let LoadR8R8Src = 0b111;
        let LoadR8R8Dst = 0b111000;
        let R16Block1 = 0b110000;
        let R8Block1 = 0b111000;
        let CondBlock1 = 0b00011000;
        let CondBlock4 = 0b00011000;
        let R8CB = 0b111;
        let Bit3CB: u8 = 0b111000;
        let Tgt3 = 0b00111000;
        let R16stkBlock4 = 0b00110000;
        match byte {
            //nop
            0x00 => {
                self.program_counter += 1;
            }
            //stop
            0x10 => {
                self.program_counter += 1;
            }
            //halt
            0x76 => {
                self.halted = true;
                info!("im halted now");
                self.program_counter += 1;
            }

            // ---------------------- BLOCK 1 ----------------------
            // ld dst, src

            // ld r16, imm16
            0x01 | 0x11 | 0x21 | 0x31 => {
                let imm16 = self.memory.read_word(self.program_counter + 1);
                self.ld_r16_imm16(R16::from_masked_u8(byte, R16Block1).unwrap(), imm16);
                self.program_counter += 3;
            }
            // ld r16mem, a
            0x02 | 0x12 | 0x22 | 0x32 => {
                self.ld_r16mem_a(R16Mem::from_masked_u8(byte, R16Block1).unwrap());
                self.program_counter += 1;
            }

            // ld a, r16mem
            0x0A | 0x1A | 0x2A | 0x3A => {
                // Some(Operation::LoadR16MemA(
                // R16Mem::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
                self.ld_a_r16mem(R16Mem::from_masked_u8(byte, R16Block1).unwrap());
                self.program_counter += 1;
            }

            // ld imm16, sp
            8 => {
                self.ld_imm16_ptr_sp();
                self.program_counter += 3;

                // Some(Operation::LoadImm16Sp)
            }

            // inc r16
            0x03 | 0x13 | 0x23 | 0x33 => {
                //Some(Operation::IncR16(
                // R16::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
                //)),
                self.inc_r16(R16::from_masked_u8(byte, R16Block1).unwrap());
                self.program_counter += 1;
            }
            // dec r16
            0x0B | 0x1B | 0x2B | 0x3B => {
                //Some(Operation::DecR16(
                //    R16::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
                //)),
                self.dec_r16(R16::from_masked_u8(byte, R16Block1).unwrap());
                self.program_counter += 1;
            }
            // add hl, r16
            0x09 | 0x19 | 0x29 | 0x39 => {
                //Some(Operation::AddHlR16(
                //    R16::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
                //)),
                self.add_hl_r16(R16::from_masked_u8(byte, R16Block1).unwrap());
                self.program_counter += 1;
            }

            // inc r8
            0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C | 0x3C | 0x34 => {
                //Some(Operation::IncR8(
                //R8::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
                //)),
                self.inc_r8(R8::from_masked_u8(byte, R8Block1).unwrap());
                self.program_counter += 1;
            }

            // dec r8
            0x05 | 0x0D | 0x15 | 0x1D | 0x25 | 0x2D | 0x3D | 0x35 => {
                //Some(Operation::DecR8(
                //R8::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
                //)),
                self.dec_r8(R8::from_masked_u8(byte, R8Block1).unwrap());
                self.program_counter += 1;
            }

            // ld r8, imm8
            0x06 | 0x0E | 0x16 | 0x1E | 0x26 | 0x2E | 0x3E | 0x36 => {
                //   Some(Operation::LoadR8Imm8(
                //  R8::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
                //)),
                self.ld_r8_imm8(R8::from_masked_u8(byte, R8Block1).unwrap());
                self.program_counter += 2;
            }

            0x07 => {
                self.rlc(R8::A);
                self.program_counter += 1;
            }
            0x17 => {
                //   Some(Operation::RLA),
                self.rl(R8::A);
                self.program_counter += 1;
            }
            0x27 => {
                //   Some(Operation::DAA),
                self.daa();
                self.program_counter += 1;
            }
            0x37 => {
                //Some(Operation::SCF),
                self.scf();
                self.program_counter += 1;
            }
            0x0F => {
                //   Some(Operation::RRCA),
                self.rrc(R8::A);
                self.program_counter += 1;
            }
            0x1F => {
                ///Some(Operation::RRA),
                self.rr(R8::A);
                self.program_counter += 1;
            }
            0x2F => {
                //    Some(Operation::CPL)
                self.cpl();
                self.program_counter += 1;
            }
            0x3F => {
                //Some(Operation::CCF),
                self.ccf();
                self.program_counter += 1;
            }

            // jr imm8
            0x18 => {
                //    Some(Operation::JrImm8)
                self.jr_imm8();
            }

            // jr cond imm8
            0x20 | 0x28 | 0x30 | 0x38 => {
                //    Some(Operation::JrCondImm8(
                //    Condition::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
                // )),
                //
                self.jr_imm8_cond(Condition::from_masked_u8(byte, CondBlock1).unwrap());
            }

            // ------------------------------ Block 2 load r8 r8 ------------------------------
            // ld r8 r8, 0 1 r8 dst r8 src
            0x40..=0x6F | 0x70..=0x75 | 0x77..=0x7F => {
                //Some(Operation::LoadR8R8(
                //    R8::from_masked_u8(opcode, ParamMasks::LoadR8R8Src.get_mask()).unwrap(),
                //    R8::from_masked_u8(opcode, ParamMasks::LoadR8R8Src.get_mask()).unwrap(),
                //)),
                self.ld_r8_r8(
                    R8::from_masked_u8(byte, LoadR8R8Src).unwrap(),
                    R8::from_masked_u8(byte, LoadR8R8Dst).unwrap(),
                );
                self.program_counter += 1;
            }

            // ------------- Block 3 Arithmetic ------------

            // add a, r8	1	0	0	0	0	Operand (r8)
            0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 => {
                // Some(Operation::Arithmetic(Arithemtic::ADD_A(
                //     R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap(),
                // )))
                self.add_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }
            // adc a, r8	1	0	0	0	1	Operand (r8)
            0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D | 0x8E | 0x8F => {
                // Some(Operation::Arithmetic(Arithemtic::ADC_A(
                //     R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap(),
                // )))
                self.adc_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }
            // sub a, r8	1	0	0	1	0	Operand (r8)
            0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 => {
                // Some(Operation::Arithmetic(Arithemtic::SUB_A(
                //     R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap(),
                // )))
                self.sub_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }
            // sbc a, r8	1	0	0	1	1	Operand (r8)
            0x98 | 0x99 | 0x9A | 0x9B | 0x9C | 0x9D | 0x9E | 0x9F => {
                // Some(Operation::Arithmetic(Arithemtic::SBC_A(
                //     R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap(),
                // )))
                self.sbc_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }
            // and a, r8	1	0	1	0	0	Operand (r8)
            0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 | 0xA6 | 0xA7 => {
                // Some(Operation::Arithmetic(Arithemtic::AND_A(
                //     R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap(),
                // )))
                self.and_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }
            // xor a, r8	1	0	1	0	1	Operand (r8)
            0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC | 0xAD | 0xAE | 0xAF => {
                // Some(Operation::Arithmetic(Arithemtic::XOR_A(
                //     R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap(),
                // )))
                self.xor_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }
            // or a, r8	1	0	1	1	0	Operand (r8)
            0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 => {
                //     Some(Operation::Arithmetic(
                //     Arithemtic::OR_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
                // )),
                self.or_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }
            // cp a, r8	1	0	1	1	1	Operand (r8)
            0xB8 | 0xB9 | 0xBA | 0xBB | 0xBC | 0xBD | 0xBE | 0xBF => {
                //     Some(Operation::Arithmetic(
                //     Arithemtic::CP_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
                // )),
                self.cp_a_r8(R8::from_masked_u8(byte, Arithmetic).unwrap());
                self.program_counter += 1;
            }

            // ---------------------------------- Block 4 ----------------------------------
            // add a, imm8
            0xC6 => {
                let value = self.memory.read_byte(self.program_counter + 1);
                let result = self.add_and_set_flags(self.registers.a, value, true, true);
                self.registers.a = result;
                self.program_counter += 2;
            }
            // adc a, imm8
            0xCE => {
                let value = self.memory.read_byte(self.program_counter + 1);
                let result = self.adc_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
            }
            // sub a, imm8
            0xD6 => {
                let value = self.memory.read_byte(self.program_counter + 1);
                let result = self.sub_and_set_flags(self.registers.a, value, true, true);
                self.registers.a = result;
                self.program_counter += 2;
            }
            // sbc a, imm8
            0xDE => {
                let value = self.memory.read_byte(self.program_counter + 1);
                let result = self.sbc_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
            }
            // and a, imm8
            0xE6 => {
                let value = self.memory.read_byte(self.program_counter + 1);
                let result = self.and_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
            }
            // xor a, imm8
            0xEE => {
                let value = self.memory.read_byte(self.program_counter + 1);
                let result = self.xor_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
            }
            // or a, imm8
            0xF6 => {
                let value = self.memory.read_byte(self.program_counter + 1);
                let result = self.or_and_set_flags(self.registers.a, value);
                self.registers.a = result;
                self.program_counter += 2;
            }
            0xFE => {
                let value = self.memory.read_byte(self.program_counter + 1);
                self.cp_and_set_flags(self.registers.a, value);
                self.program_counter += 2;
            }
            // ret cond
            0xC0 | 0xC8 | 0xD0 | 0xD8 =>
            //Some(Operation::RetCond(
            //    Condition::from_masked_u8(opcode, ParamMasks::CondBlock4.get_mask()).unwrap(),
            //)),
            {
                self.ret_cond(Condition::from_masked_u8(byte, CondBlock4).unwrap());
                // self.program_counter += 1;
            }
            // ret
            0xC9 =>
            //Some(Operation::Ret),
            {
                self.ret();
                // self.program_counter+=1;
            }
            // RetI,
            0xD9 =>
            // Some(Operation::RetI),
            {
                self.reti();
            }
            // JpCondImm16(Condition),
            0xC2 | 0xCA | 0xD2 | 0xDA =>
            //Some(Operation::JpCondImm16(
            //               Condition::from_masked_u8(opcode, ParamMasks::CondBlock4.get_mask()).unwrap(),
            //         )),
            {
                let address = self.memory.read_word(self.program_counter + 1);
                self.program_counter += 2;
                self.jp_cond(
                    address,
                    Condition::from_masked_u8(byte, CondBlock4).unwrap(),
                );
            }
            // JpImm16,
            0xC3 =>
            //Some(Operation::JpImm16),
            {
                let address = self.memory.read_word(self.program_counter + 1);
                self.program_counter += 2;
                self.jp(address);
            }

            // JpHl,
            0xE9 => {
                //    Some(Operation::JpHl),
                self.jp(self.registers.get_hl());
            }
            // CallCondImm16(Condition),
            0xC4 | 0xCC | 0xD4 | 0xDC =>
            //Some(Operation::CallCondImm16(
            //    Condition::from_masked_u8(opcode, ParamMasks::CondBlock4.get_mask()).unwrap(),
            //)),
            {
                let address = self.memory.read_word(self.program_counter + 1);
                self.program_counter += 2;
                self.call_cond(
                    address,
                    Condition::from_masked_u8(byte, CondBlock4).unwrap(),
                );
            }
            // CallImm16,
            0xCD =>
            //Some(Operation::CallImm16),
            {
                let address = self.memory.read_word(self.program_counter + 1);
                self.program_counter += 2;
                self.call(address);
            }
            // RstTgt3,
            0xC7 | 0xCF | 0xD7 | 0xDF | 0xE7 | 0xEF | 0xF7 | 0xFF =>
            //Some(Operation::RstTgt3),
            {
                let address = (byte & Tgt3) >> Tgt3.trailing_zeros();
                self.rst_tgt3(address);
            }
            // PopR16(R16Stk),
            0xC1 | 0xD1 | 0xE1 | 0xF1 =>
            // Some(Operation::PopR16(
            //                R16Stk::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
            //          )),
            {
                self.pop_r16(R16Stk::from_masked_u8(byte, R16stkBlock4).unwrap());
                self.program_counter += 1;
            }
            // PushR16(R16Stk),
            0xC5 | 0xD5 | 0xE5 | 0xF5 =>
            //Some(Operation::PushR16(
            //               R16Stk::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
            //          )),
            {
                self.push_r16(R16Stk::from_masked_u8(byte, R16stkBlock4).unwrap());
                self.program_counter += 1;
            }
            // LdhCAddrA,
            0xE2 =>
            //Some(Operation::LdhCAddrA),
            {
                self.ldh_cptr_a();
                self.program_counter += 1;
            }
            // LdhImm8AddrA,
            0xE0 =>
            //Some(Operation::LdhImm8AddrA),
            {
                let offset = self.memory.read_byte(self.program_counter + 1);
                self.ldh_imm8addr_a(offset);
                self.program_counter += 2;
            }
            // LdImm16AddrA,
            0xEA =>
            //Some(Operation::LdImm16AddrA),
            {
                let address = self.memory.read_word(self.program_counter + 1);
                self.ld_imm16addr_a(address);
                self.program_counter += 3;
            }
            // LdhACAddr,
            0xF2 =>
            //Some(Operation::LdhACAddr),
            {
                self.ldh_a_caddr();
                self.program_counter += 1;
            }
            // LdhAImm8Addr,
            0xF0 =>
            //Some(Operation::LdhAImm8Addr),
            {
                let offset = self.memory.read_byte(self.program_counter + 1);
                self.ldh_a_imm8addr(offset);
                self.program_counter += 2;
            }
            // LdAImm16Addr,
            0xFA =>
            //Some(Operation::LdAImm16Addr),
            {
                let address = self.memory.read_word(self.program_counter + 1);
                self.ld_a_imm16addr(address);
                self.program_counter += 3;
            }
            // AddSpImm8,
            0xE8 =>
            //Some(Operation::AddSpImm8),
            {
                let imm8 = self.memory.read_byte(self.program_counter + 1);
                self.registers.stack_pointer = self.add_sp_imm8(imm8);
                self.program_counter += 2;
            }
            // LdHlSpImm8,
            0xF8 =>
            //Some(Operation::LdHlSpImm8),
            {
                let imm8 = self.memory.read_byte(self.program_counter + 1);
                let result = self.add_sp_imm8(imm8);
                self.registers.set_hl(result);
                // self.ld_hl_sp_plus_imm8(imm8);
                self.program_counter += 2;
            }
            // LdSpHl,
            0xF9 =>
            //Some(Operation::LdSpHl),
            {
                self.ld_sp_hl();
                self.program_counter += 1;
            }
            // Di,
            0xF3 =>
            //Some(Operation::Di),
            {
                self.di();
                self.interrupts_enabled = false;
                self.program_counter += 1;
            }
            // Ei,
            0xFB =>
            //Some(Operation::Ei),
            {
                self.ei();
                self.interrupts_enabled = true;
                self.program_counter += 1;
            }

            // ---------------------- CB PREFIXED OPCODES ----------------------
            0xCB =>
            //Some(Operation::CbOperation(CbOperation::RlR8(R8::A))),
            {
                let prefixed = self.memory.read_byte(self.program_counter + 1);
                match prefixed {
                    0x0 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 => {
                        self.rlc(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x08 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x0E | 0x0F => {
                        self.rrc(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x10 | 0x11 | 0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 => {
                        self.rl(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x18 | 0x19 | 0x1A | 0x1B | 0x1C | 0x1D | 0x1E | 0x1F => {
                        self.rr(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25 | 0x26 | 0x27 => {
                        self.sla(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x28 | 0x29 | 0x2A | 0x2B | 0x2C | 0x2D | 0x2E | 0x2F => {
                        self.sra(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 | 0x36 | 0x37 => {
                        self.swap(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x38 | 0x39 | 0x3A | 0x3B | 0x3C | 0x3D | 0x3E | 0x3F => {
                        self.srl(R8::from_masked_u8(prefixed, R8CB).unwrap());
                        self.program_counter += 2;
                    }
                    0x40..=0x7F => {
                        self.bit(
                            R8::from_masked_u8(prefixed, R8CB).unwrap(),
                            (prefixed & Bit3CB) >> Bit3CB.trailing_zeros(),
                        );
                        self.program_counter += 2;
                    }
                    0x80..=0xBF => {
                        self.res(
                            R8::from_masked_u8(prefixed, R8CB).unwrap(),
                            (prefixed & Bit3CB) >> Bit3CB.trailing_zeros(),
                        );
                        self.program_counter += 2;
                    }
                    0xC0..=0xFF => {
                        self.set(
                            R8::from_masked_u8(prefixed, R8CB).unwrap(),
                            (prefixed & Bit3CB) >> Bit3CB.trailing_zeros(),
                        );
                        self.program_counter += 2;
                    }
                    _ => {}
                };
            }
            _ => {}
        }
    }

    pub fn next(&mut self) {
        self.handle_interrupt();
        if !self.halted {
            // print!("not halted");
            let current_byte = self.memory.read_byte(self.program_counter);
            self.execute_instruction(current_byte);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::{Cpu, Flags, Registers};

    // #[test]
    // fn test1() {
    //     let mut cpu = Cpu {
    //         registers: Registers {
    //             a: 0,
    //             b: 0,
    //             c: 0,
    //             d: 0,
    //             e: 0,
    //             f: Flags {
    //                 zero: false,
    //                 substract: false,
    //                 half_carry: false,
    //                 carry: false,
    //             },
    //             h: 0,
    //             l: 0,
    //             stack_pointer: 0,
    //             program_counter: 0,
    //         },
    //     };
    //     cpu.add_a_u8(8);
    //     assert_eq!(8, cpu.registers.a);
    //     cpu.add_a_u8(255);
    //     assert_eq!(7, cpu.registers.a);
    //     assert_eq!(true, cpu.registers.f.carry);
    //     cpu.adc_a_u8(8);
    //     assert_eq!(16, cpu.registers.a);
    // }
}
