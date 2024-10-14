const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

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
    // fn get_af(&self) -> u16 {
    //     (self.a as u16) << 8 | self.f as u16
    // }
    // fn set_af(&mut self, value: u16) {
    //     self.a = (value >> 8) as u8;
    //     self.f = value as u8;
    // }

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
        self.l = value as u8;
    }
}

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

enum TargetRegister {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

pub struct Cpu {
    pub(crate) registers: Registers,
    pub(crate) program_counter: u16,
}

impl Cpu {
    fn update_flags(&mut self, last_operation: u8) {}
    //ADD A,r8
    //ADD A,[HL]
    //ADD A,n8
    fn add_a_u8(&mut self, value: u8) {
        let (result, overflow) = self.registers.a.overflowing_add(value);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.carry = overflow;
        // half carry is set when bits from lower half overflew to upper half
        self.registers.f.half_carry = (self.registers.a & 0b1111) + (value & 0b1111) > 0b1111;
        self.registers.a = result;
    }
    // ADC A,r8
    // ADC A,[HL]
    // ADC A,n8
    fn adc_a_u8(&mut self, value: u8) {
        let result = self.registers.a.wrapping_add(value);
        let (result_with_carry, overflow) = result.overflowing_add(self.registers.f.carry as u8);
        self.registers.f.zero = result_with_carry == 0;
        self.registers.f.substract = false;
        self.registers.f.carry = overflow;
        // half carry is set when bits from lower half overflew to upper half
        self.registers.f.half_carry = (self.registers.a & 0b1111) + (value & 0b1111) > 0b1111;
        self.registers.a = result_with_carry;
    }

    // AND A,r8
    // AND A,[HL]
    // AND A,n8
    fn and_a_u8(&mut self, value: u8) -> u8 {
        let result = self.registers.a & value;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = true;
        self.registers.f.carry = false;
        return result;
    }
    // CP A,r8
    // CP A,[HL]
    // CP A,n8
    fn cp_a_u8(&mut self, value: u8) -> u8 {
        let (result, overflow) = self.registers.a.overflowing_sub(value);
        self.registers.f.zero = result == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = (self.registers.a & 0b1111) + (value & 0b1111) > 0b1111;
        self.registers.f.carry = value > self.registers.a;
        return result;
    }

    fn inc_r8(&mut self, target: TargetRegister) {
        match target {
            TargetRegister::A => self.registers.a = self.registers.a.wrapping_add(1),
            TargetRegister::B => self.registers.b = self.registers.b.wrapping_add(1),
            TargetRegister::C => self.registers.c = self.registers.c.wrapping_add(1),
            TargetRegister::D => self.registers.d = self.registers.d.wrapping_add(1),
            TargetRegister::E => self.registers.e = self.registers.e.wrapping_add(1),
            TargetRegister::H => self.registers.h = self.registers.h.wrapping_add(1),
            TargetRegister::L => self.registers.l = self.registers.l.wrapping_add(1),
        };
    }
    fn dec_r8(&mut self, target: TargetRegister) {
        match target {
            TargetRegister::A => self.registers.a = self.registers.a.wrapping_sub(1),
            TargetRegister::B => self.registers.b = self.registers.b.wrapping_sub(1),
            TargetRegister::C => self.registers.c = self.registers.c.wrapping_sub(1),
            TargetRegister::D => self.registers.d = self.registers.d.wrapping_sub(1),
            TargetRegister::E => self.registers.e = self.registers.e.wrapping_sub(1),
            TargetRegister::H => self.registers.h = self.registers.h.wrapping_sub(1),
            TargetRegister::L => self.registers.l = self.registers.l.wrapping_sub(1),
        };
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::{Cpu, Flags, Registers};

    #[test]
    fn test1() {
        let mut cpu = Cpu {
            registers: Registers {
                a: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                f: Flags {
                    zero: false,
                    substract: false,
                    half_carry: false,
                    carry: false,
                },
                h: 0,
                l: 0,
                stack_pointer: 0,
                program_counter: 0,
            },
        };
        cpu.add_a_u8(8);
        assert_eq!(8, cpu.registers.a);
        cpu.add_a_u8(255);
        assert_eq!(7, cpu.registers.a);
        assert_eq!(true, cpu.registers.f.carry);
        cpu.adc_a_u8(8);
        assert_eq!(16, cpu.registers.a);
    }
}
