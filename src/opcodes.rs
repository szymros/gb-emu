use std::collections::HashMap;
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
        match byte & mask {
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
enum R16 {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
}

impl R16 {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match byte & mask {
            0 => Some(R16::BC),
            1 => Some(R16::DE),
            2 => Some(R16::HL),
            3 => Some(R16::SP),
            _ => None,
        }
    }
}

#[derive(Copy, Clone)]
enum R16Stk {
    BC = 0,
    DE = 1,
    HL = 2,
    AF = 3,
}
impl R16Stk {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match byte & mask {
            0 => Some(R16Stk::BC),
            1 => Some(R16Stk::DE),
            2 => Some(R16Stk::HL),
            3 => Some(R16Stk::AF),
            _ => None,
        }
    }
}

#[derive(Copy, Clone)]
enum R16Mem {
    BC = 0,
    DE = 1,
    HL_UP = 2,
    HL_DOWN = 3,
}

impl R16Mem {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match byte & mask {
            0 => Some(R16Mem::BC),
            1 => Some(R16Mem::DE),
            2 => Some(R16Mem::HL_UP),
            3 => Some(R16Mem::HL_DOWN),
            _ => None,
        }
    }
}

#[derive(Copy, Clone)]
enum Condition {
    NotZero = 0,
    Zero = 1,
    NoCarry = 2,
    Carry = 3,
}
impl Condition {
    fn from_masked_u8(byte: u8, mask: u8) -> Option<Self> {
        match byte & mask {
            0 => Some(Condition::NotZero),
            1 => Some(Condition::Zero),
            2 => Some(Condition::NoCarry),
            3 => Some(Condition::Carry),
            _ => None,
        }
    }
}

pub enum Arithemtic {
    ADD_A(R8),
    ADC_A(R8),
    SUB_A(R8),
    SBC_A(R8),
    AND_A(R8),
    XOR_A(R8),
    OR_A(R8),
    CP_A(R8),
}

pub enum CbOperation {
    RlcR8(R8),
    RrcR8(R8),
    RlR8(R8),
    RrR8(R8),
    SlaR8(R8),
    SraR8(R8),
    SwapR8(R8),
    SrlR8(R8),
    BitR8(u8, R8),
    ResR8(u8, R8),
    SetR8(u8, R8),
}

pub enum Operation {
    Arithmetic(Arithemtic),
    LoadR8R8(R8, R8),
    LoadR16Imm8(R16),
    LoadR16MemA(R16Mem),
    LoadImm16Sp,
    IncR16(R16),
    DecR16(R16),
    AddHlR16(R16),
    IncR8(R8),
    DecR8(R8),
    LoadR8Imm8(R8),
    RLCA,
    RRCA,
    RLA,
    RRA,
    DAA,
    CPL,
    SCF,
    CCF,
    JrImm8,
    JrCondImm8(Condition),
    AddAImm8,
    AdcAImm8,
    SubAImm8,
    SbcAImm8,
    AndAImm8,
    XorAImm8,
    OrAImm8,
    CpAImm8,
    RetCond(Condition),
    Ret,
    RetI,
    JpCondImm16(Condition),
    JpImm16,
    JpHl,
    CallCondImm16(Condition),
    CallImm16,
    RstTgt3,
    PopR16(R16Stk),
    PushR16(R16Stk),
    LdhCAddrA,
    LdhImm8AddrA,
    LdImm16AddrA,
    LdhACAddr,
    LdhAImm8Addr,
    LdAImm16Addr,
    AddSpImm8,
    LdHlSpImm8,
    LdSpHl,
    Di,
    Ei,
    CbOperation(CbOperation),
}

pub enum ParamMasks {
    Arithmetic,
    LoadR8R8Src,
    LoadR8R8Dst,
    R16Block1,
    R8Block1,
    CondBlock4,
    R8CB,
    Bit3CB,
}
impl ParamMasks {
    fn get_mask(&self) -> u8 {
        match self {
            ParamMasks::Arithmetic => 0b111,
            ParamMasks::LoadR8R8Src => 0b111,
            ParamMasks::LoadR8R8Dst => 0b111000,
            ParamMasks::R16Block1 => 0b110000,
            ParamMasks::R8Block1 => 0b111000,
            ParamMasks::CondBlock4 => 0b11000000,
            ParamMasks::R8CB => 0b111,
            ParamMasks::Bit3CB => 0b111000,
        }
    }
}
pub(crate) fn match_instruction(opcode: u8) -> Option<Operation> {
    match opcode {
        0 => None,

        // ---------------------- BLOCK 1 ----------------------
        // ld dst, src

        // ld r16, imm16
        0x01 | 0x11 | 0x21 | 0x31 => Some(Operation::LoadR16Imm8(
            R16::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
        )),

        // ld r16mem, a
        0x02 | 0x12 | 0x22 | 0x32 => Some(Operation::LoadR16MemA(
            R16Mem::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
        )),

        // ld a, r16mem
        0x0A | 0x1A | 0x2A | 0x3A => Some(Operation::LoadR16MemA(
            R16Mem::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
        )),

        // ld imm16, sp
        8 => Some(Operation::LoadImm16Sp),

        // inc r16
        0x03 | 0x13 | 0x23 | 0x33 => Some(Operation::IncR16(
            R16::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
        )),

        // dec r16
        0x0B | 0x1B | 0x2B | 0x3B => Some(Operation::DecR16(
            R16::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
        )),

        // add hl, r16
        0x09 | 0x19 | 0x29 | 0x39 => Some(Operation::AddHlR16(
            R16::from_masked_u8(opcode, ParamMasks::R16Block1.get_mask()).unwrap(),
        )),

        // inc r8
        0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C | 0x3C => Some(Operation::IncR8(
            R8::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
        )),

        // dec r8
        0x05 | 0x0D | 0x15 | 0x1D | 0x25 | 0x2D | 0x3D => Some(Operation::DecR8(
            R8::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
        )),

        // ld r8, imm8
        0x06 | 0x0E | 0x16 | 0x1E | 0x26 | 0x2E | 0x3E => Some(Operation::LoadR8Imm8(
            R8::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
        )),

        0x07 => Some(Operation::RLCA),
        0x17 => Some(Operation::RLA),
        0x27 => Some(Operation::DAA),
        0x37 => Some(Operation::SCF),
        0x0F => Some(Operation::RRCA),
        0x1F => Some(Operation::RRA),
        0x2F => Some(Operation::CPL),
        0x3F => Some(Operation::CCF),

        // jr imm8
        0x18 => Some(Operation::JrImm8),

        // jr cond imm8
        0x20 | 0x28 | 0x30 | 0x38 => Some(Operation::JrCondImm8(
            Condition::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
        )),

        // ------------------------------ Block 2 load r8 r8 ------------------------------
        // ld r8 r8, 0 1 r8 dst r8 src
        0x40..0x6F => Some(Operation::LoadR8R8(
            R8::from_masked_u8(opcode, ParamMasks::LoadR8R8Src.get_mask()).unwrap(),
            R8::from_masked_u8(opcode, ParamMasks::LoadR8R8Src.get_mask()).unwrap(),
        )),

        // ------------- Block 3 Arithmetic ------------

        // add a, r8	1	0	0	0	0	Operand (r8)
        0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 => Some(Operation::Arithmetic(
            Arithemtic::ADD_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),
        // adc a, r8	1	0	0	0	1	Operand (r8)
        0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D | 0x8E | 0xF => Some(Operation::Arithmetic(
            Arithemtic::ADC_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),
        // sub a, r8	1	0	0	1	0	Operand (r8)
        0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 => Some(Operation::Arithmetic(
            Arithemtic::SUB_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),
        // sbc a, r8	1	0	0	1	1	Operand (r8)
        0x98 | 0x99 | 0x9A | 0x9B | 0x9C | 0x9D | 0x9E | 0x9F => Some(Operation::Arithmetic(
            Arithemtic::SBC_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),
        // and a, r8	1	0	1	0	0	Operand (r8)
        0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 | 0xA6 | 0xA7 => Some(Operation::Arithmetic(
            Arithemtic::AND_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),
        // xor a, r8	1	0	1	0	1	Operand (r8)
        0xA8 | 0xA9 | 0xAA | 0xAB | 0xAC | 0xAD | 0xAE | 0xAF => Some(Operation::Arithmetic(
            Arithemtic::XOR_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),
        // or a, r8	1	0	1	1	0	Operand (r8)
        0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 => Some(Operation::Arithmetic(
            Arithemtic::OR_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),
        // cp a, r8	1	0	1	1	1	Operand (r8)
        0xB8 | 0xB9 | 0xBA | 0xBB | 0xBC | 0xBD | 0xBE | 0xBF => Some(Operation::Arithmetic(
            Arithemtic::CP_A(R8::from_masked_u8(opcode, ParamMasks::Arithmetic as u8).unwrap()),
        )),

        // ---------------------------------- Block 4 ----------------------------------
        // add a, imm8
        0xC6 => Some(Operation::AddAImm8),
        // adc a, imm8
        0xCE => Some(Operation::AdcAImm8),
        // sub a, imm8
        0xD6 => Some(Operation::SubAImm8),
        // sbc a, imm8
        0xDE => Some(Operation::SbcAImm8),
        // and a, imm8
        0xE6 => Some(Operation::AndAImm8),
        // xor a, imm8
        0xEE => Some(Operation::XorAImm8),
        // or a, imm8
        0xF6 => Some(Operation::OrAImm8),
        // ret cond
        0xC0 | 0xC8 | 0xD0 | 0xD8 => Some(Operation::RetCond(
            Condition::from_masked_u8(opcode, ParamMasks::CondBlock4.get_mask()).unwrap(),
        )),
        // ret
        0xC9 => Some(Operation::Ret),
        // RetI,
        0xD9 => Some(Operation::RetI),
        // JpCondImm16(Condition),
        0xC2 | 0xCA | 0xD2 | 0xDA => Some(Operation::JpCondImm16(
            Condition::from_masked_u8(opcode, ParamMasks::CondBlock4.get_mask()).unwrap(),
        )),
        // JpImm16,
        0xC3 => Some(Operation::JpImm16),

        // JpHl,
        0xE9 => Some(Operation::JpHl),
        // CallCondImm16(Condition),
        0xC4 | 0xCC | 0xD4 | 0xDC => Some(Operation::CallCondImm16(
            Condition::from_masked_u8(opcode, ParamMasks::CondBlock4.get_mask()).unwrap(),
        )),
        // CallImm16,
        0xCD => Some(Operation::CallImm16),
        // RstTgt3,
        0xC7 | 0xCF | 0xD7 | 0xDF | 0xE7 | 0xEF | 0xF7 | 0xFF => Some(Operation::RstTgt3),
        // PopR16(R16Stk),
        0xC1 | 0xD1 | 0xE1 | 0xF1 => Some(Operation::PopR16(
            R16Stk::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
        )),
        // PushR16(R16Stk),
        0xC5 | 0xD5 | 0xE5 | 0xF5 => Some(Operation::PushR16(
            R16Stk::from_masked_u8(opcode, ParamMasks::R8Block1.get_mask()).unwrap(),
        )),
        // LdhCAddrA,
        0xE2 => Some(Operation::LdhCAddrA),
        // LdhImm8AddrA,
        0xE0 => Some(Operation::LdhImm8AddrA),
        // LdImm16AddrA,
        0xEA => Some(Operation::LdImm16AddrA),
        // LdhACAddr,
        0xF2 => Some(Operation::LdhACAddr),
        // LdhAImm8Addr,
        0xF0 => Some(Operation::LdhAImm8Addr),
        // LdAImm16Addr,
        0xFA => Some(Operation::LdAImm16Addr),
        // AddSpImm8,
        0xE8 => Some(Operation::AddSpImm8),
        // LdHlSpImm8,
        0xF8 => Some(Operation::LdHlSpImm8),
        // LdSpHl,
        0xF9 => Some(Operation::LdSpHl),
        // Di,
        0xF3 => Some(Operation::Di),
        // Ei,
        0xFB => Some(Operation::Ei),

        // ---------------------- CB PREFIXED OPCODES ----------------------
        0xCB => Some(Operation::CbOperation(CbOperation::RlR8(R8::A))),
        _ => None,
    }
}
