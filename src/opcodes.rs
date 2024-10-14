#[derive(Copy, Clone)]
pub enum R8 {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    HL = 6,
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
            6 => Some(R8::HL),
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

#[derive(Copy, Clone)]
enum R16Stk {
    BC = 0,
    DE = 1,
    HL = 2,
    AF = 3,
}

#[derive(Copy, Clone)]
enum R16Mem {
    BC = 0,
    DE = 1,
    HL_UP = 2,
    HL_DOWN = 3,
}

#[derive(Copy, Clone)]
enum Condition {
    NotZero = 0,
    Zero = 1,
    NoCarry = 2,
    Carry = 3,
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

pub enum LoadR8R8 {
    LD_R8_R8(R8, R8),
}

pub enum Operation {
    Arithmetic(Arithemtic),
}

pub enum ParamMasks {
    Arithmetic = 7,
}
pub(crate) fn match_instruction(opcode: u8) -> Option<Operation> {
    match opcode {
        // Loads
        //

        // ------------- Arithmetic ------------

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

        // ---------- Arthmetic end ------------
        _ => None,
    }
}

// struct Opcode {
//     code: u8,
//     mnemonic: &'static str,
//     param_type: Vec<ParamTypes>,
//     param_bits: Vec<u8>,
//     cycles: u8,
// }
//
// impl Opcode {
//     fn new(
//         base_code: u8,
//         mnemonic: &'static str,
//         param_type: Vec<ParamTypes>,
//         param_bits: Vec<u8>,
//         cycles: u8,
//     ) -> Opcode {
//         let mut full_code = base_code;
//         for (param, bits) in param_type.iter().zip(param_bits.iter()) {
//             let val = match param {
//                 ParamTypes::R8(inner) => *inner as u8,
//                 ParamTypes::R16(inner) => *inner as u8,
//                 ParamTypes::R16Stk(inner) => *inner as u8,
//                 ParamTypes::R16Mem(inner) => *inner as u8,
//                 ParamTypes::Condition(inner) => *inner as u8,
//             };
//             full_code = full_code | (val << bits);
//         }
//
//         Opcode {
//             code: full_code,
//             mnemonic,
//             param_type,
//             param_bits,
//             cycles,
//         }
//     }
// }
