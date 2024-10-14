mod cpu;
mod opcodes;
use cpu::{Cpu, Flags, Registers};
use opcodes::{Arithemtic, Operation, R8};
fn main() {
    let cpu = Cpu {
        registers: Registers {
            a: 0,
            b: 0,
            c: 0,
            e: 0,
            d: 0,
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
        program_counter: 0,
    };
    let instruction = opcodes::match_instruction(0x81).unwrap();
    match instruction {
        Operation::Arithmetic(Arithemtic::ADD_A(opcode)) => {
            println!("arithmetic");
            println!("opcode: {}", opcode);
            let param = opcode & 7;
            println!("param: {}", param);
        }
        _ => (),
    }
    println!("Hello, world!");
}
