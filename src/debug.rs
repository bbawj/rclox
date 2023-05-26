use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: Chunk, name: &str) {
    println!("== {} ==", name);

    for (offset, instruction) in chunk.code.iter().enumerate() {
        disassemble_instruction(&chunk, instruction, offset.try_into().unwrap());
    }
}

pub fn disassemble_instruction(chunk: &Chunk, instruction: &OpCode, offset: usize) -> usize {
    print!("{:04} ", offset);

    let line = chunk.get_line(offset);
    if offset > 0 && line == chunk.get_line(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", line);
    }

    match instruction {
        OpCode::OpReturn => simple_instruction("OpReturn", offset),
        OpCode::OpConstant(c) => {
            println!(
                "{} {:4} {}",
                "OpConstant",
                c,
                chunk.constants.get(c.clone() as usize).unwrap()
            );
            return offset + 2;
        }
        OpCode::OpConstantLong(c) => {
            println!(
                "{} {:4} {}",
                "OpConstantLong",
                c,
                chunk.constants.get(c.clone() as usize).unwrap()
            );
            return offset + 4;
        }
        OpCode::OpNegate => simple_instruction("OpNegate", offset),
        OpCode::OpAdd => simple_instruction("OpAdd", offset),
        OpCode::OpSubtract => simple_instruction("OpSubtract", offset),
        OpCode::OpMultiply => simple_instruction("OpMultiply", offset),
        OpCode::OpDivide => simple_instruction("OpDivide", offset),
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    return offset + 1;
}
