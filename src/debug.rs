use crate::{
    chunk::{Chunk, OpCode},
    compile::INTERNER,
    value::{as_obj, as_string},
    vm::Vm,
};

pub fn disassemble_chunk(chunk: &Chunk, name: u32, interner: &INTERNER) {
    let mut name = interner.lock().as_ref().unwrap().lookup(name);
    if name == "" {
        name = "script";
    }
    println!("== {} ==", name);

    for (offset, instruction) in chunk.code.iter().enumerate() {
        disassemble_instruction(&chunk, instruction, offset.try_into().unwrap());
    }
}

pub fn disassemble_instruction(chunk: &Chunk, instruction: &OpCode, offset: usize) {
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
                "{} {:4} {:?}",
                "OpConstant",
                c,
                chunk.constants.get(c.clone() as usize).unwrap()
            );
        }
        OpCode::OpConstantLong(c) => {
            println!(
                "{} {:4} {:?}",
                "OpConstantLong",
                c,
                chunk.constants.get(c.clone() as usize).unwrap()
            );
        }
        OpCode::OpNegate => simple_instruction("OpNegate", offset),
        OpCode::OpAdd => simple_instruction("OpAdd", offset),
        OpCode::OpSubtract => simple_instruction("OpSubtract", offset),
        OpCode::OpMultiply => simple_instruction("OpMultiply", offset),
        OpCode::OpDivide => simple_instruction("OpDivide", offset),
        OpCode::OpNil => simple_instruction("OpNil", offset),
        OpCode::OpTrue => simple_instruction("OpTrue", offset),
        OpCode::OpFalse => simple_instruction("OpFalse", offset),
        OpCode::OpNot => simple_instruction("OpNot", offset),
        OpCode::OpEqual => simple_instruction("OpEqual", offset),
        OpCode::OpGreater => simple_instruction("OpGreater", offset),
        OpCode::OpLess => simple_instruction("OpLess", offset),
        OpCode::OpPrint => simple_instruction("OpPrint", offset),
        OpCode::OpPop => simple_instruction("OpPop", offset),
        OpCode::OpDefineGlobal(id) => {
            println!(
                "{} {:4} {:?}",
                "OpDefineGlobal",
                id,
                chunk.constants.get(id.clone() as usize).unwrap()
            );
        }
        OpCode::OpGetGlobal(id) => {
            println!(
                "{} {:4} {:?}",
                "OpDefineGlobal",
                id,
                chunk.constants.get(id.clone() as usize).unwrap()
            );
        }
        OpCode::OpSetGlobal(id) => {
            let value = as_obj(Vm::read_constant(chunk, *id as usize));
            let name = as_string(INTERNER.lock().as_deref().unwrap(), &value);
            println!("{} {:4} {}", "OpSetGlobal", id, name)
        }
        OpCode::OpGetLocal(slot) => println!("{} {:4}", "OpGetLocal", slot),
        OpCode::OpSetLocal(slot) => println!("{} {:4}", "OpSetLocal", slot),
        OpCode::OpJumpIfFalse(offset) => println!("{} {:8}", "OpJumpIfFalse", offset),
        OpCode::OpJump(offset) => println!("{} {:8}", "OpJump", offset),
        OpCode::OpLoop(offset) => println!("{} {:8}", "OpLoop", offset),
        OpCode::OpCall(arg_count) => {
            println!("{} {:8}", "OpCall", arg_count);
        }
    }
}

pub fn simple_instruction(name: &str, offset: usize) {
    println!("{}", name);
}
