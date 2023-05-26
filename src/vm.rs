use std::{cell::RefCell, rc::Weak};

use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_instruction,
    value::Value,
};

pub struct Vm<'a> {
    pub chunk: &'a Chunk,
    pub instructions: &'a Vec<OpCode>,
    pub stack: [Value; 256],
    pub stack_top: u8,
}

pub struct TokenError {
    line: usize,
    message: String,
}

pub enum InterpretError {
    InterpretTokenError(TokenError),
    InterpretCompileError,
    InterpretRuntimeError,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            instructions: &chunk.code,
            stack: [0.0; 256],
            stack_top: 0,
        }
    }
    pub fn interpret(&mut self, debug: bool) -> Result<(), InterpretError> {
        // self.chunk = chunk;
        // self.instructions = &self.chunk.code;
        self.run(debug)
    }

    fn run(&mut self, debug: bool) -> Result<(), InterpretError> {
        for (i, instruction) in self.instructions.iter().enumerate() {
            if debug {
                for value in self.stack {
                    println!("[ {} ]", value);
                }
                disassemble_instruction(&self.chunk, instruction, i);
            }
            match instruction {
                OpCode::OpConstant(c) => {
                    let constant = self.chunk.constants.get(c.clone() as usize).unwrap();
                    self.push(*constant);
                }
                OpCode::OpConstantLong(c) => {
                    let constant = self.chunk.constants.get(c.clone() as usize).unwrap();
                    println!("{}", constant);
                }
                OpCode::OpReturn => {
                    println!("{}", self.pop());
                    return Ok(());
                }
                OpCode::OpNegate => {
                    self.stack[self.stack_top as usize] = -self.stack[self.stack_top as usize];
                }
                OpCode::OpAdd => self.binary_op('+'),
                OpCode::OpSubtract => self.binary_op('-'),
                OpCode::OpMultiply => self.binary_op('*'),
                OpCode::OpDivide => self.binary_op('/'),
            }
        }
        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top as usize] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top as usize];
    }

    fn binary_op(&mut self, op: char) {
        let b = self.pop();
        let a = self.pop();
        match op {
            '+' => self.push(a + b),
            '-' => self.push(a - b),
            '*' => self.push(a * b),
            '/' => self.push(a / b),
            _ => unreachable!(),
        }
    }
}
