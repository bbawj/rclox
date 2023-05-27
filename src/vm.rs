use std::{cell::RefCell, rc::Weak};

use crate::{
    chunk::{Chunk, OpCode},
    compile::{self, Compiler},
    debug::disassemble_instruction,
    value::Value,
};

pub struct Vm {
    pub chunk: Chunk,
    // pub instructions: Vec<OpCode>,
    pub stack: [Value; 256],
    pub stack_top: u8,
}

pub struct TokenError {
    line: usize,
    message: String,
}

#[derive(Debug)]
pub enum InterpretError {
    InterpretParserError,
    InterpretCompileError,
    InterpretRuntimeError,
}

impl std::fmt::Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpretError::InterpretParserError => write!(f, "Parser error."),
            InterpretError::InterpretCompileError => write!(f, "Compile error."),
            InterpretError::InterpretRuntimeError => write!(f, "Runtime Error"),
        }
    }
}

impl std::error::Error for InterpretError {}

impl Vm {
    pub fn new(source: &str) -> Result<Self, InterpretError> {
        let chunk = Compiler::new(source).compile()?;
        Ok(Self {
            chunk,
            // instructions: chunk.code,
            stack: [0.0; 256],
            stack_top: 0,
        })
    }

    pub fn interpret(&mut self, debug: bool) -> Result<(), InterpretError> {
        // self.chunk = chunk;
        // self.instructions = &self.chunk.code;
        self.run(debug)
    }

    fn run(&mut self, debug: bool) -> Result<(), InterpretError> {
        let code = self.chunk.code.clone();
        println!("{:?}", code);
        for (i, instruction) in code.iter().enumerate() {
            if debug {
                for value in self.stack {
                    println!("[ {} ]", value);
                }
                disassemble_instruction(&self.chunk, instruction, i);
            }
            match instruction {
                OpCode::OpConstant(c) => {
                    let constant = self.chunk.constants.get(c.clone() as usize).unwrap();
                    self.push(constant.clone());
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
                    self.stack[self.stack_top as usize - 1] =
                        -self.stack[self.stack_top as usize - 1];
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
