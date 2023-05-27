use crate::{
    chunk::{Chunk, OpCode},
    compile::Compiler,
    debug::disassemble_instruction,
    value::{
        as_bool, as_number, bool_val, is_bool, is_nil, is_number, nil_val, number_val,
        values_equal, Value, ValueType,
    },
};

pub struct Vm {
    pub chunk: Chunk,
    pub stack: [Value; 256],
    pub stack_top: u8,
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
            stack: [Value {
                value_type: ValueType::ValNil,
                _as: crate::value::As { number: 0.0 },
            }; 256],
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
                    self.push(*constant);
                    Ok(())
                }
                OpCode::OpConstantLong(c) => {
                    let constant = self.chunk.constants.get(c.clone() as usize).unwrap();
                    println!("{}", constant);
                    Ok(())
                }
                OpCode::OpReturn => {
                    println!("{}", self.pop());
                    return Ok(());
                }
                OpCode::OpNegate => {
                    if !is_number(self.peek(0)) {
                        return Err(InterpretError::InterpretRuntimeError);
                    }
                    let number = -as_number(self.pop());
                    self.push(number_val(number));
                    Ok(())
                }
                OpCode::OpAdd => self.binary_op('+'),
                OpCode::OpSubtract => self.binary_op('-'),
                OpCode::OpMultiply => self.binary_op('*'),
                OpCode::OpDivide => self.binary_op('/'),
                OpCode::OpNil => {
                    self.push(nil_val());
                    Ok(())
                }
                OpCode::OpTrue => {
                    self.push(bool_val(true));
                    Ok(())
                }
                OpCode::OpFalse => {
                    self.push(bool_val(false));
                    Ok(())
                }
                OpCode::OpNot => {
                    let value = Vm::is_falsey(self.pop());
                    self.push(bool_val(!value));
                    Ok(())
                }
                OpCode::OpEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(bool_val(values_equal(a, b)));
                    Ok(())
                }
                OpCode::OpGreater => self.binary_op('>'),
                OpCode::OpLess => self.binary_op('<'),
            }?;
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

    fn peek(&self, distance: usize) -> Value {
        return self.stack[self.stack_top as usize - 1 - distance];
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn binary_op(&mut self, op: char) -> Result<(), InterpretError> {
        if !is_number(self.peek(0)) || !is_number(self.peek(1)) {
            return Err(InterpretError::InterpretRuntimeError);
        }
        let b = as_number(self.pop());
        let a = as_number(self.pop());
        match op {
            '+' => self.push(number_val(a + b)),
            '-' => self.push(number_val(a - b)),
            '*' => self.push(number_val(a * b)),
            '/' => self.push(number_val(a / b)),
            '>' => self.push(bool_val(a > b)),
            '<' => self.push(bool_val(a < b)),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn is_falsey(value: Value) -> bool {
        return is_nil(value) || is_bool(value) && !as_bool(value);
    }
}
