use crate::{
    chunk::{Chunk, OpCode},
    compile::Compiler,
    debug::disassemble_instruction,
    value::{
        as_bool, as_number, as_string, bool_val, is_bool, is_nil, is_number, is_string, nil_val,
        number_val, string_val, values_equal, Value,
    },
};

pub struct Vm {
    pub chunk: Chunk,
    pub stack: [Option<Value>; 256],
    pub stack_top: u8,
}

#[derive(Debug)]
struct ErrorData {
    line: usize,
    message: String,
}

#[derive(Debug)]
pub enum InterpretError {
    InterpretParserError,
    InterpretCompileError(ErrorData),
    InterpretRuntimeError(ErrorData),
}

impl std::fmt::Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpretError::InterpretParserError => write!(f, "Parser error."),
            InterpretError::InterpretCompileError(e) => {
                write!(f, "[line {}] {}", e.line, e.message)
            }
            InterpretError::InterpretRuntimeError(e) => {
                write!(f, "[line {}] {}", e.line, e.message)
            }
        }
    }
}

impl std::error::Error for InterpretError {}

impl Vm {
    pub fn new(source: &str) -> Result<Self, InterpretError> {
        let chunk = Compiler::new(source).compile()?;
        const INIT: Option<Value> = None;
        const SIZE: usize = 256;
        Ok(Self {
            chunk,
            // instructions: chunk.code,
            stack: [INIT; SIZE],
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
        for (i, instruction) in code.iter().enumerate() {
            let line = self.chunk.get_line(i);
            if debug {
                for value in &self.stack {
                    println!("[ {:?} ]", value);
                }
                disassemble_instruction(&self.chunk, instruction, i);
            }
            match instruction {
                OpCode::OpConstant(c) => {
                    let constant = self.chunk.constants.get(c.clone() as usize).unwrap();
                    self.push(constant.clone());
                    Ok(())
                }
                OpCode::OpConstantLong(c) => {
                    let constant = self.chunk.constants.get(c.clone() as usize).unwrap();
                    println!("{:?}", constant);
                    Ok(())
                }
                OpCode::OpReturn => {
                    println!("{:?}", self.pop());
                    return Ok(());
                }
                OpCode::OpNegate => {
                    if let Value::ValNumber(number) = self.peek(0) {
                        let number = -number;
                        self.push(number_val(number));
                    } else {
                        return Err(InterpretError::InterpretRuntimeError(ErrorData {
                            line,
                            message: "Operand must be a number".to_string(),
                        }));
                    }
                    Ok(())
                }
                OpCode::OpAdd => {
                    if is_string(self.peek(0)) && is_string(self.peek(1)) {
                        self.concatenate()?;
                    } else {
                        self.binary_op('+', line)?;
                    }
                    Ok(())
                }
                OpCode::OpSubtract => self.binary_op('-', line),
                OpCode::OpMultiply => self.binary_op('*', line),
                OpCode::OpDivide => self.binary_op('/', line),
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
                    self.push(bool_val(values_equal(&a, &b)));
                    Ok(())
                }
                OpCode::OpGreater => self.binary_op('>', line),
                OpCode::OpLess => self.binary_op('<', line),
            }?;
        }
        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top as usize] = Some(value);
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        match self.stack[self.stack_top as usize].take() {
            Some(v) => v,
            None => panic!(),
        }
    }

    fn peek(&self, distance: usize) -> &Value {
        match &self.stack[self.stack_top as usize - 1 - distance] {
            Some(v) => v,
            None => panic!(),
        }
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn binary_op(&mut self, op: char, line: usize) -> Result<(), InterpretError> {
        if !is_number(self.peek(0)) || !is_number(self.peek(1)) {
            return Err(InterpretError::InterpretRuntimeError(ErrorData {
                line,
                message: "Operands must be numbers.".to_string(),
            }));
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
        return is_nil(&value) || is_bool(&value) && !as_bool(value);
    }

    fn concatenate(&mut self) -> Result<(), InterpretError> {
        // println!("hello");
        let b = as_string(self.pop());
        let a = as_string(self.pop());

        self.push(string_val(a + &b));
        // println!("{:?}", self.stack);
        Ok(())
    }
}
