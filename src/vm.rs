use std::collections::HashMap;

use crate::{
    chunk::{Chunk, OpCode},
    compile::Compiler,
    interner::Interner,
    value::{
        as_bool, as_number, as_obj, as_string, bool_val, is_bool, is_nil, is_number, is_string,
        nil_val, number_val, string_val, values_equal, Value,
    },
};

pub struct Vm {
    pub chunk: Chunk,
    pub stack: [Option<Value>; 256],
    pub stack_top: u8,
    pub compiler: Compiler,
    globals: HashMap<&'static str, Value>,
    ip: usize,
}

#[derive(Debug)]
pub struct ErrorData {
    pub line: usize,
    pub message: String,
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

impl std::fmt::Debug for Vm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== VM ==")?;

        for (offset, instruction) in self.chunk.code.iter().enumerate() {
            write!(f, "{:04} ", offset)?;

            let line = self.chunk.get_line(offset);
            if offset > 0 && line == self.chunk.get_line(offset - 1) {
                write!(f, "   | ")?;
            } else {
                write!(f, "{:4} ", line)?;
            }

            match instruction {
                OpCode::OpReturn => writeln!(f, "OpReturn"),
                OpCode::OpConstant(c) => {
                    write!(f, "{} {:4} ", "OpConstant", c)?;
                    match self.chunk.get_constant(*c as usize).unwrap() {
                        Value::ValBool(v) => writeln!(f, "{}", v),
                        Value::ValNumber(v) => writeln!(f, "{}", v),
                        Value::ValNil => writeln!(f),
                        Value::ValObj(v) => {
                            writeln!(f, "{}", as_string(&self.compiler.strings, v))
                        }
                    }
                }
                OpCode::OpConstantLong(c) => writeln!(
                    f,
                    "{} {:4} {:?}",
                    "OpConstantLong",
                    c,
                    self.chunk.get_constant(*c as usize).unwrap()
                ),
                OpCode::OpNegate => writeln!(f, "OpNegate"),
                OpCode::OpAdd => writeln!(f, "OpAdd"),
                OpCode::OpSubtract => writeln!(f, "OpSubtract"),
                OpCode::OpMultiply => writeln!(f, "OpMultiply"),
                OpCode::OpDivide => writeln!(f, "OpDivide"),
                OpCode::OpNil => writeln!(f, "OpNil"),
                OpCode::OpTrue => writeln!(f, "OpTrue"),
                OpCode::OpFalse => writeln!(f, "OpFalse"),
                OpCode::OpNot => writeln!(f, "OpNot"),
                OpCode::OpEqual => writeln!(f, "OpEqual"),
                OpCode::OpGreater => writeln!(f, "OpGreater"),
                OpCode::OpLess => writeln!(f, "OpLess"),
                OpCode::OpPrint => writeln!(f, "OpPrint"),
                OpCode::OpPop => writeln!(f, "OpPop"),
                OpCode::OpDefineGlobal(id) => {
                    let value = as_obj(
                        self.chunk
                            .constants
                            .get(id.clone() as usize)
                            .unwrap()
                            .clone(),
                    );
                    let name = as_string(&self.compiler.strings, &value);
                    writeln!(f, "{} {:4} {}", "OpDefineGlobal", id, name)
                }
                OpCode::OpGetGlobal(id) => {
                    let value = as_obj(
                        self.chunk
                            .constants
                            .get(id.clone() as usize)
                            .unwrap()
                            .clone(),
                    );
                    let name = as_string(&self.compiler.strings, &value);
                    writeln!(f, "{} {:4} {}", "OpGetGlobal", id, name)
                }
                OpCode::OpSetGlobal(id) => {
                    let value = as_obj(
                        self.chunk
                            .constants
                            .get(id.clone() as usize)
                            .unwrap()
                            .clone(),
                    );
                    let name = as_string(&self.compiler.strings, &value);
                    writeln!(f, "{} {:4} {}", "OpSetGlobal", id, name)
                }
                OpCode::OpGetLocal(slot) => writeln!(f, "{} {:4}", "OpGetLocal", slot),
                OpCode::OpSetLocal(slot) => writeln!(f, "{} {:4}", "OpSetLocal", slot),
                OpCode::OpJumpIfFalse(offset) => writeln!(f, "{} {:8}", "OpJumpIfFalse", offset),
                OpCode::OpJump(offset) => writeln!(f, "{} {:8}", "OpJump", offset),
            }?;
        }
        Ok(())
    }
}

impl Vm {
    pub fn new(source: &str) -> Result<Self, InterpretError> {
        let interner = Interner::with_capacity(256);
        let mut compiler = Compiler::new(source, interner);
        let chunk = compiler.compile()?;
        const INIT: Option<Value> = None;
        const SIZE: usize = 256;
        Ok(Self {
            chunk,
            // instructions: chunk.code,
            stack: [INIT; SIZE],
            stack_top: 0,
            compiler,
            globals: HashMap::new(),
            ip: 0,
        })
    }

    pub fn interpret(&mut self, debug: bool) -> Result<(), InterpretError> {
        self.run(debug)?;
        Ok(())
    }

    fn run(&mut self, debug: bool) -> Result<(), InterpretError> {
        let num_instructions = self.chunk.code.len();
        println!("{:?}", self);

        while self.ip < num_instructions {
            let line = self.chunk.get_line(self.ip);
            if debug {
                println!("=== STACK ===");
                for value in &self.stack {
                    println!("[ {:?} ]", value);
                }
            }
            match &self.chunk.code[self.ip].clone() {
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
                OpCode::OpReturn => Ok(()),
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
                    let value = Vm::is_falsey(&self.pop());
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
                OpCode::OpPrint => {
                    let val = self.pop();
                    if let Value::ValObj(_) = &val {
                        println!("{}", as_string(&self.compiler.strings, &as_obj(val)))
                    } else {
                        println!("{:?}", val);
                    }
                    Ok(())
                }
                OpCode::OpPop => {
                    self.pop();
                    Ok(())
                }
                OpCode::OpDefineGlobal(id) => {
                    let val = self.pop();
                    let name = self.compiler.strings.lookup(id.clone());
                    self.globals.insert(name, val);
                    Ok(())
                }
                OpCode::OpGetGlobal(id) => {
                    let value = as_obj(self.chunk.get_constant(*id as usize).unwrap().clone());
                    let name = as_string(&self.compiler.strings, &value);
                    if let Some(value) = self.globals.get(name) {
                        self.push(value.clone());
                    } else {
                        return Err(InterpretError::InterpretRuntimeError(ErrorData {
                            line,
                            message: format!("Undefined variable '{}'.", name),
                        }));
                    }
                    Ok(())
                }
                OpCode::OpSetGlobal(id) => {
                    let value = as_obj(self.chunk.get_constant(*id as usize).unwrap().clone());
                    let name = as_string(&self.compiler.strings, &value);

                    if let Some(_) = self.globals.get(name) {
                        let val = self.peek(0);
                        self.globals.insert(name, val.clone());
                    } else {
                        return Err(InterpretError::InterpretRuntimeError(ErrorData {
                            line,
                            message: format!("Undefined variable '{}'.", name),
                        }));
                    }
                    Ok(())
                }
                OpCode::OpGetLocal(slot) => {
                    let value = self.stack[*slot as usize].as_ref().unwrap();
                    self.push(value.clone());
                    Ok(())
                }
                OpCode::OpSetLocal(slot) => {
                    self.stack[*slot as usize] = Some(self.peek(0).clone());
                    Ok(())
                }
                OpCode::OpJumpIfFalse(offset) => {
                    let value = self.peek(0);
                    if Self::is_falsey(value) {
                        self.ip += *offset as usize;
                        continue;
                    }
                    Ok(())
                }
                OpCode::OpJump(offset) => {
                    self.ip += *offset as usize;
                    continue;
                }
            }?;
            self.ip += 1;
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

    fn is_falsey(value: &Value) -> bool {
        return is_nil(value) || is_bool(value) && !as_bool(value);
    }

    fn concatenate(&mut self) -> Result<(), InterpretError> {
        let b = as_obj(self.pop());
        let a = as_obj(self.pop());
        let str_b = as_string(&self.compiler.strings, &b);
        let str_a = as_string(&self.compiler.strings, &a);
        let id = self.compiler.strings.intern(&(str_a.to_string() + str_b));

        self.push(string_val(id));
        Ok(())
    }
}
