use std::{
    collections::HashMap,
    time::{self, SystemTime},
};

use crate::{
    chunk::{Chunk, OpCode},
    compile::{Compiler, INTERNER},
    object::{Obj, ObjFunction},
    scanner::Scanner,
    value::{
        as_bool, as_function, as_number, as_obj, as_string, bool_val, is_bool, is_nil, is_number,
        is_string, native_val, nil_val, number_val, string_val, values_equal, Value,
    },
};

#[derive(Debug, Clone)]
pub struct CallFrame {
    function: ObjFunction,
    ip: usize,
    slots_start: usize,
}

const FRAMES_MAX: u8 = 64;
const STACK_MAX: u16 = 64 * u8::MAX as u16;

pub struct Vm {
    pub frames: [Option<CallFrame>; FRAMES_MAX as usize],
    pub frame_count: u8,
    pub stack: [Option<Value>; STACK_MAX as usize],
    pub stack_top: u8,
    pub compiler: Compiler,
    globals: HashMap<&'static str, Value>,
}

#[derive(Debug)]
pub struct CompileError {
    pub line: usize,
    pub message: String,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub line: usize,
    pub message: String,
}

#[derive(Debug)]
pub enum InterpretError {
    InterpretParserError,
    InterpretCompileError(CompileError),
    InterpretRuntimeError(RuntimeError),
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

impl From<CompileError> for InterpretError {
    fn from(value: CompileError) -> Self {
        InterpretError::InterpretCompileError(value)
    }
}

impl From<RuntimeError> for InterpretError {
    fn from(value: RuntimeError) -> Self {
        InterpretError::InterpretRuntimeError(value)
    }
}

impl std::error::Error for InterpretError {}

// impl std::fmt::Debug for Vm {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut frame_count = self.frame_count;
//         println!("{}", frame_count);
//         let mut frame = self.frames[frame_count as usize - 1]
//             .as_ref()
//             .unwrap()
//             .clone();
//         writeln!(f, "== VM ==")?;
//
//         loop {
//             let offset = frame.ip;
//             write!(f, "{:04} ", offset)?;
//
//             let line = frame.function.chunk.get_line(frame.ip.into());
//             if offset > 0 && line == frame.function.chunk.get_line(offset - 1) {
//                 write!(f, "   | ")?;
//             } else {
//                 write!(f, "{:4} ", line)?;
//             }
//
//             match self.read_byte(&mut frame) {
//                 OpCode::OpReturn => {
//                     writeln!(f, "OpReturn")?;
//                     frame_count -= 1;
//                     if frame_count == 0 {
//                         break Ok(());
//                     }
//                     frame = self.frames[frame_count as usize - 1]
//                         .as_ref()
//                         .unwrap()
//                         .clone();
//                     Ok(())
//                 }
//                 OpCode::OpConstant(c) => {
//                     write!(f, "{} {:4} ", "OpConstant", c)?;
//                     match Vm::read_constant(&frame, c as usize) {
//                         Value::ValBool(v) => writeln!(f, "{}", v),
//                         Value::ValNumber(v) => writeln!(f, "{}", v),
//                         Value::ValNil => writeln!(f),
//                         Value::ValObj(v) => {
//                             writeln!(f, "{}", as_string(INTERNER.lock().as_deref().unwrap(), &v))
//                         }
//                     }
//                 }
//                 OpCode::OpConstantLong(c) => writeln!(
//                     f,
//                     "{} {:4} {:?}",
//                     "OpConstantLong",
//                     c,
//                     Vm::read_constant(&frame.function.chunk, c as usize)
//                 ),
//                 OpCode::OpNegate => writeln!(f, "OpNegate"),
//                 OpCode::OpAdd => writeln!(f, "OpAdd"),
//                 OpCode::OpSubtract => writeln!(f, "OpSubtract"),
//                 OpCode::OpMultiply => writeln!(f, "OpMultiply"),
//                 OpCode::OpDivide => writeln!(f, "OpDivide"),
//                 OpCode::OpNil => writeln!(f, "OpNil"),
//                 OpCode::OpTrue => writeln!(f, "OpTrue"),
//                 OpCode::OpFalse => writeln!(f, "OpFalse"),
//                 OpCode::OpNot => writeln!(f, "OpNot"),
//                 OpCode::OpEqual => writeln!(f, "OpEqual"),
//                 OpCode::OpGreater => writeln!(f, "OpGreater"),
//                 OpCode::OpLess => writeln!(f, "OpLess"),
//                 OpCode::OpPrint => writeln!(f, "OpPrint"),
//                 OpCode::OpPop => writeln!(f, "OpPop"),
//                 OpCode::OpDefineGlobal(id) => {
//                     let value = as_obj(Vm::read_constant(&frame.function.chunk, id as usize));
//                     let name = as_string(INTERNER.lock().as_deref().unwrap(), &value);
//                     writeln!(f, "{} {:4} {}", "OpDefineGlobal", id, name)
//                 }
//                 OpCode::OpGetGlobal(id) => {
//                     let value = as_obj(Vm::read_constant(&frame.function.chunk, id as usize));
//                     let name = as_string(INTERNER.lock().as_deref().unwrap(), &value);
//                     writeln!(f, "{} {:4} {}", "OpGetGlobal", id, name)
//                 }
//                 OpCode::OpSetGlobal(id) => {
//                     let value = as_obj(Vm::read_constant(&frame.function.chunk, id as usize));
//                     let name = as_string(INTERNER.lock().as_deref().unwrap(), &value);
//                     writeln!(f, "{} {:4} {}", "OpSetGlobal", id, name)
//                 }
//                 OpCode::OpGetLocal(slot) => writeln!(f, "{} {:4}", "OpGetLocal", slot),
//                 OpCode::OpSetLocal(slot) => writeln!(f, "{} {:4}", "OpSetLocal", slot),
//                 OpCode::OpJumpIfFalse(offset) => writeln!(f, "{} {:8}", "OpJumpIfFalse", offset),
//                 OpCode::OpJump(offset) => writeln!(f, "{} {:8}", "OpJump", offset),
//                 OpCode::OpLoop(offset) => writeln!(f, "{} {:8}", "OpLoop", offset),
//                 OpCode::OpCall(arg_count) => {
//                     writeln!(f, "{} {:8}", "OpCall", arg_count),
//                     frame = self.frames[]
//                     writeln!(f, "{}", )
//                 }
//             }?;
//         }
//     }
// }

const INIT: Option<Value> = None;
const INIT_FRAME: Option<CallFrame> = None;

impl Vm {
    pub fn new() -> Result<Self, InterpretError> {
        let compiler = Compiler::new();

        let frames = [INIT_FRAME; FRAMES_MAX as usize];

        let stack = [INIT; STACK_MAX as usize];

        Ok(Self {
            stack,
            stack_top: 0,
            compiler,
            globals: HashMap::new(),
            frames,
            frame_count: 0,
        })
    }

    pub fn interpret(&mut self, source: &str, debug: bool) -> Result<(), InterpretError> {
        self.define_native("clock", clock_native);

        let scanner = Scanner::new(source.to_string());
        self.compiler.scanner = scanner;
        let function = self.compiler.compile()?;

        self.push(Value::ValObj(Box::new(Obj::ObjFunction(function.clone()))));

        self.call(*function, 0, 1)?;

        match self.run(debug) {
            Ok(_) => (),
            Err(e) => {
                println!("{}", e.message);
                for i in (0..self.frame_count).rev() {
                    let frame = self
                        .frames
                        .get(i as usize)
                        .as_ref()
                        .unwrap()
                        .as_ref()
                        .unwrap();
                    let function = &frame.function;
                    let instruction_offset = function.chunk.code.len() - 1 - frame.ip - 2;
                    print!("[line {}] in ", function.chunk.get_line(instruction_offset));
                    let name = INTERNER
                        .lock()
                        .as_deref()
                        .unwrap()
                        .lookup(function.name_lookup);
                    if name.is_empty() {
                        println!("script");
                    } else {
                        println!("{}", name);
                    }
                }

                self.reset_stack();
            }
        }
        Ok(())
    }

    fn read_byte(frame: &mut CallFrame) -> OpCode {
        let byte = &frame.function.chunk.code[frame.ip as usize];
        frame.ip += 1;
        byte.clone()
    }

    pub fn read_constant(chunk: &Chunk, loc: usize) -> Value {
        chunk.constants.get(loc).unwrap().clone()
    }

    fn run(&mut self, debug: bool) -> Result<(), RuntimeError> {
        let mut frame = self.frames[self.frame_count as usize - 1]
            .as_mut()
            .unwrap()
            .clone();

        loop {
            let line = frame.function.chunk.get_line(frame.ip.into());
            let byte = &Vm::read_byte(&mut frame);

            if debug {
                println!("=== STACK ===");
                println!("{:?}", byte);
                for value in &self.stack {
                    if value.is_none() {
                        break;
                    }
                    println!("[ {:?} ]", value);
                }
            }
            match byte {
                OpCode::OpConstant(c) => {
                    let constant = Vm::read_constant(&frame.function.chunk, (*c).into());
                    self.push(constant.clone());
                }
                OpCode::OpConstantLong(c) => {
                    // let constant = self.read_constant(&frame, *c);
                    // println!("{:?}", constant);
                }
                OpCode::OpReturn => {
                    let result = self.pop();
                    let slots_start = frame.slots_start;
                    // self.frames[self.frame_count as usize] = Some(frame);
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        break;
                    }

                    self.stack_top = slots_start as u8;
                    self.push(result);
                    frame = self.frames[self.frame_count as usize - 1]
                        .as_ref()
                        .unwrap()
                        .clone();
                }
                OpCode::OpNegate => {
                    if let Value::ValNumber(number) = self.peek(0) {
                        let number = -number;
                        self.push(number_val(number));
                    } else {
                        return Err(RuntimeError {
                            line,
                            message: "Operand must be a number".to_string(),
                        });
                    }
                }
                OpCode::OpAdd => {
                    if is_string(self.peek(0)) && is_string(self.peek(1)) {
                        self.concatenate();
                    } else {
                        self.binary_op('+', line)?;
                    }
                }
                OpCode::OpSubtract => self.binary_op('-', line)?,
                OpCode::OpMultiply => self.binary_op('*', line)?,
                OpCode::OpDivide => self.binary_op('/', line)?,
                OpCode::OpNil => {
                    self.push(nil_val());
                }
                OpCode::OpTrue => {
                    self.push(bool_val(true));
                }
                OpCode::OpFalse => {
                    self.push(bool_val(false));
                }
                OpCode::OpNot => {
                    let value = Vm::is_falsey(&self.pop());
                    self.push(bool_val(!value));
                }
                OpCode::OpEqual => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(bool_val(values_equal(&a, &b)));
                }
                OpCode::OpGreater => self.binary_op('>', line)?,
                OpCode::OpLess => self.binary_op('<', line)?,
                OpCode::OpPrint => {
                    let val = self.pop();
                    if let Value::ValObj(_) = &val {
                        println!(
                            "{}",
                            as_string(INTERNER.lock().as_deref().unwrap(), &as_obj(val))
                        )
                    } else {
                        println!("{:?}", val);
                    }
                }
                OpCode::OpPop => {
                    self.pop();
                }
                OpCode::OpDefineGlobal(id) => {
                    let val = self.pop();
                    let value = as_obj(Vm::read_constant(
                        &frame.function.chunk,
                        (*id).try_into().unwrap(),
                    ));
                    let name = as_string(INTERNER.lock().as_deref().unwrap(), &value);
                    self.globals.insert(name, val);
                }
                OpCode::OpGetGlobal(id) => {
                    let value = as_obj(Vm::read_constant(
                        &frame.function.chunk,
                        (*id).try_into().unwrap(),
                    ));
                    let name = as_string(INTERNER.lock().as_deref().unwrap(), &value);
                    if let Some(value) = self.globals.get(name) {
                        self.push(value.clone());
                    } else {
                        return Err(RuntimeError {
                            line,
                            message: format!("Undefined variable '{}'.", name),
                        });
                    }
                }
                OpCode::OpSetGlobal(id) => {
                    let value = as_obj(Vm::read_constant(
                        &frame.function.chunk,
                        (*id).try_into().unwrap(),
                    ));
                    let name = as_string(INTERNER.lock().as_deref().unwrap(), &value);

                    if let Some(_) = self.globals.get(name) {
                        let val = self.peek(0);
                        self.globals.insert(name, val.clone());
                    } else {
                        return Err(RuntimeError {
                            line,
                            message: format!("Undefined variable '{}'.", name),
                        });
                    }
                }
                OpCode::OpGetLocal(slot) => {
                    let value = self.stack[frame.slots_start + *slot as usize]
                        .as_ref()
                        .unwrap();
                    self.push(value.clone());
                }
                OpCode::OpSetLocal(slot) => {
                    self.stack[frame.slots_start + *slot as usize] = Some(self.peek(0).clone());
                }
                OpCode::OpJumpIfFalse(offset) => {
                    let value = self.peek(0);
                    if Self::is_falsey(value) {
                        frame.ip += *offset as usize - 1;
                    }
                }
                OpCode::OpJump(offset) => {
                    frame.ip += *offset as usize - 1;
                }
                OpCode::OpLoop(offset) => {
                    frame.ip -= *offset as usize + 1;
                }
                OpCode::OpCall(arg_count) => {
                    self.frames[self.frame_count as usize - 1] = Some(frame);
                    self.call_value(*arg_count, line)?;
                    frame = self.frames[self.frame_count as usize - 1]
                        .as_ref()
                        .unwrap()
                        .clone();
                }
            };
        }
        return Ok(());
    }

    fn call_value(&mut self, arg_count: u8, line: usize) -> Result<(), RuntimeError> {
        let callee = self.peek(arg_count.into());
        match callee {
            Value::ValObj(obj) => match obj.as_ref() {
                Obj::ObjFunction(function) => self.call(*function.clone(), arg_count, line),
                Obj::ObjNative(function) => {
                    let start_arg_index = (self.stack_top - arg_count) as usize;
                    let result = function(self.stack[start_arg_index..].to_vec());
                    self.stack_top -= arg_count + 1;
                    self.push(result);
                    Ok(())
                }
                _ => unreachable!(),
            },
            _ => {
                return Err(RuntimeError {
                    line,
                    message: "Can only call functions and classes.".to_string(),
                })
            }
        }
    }

    fn call(
        &mut self,
        function: ObjFunction,
        arg_count: u8,
        line: usize,
    ) -> Result<(), RuntimeError> {
        if arg_count != function.arity {
            return Err(RuntimeError {
                line,
                message: format!(
                    "Expected {} arguments but got {}.",
                    function.arity, arg_count
                ),
            });
        }

        if self.frame_count == FRAMES_MAX {
            return Err(RuntimeError {
                line,
                message: "Stack overflow.".to_string(),
            });
        }

        self.frames[self.frame_count as usize] = Some(CallFrame {
            function,
            ip: 0,
            slots_start: self.stack_top as usize - arg_count as usize - 1,
        });
        self.frame_count += 1;
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

    fn binary_op(&mut self, op: char, line: usize) -> Result<(), RuntimeError> {
        if !is_number(self.peek(0)) || !is_number(self.peek(1)) {
            return Err(RuntimeError {
                line,
                message: "Operands must be numbers.".to_string(),
            });
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

    fn concatenate(&mut self) {
        let b = as_obj(self.pop());
        let a = as_obj(self.pop());
        let str_b = as_string(INTERNER.lock().as_deref().unwrap(), &b);
        let str_a = as_string(INTERNER.lock().as_deref().unwrap(), &a);
        let id = INTERNER
            .lock()
            .as_deref_mut()
            .unwrap()
            .intern(&(str_a.to_string() + str_b));

        self.push(string_val(id));
    }

    fn define_native(&mut self, name: &str, function: fn(Vec<Option<Value>>) -> Value) {
        let name_lookup = INTERNER.lock().as_deref_mut().unwrap().intern(name);
        let name = INTERNER.lock().as_ref().unwrap().lookup(name_lookup);
        let value = native_val(function);
        self.globals.insert(name, value);
    }
}

fn clock_native(args: Vec<Option<Value>>) -> Value {
    let seconds = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    number_val(seconds)
}
