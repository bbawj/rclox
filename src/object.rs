use std::{rc::Rc};

use crate::{chunk::Chunk, interner::Interner, value::Value, scanner::{Token, TokenType}};

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    ObjString(u32),
    ObjFunction(Box<ObjFunction>),
    ObjNative(Box<fn(args: Vec<Option<Value>>) -> Value>),
    ObjClosure(ObjClosure),
    ObjUpvalue(ObjUpvalue)
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name_lookup: u32,
    pub locals: [Option<Local>; 256],
    pub local_count: u8,
    pub upvalues: [Option<Upvalue>; 256],
    pub upvalue_count: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub name: Token,
    pub depth: usize,
    pub is_uninitialized: bool,
    pub is_captured: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Upvalue {
    pub index: u8,
    pub is_local: bool
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjClosure {
    pub function: Rc<ObjFunction>,
    pub upvalues: Vec<*mut ObjUpvalue>,
    pub upvalue_count: u8,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjUpvalue {
    pub value: *mut Value,
    pub closed: Box<Value>,
    pub location: usize,
    pub next: *mut ObjUpvalue
}

pub fn allocate_string(interner: &mut Interner, string: String) -> Box<Obj> {
    let id = interner.intern(&string);

    Box::new(Obj::ObjString(id))
}

pub fn allocate_function(interner: &mut Interner, name: &str) -> Box<ObjFunction> {
    let id = interner.intern(name);
    const INIT_LOCAL: Option<Local> = None;
    const INIT_UPVALUE: Option<Upvalue> = None;
    let mut locals = [INIT_LOCAL; 256];
    locals[0] = Some(Local {
        name: Token {
            token_type: TokenType::Nil,
            lexeme: String::new(),
            line: 0,
            literal: None,
        },
        depth: 0,
        is_uninitialized: false,
        is_captured: false,
    });
    let function = ObjFunction {
        arity: 0,
        chunk: Chunk::new(),
        locals,
        local_count: 1,
        upvalues: [INIT_UPVALUE; 256],
        upvalue_count: 0,
        name_lookup: id,
    };
    Box::new(function)
}

pub fn allocate_native(function: fn(args: Vec<Option<Value>>) -> Value) -> Box<Obj> {
    Box::new(Obj::ObjNative(Box::new(function)))
}

pub fn allocate_closure(function: Box<ObjFunction>) -> ObjClosure {
    let upvalue_count = function.upvalue_count;
    ObjClosure { function: Rc::new(*function), upvalues: vec![std::ptr::null_mut();upvalue_count.into()], upvalue_count }
}

pub fn allocate_upvalue(location: usize, value: Value) -> Box<ObjUpvalue> {
    Box::new(ObjUpvalue {
        value: Box::into_raw(Box::new(value)),
        closed: Box::new(Value::ValNil),
        location,
        next: std::ptr::null_mut()
    })
}
