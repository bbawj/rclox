use std::{rc::Rc, cell::RefCell};

use crate::{chunk::Chunk, interner::Interner, value::Value, scanner::{Token, TokenType}};

#[derive(Debug, Clone)]
pub enum Obj {
    ObjString(u32),
    ObjFunction(Box<ObjFunction>),
    ObjNative(Box<fn(args: Vec<Option<Value>>) -> Value>),
    ObjClosure(ObjClosure),
    ObjUpvalue(ObjUpvalue)
}

#[derive(Debug, Clone)]
pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name_lookup: u32,
    pub locals: [Option<Local>; 256],
    pub local_count: u8,
    pub upvalues: [Option<Upvalue>; 256],
    pub upvalue_count: u8,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub name: Token,
    pub depth: usize,
    pub is_uninitialized: bool,
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub index: u8,
    pub is_local: bool
}

#[derive(Debug, Clone)]
pub struct ObjClosure {
    pub function: Rc<ObjFunction>,
    pub upvalues: Vec<Option<ObjUpvalue>>,
    pub upvalue_count: u8,
}

#[derive(Debug, Clone)]
pub struct ObjUpvalue {
    pub location: Rc<RefCell<Value>>
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
    ObjClosure { function: Rc::new(*function), upvalues: vec![None; upvalue_count.into()], upvalue_count }
}

pub fn allocate_upvalue(value: Rc<RefCell<Value>>) -> ObjUpvalue {
    ObjUpvalue {
        location: value,
    }
}
