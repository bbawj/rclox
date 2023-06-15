use crate::{chunk::Chunk, interner::Interner, value::Value};

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    ObjString(u32),
    ObjFunction(Box<ObjFunction>),
    ObjNative(Box<fn(args: Vec<Option<Value>>) -> Value>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name_lookup: u32,
}

pub fn allocate_string(interner: &mut Interner, string: String) -> Box<Obj> {
    let id = interner.intern(&string);

    Box::new(Obj::ObjString(id))
}

pub fn allocate_function(interner: &mut Interner, name: &str) -> Box<ObjFunction> {
    let id = interner.intern(name);
    let function = ObjFunction {
        arity: 0,
        chunk: Chunk::new(),
        name_lookup: id,
    };
    Box::new(function)
}

pub fn allocate_native(function: fn(args: Vec<Option<Value>>) -> Value) -> Box<Obj> {
    Box::new(Obj::ObjNative(Box::new(function)))
}
