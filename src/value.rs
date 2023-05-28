// pub type Value = f64;

use crate::{interner::Interner, object::Obj};

pub type ValueArray = Vec<Value>;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    ValBool(bool),
    ValNumber(f64),
    ValNil,
    ValObj(Box<Obj>),
}

pub fn bool_val(value: bool) -> Value {
    Value::ValBool(value)
}

pub fn number_val(value: f64) -> Value {
    Value::ValNumber(value)
}

pub fn nil_val() -> Value {
    Value::ValNil
}

pub fn obj_val(value: Box<Obj>) -> Value {
    Value::ValObj(value)
}

pub fn string_val(value: u32) -> Value {
    Value::ValObj(Box::new(Obj::ObjString(value)))
}

pub fn as_bool(value: Value) -> bool {
    if let Value::ValBool(v) = value {
        v
    } else {
        panic!()
    }
}

pub fn as_number(value: Value) -> f64 {
    if let Value::ValNumber(v) = value {
        v
    } else {
        panic!()
    }
}

pub fn as_obj(value: Value) -> Box<Obj> {
    if let Value::ValObj(v) = value {
        v
    } else {
        panic!()
    }
}

pub fn as_string(interner: &Interner, value: Value) -> &str {
    if let Value::ValObj(a) = value {
        match *a {
            Obj::ObjString(id) => interner.lookup(id),
        }
    } else {
        panic!()
    }
}

pub fn is_number(value: &Value) -> bool {
    if let Value::ValNumber(_) = value {
        true
    } else {
        false
    }
}

pub fn is_bool(value: &Value) -> bool {
    if let Value::ValBool(_) = value {
        true
    } else {
        false
    }
}

pub fn is_nil(value: &Value) -> bool {
    if let Value::ValNil = value {
        true
    } else {
        false
    }
}

pub fn is_obj(value: &Value) -> bool {
    if let Value::ValObj(_) = value {
        true
    } else {
        false
    }
}

pub fn is_string(value: &Value) -> bool {
    if let Value::ValObj(a) = value {
        match a.as_ref() {
            Obj::ObjString(_) => true,
        }
    } else {
        false
    }
}

pub fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::ValBool(a), Value::ValBool(b)) => a == b,
        (Value::ValNumber(a), Value::ValNumber(b)) => a == b,
        (Value::ValNil, Value::ValNil) => true,
        (Value::ValObj(a), Value::ValObj(b)) => match (a.as_ref(), b.as_ref()) {
            (Obj::ObjString(a), Obj::ObjString(b)) => a == b,
        },
        _ => false,
    }
}
