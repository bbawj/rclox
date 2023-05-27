// pub type Value = f64;

pub type ValueArray = Vec<Value>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValueType {
    ValBool,
    ValNumber,
    ValNil,
}

#[derive(Copy, Clone)]
pub struct Value {
    pub value_type: ValueType,
    pub _as: As,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            match self.value_type {
                ValueType::ValBool => write!(
                    f,
                    "Value {{ value_type: {:?}, _as: {}}}",
                    self.value_type, self._as.boolean
                ),
                ValueType::ValNumber => write!(
                    f,
                    "Value {{ value_type: {:?}, _as: {}}}",
                    self.value_type, self._as.number
                ),
                ValueType::ValNil => write!(f, "Value {{ value_type: {:?}}}", self.value_type),
            }
        }
    }
}

#[derive(Copy, Clone)]
pub union As {
    pub boolean: bool,
    pub number: f64,
}

pub fn bool_val(value: bool) -> Value {
    Value {
        value_type: ValueType::ValBool,
        _as: As { boolean: value },
    }
}

pub fn number_val(value: f64) -> Value {
    Value {
        value_type: ValueType::ValNumber,
        _as: As { number: value },
    }
}

pub fn nil_val() -> Value {
    Value {
        value_type: ValueType::ValNil,
        _as: As { number: 0.0 },
    }
}

pub fn as_bool(value: Value) -> bool {
    unsafe { value._as.boolean }
}

pub fn as_number(value: Value) -> f64 {
    unsafe { value._as.number }
}

pub fn is_number(value: Value) -> bool {
    value.value_type == ValueType::ValNumber
}

pub fn is_bool(value: Value) -> bool {
    value.value_type == ValueType::ValBool
}

pub fn is_nil(value: Value) -> bool {
    value.value_type == ValueType::ValNil
}

pub fn values_equal(a: Value, b: Value) -> bool {
    if a.value_type != b.value_type {
        return false;
    }
    match a.value_type {
        ValueType::ValBool => as_bool(a) == as_bool(b),
        ValueType::ValNumber => as_number(a) == as_number(b),
        ValueType::ValNil => true,
    }
}
