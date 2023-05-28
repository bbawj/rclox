#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    ObjString(String),
}

pub fn allocate_string(string: String) -> Obj {
    Obj::ObjString(string)
}
