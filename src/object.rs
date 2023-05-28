#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    ObjString(Box<String>),
}

pub fn allocate_string(string: String) -> Obj {
    Obj::ObjString(Box::new(string))
}
