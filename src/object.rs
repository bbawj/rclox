use crate::interner::Interner;

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    ObjString(u32),
}

pub fn allocate_string(interner: &mut Interner, string: String) -> Obj {
    let id = interner.intern(&string);

    Obj::ObjString(id)
}
