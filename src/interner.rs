use std::collections::HashMap;

// https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
pub struct Interner {
    map: HashMap<&'static str, u32>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Self {
        let cap = cap.next_power_of_two();
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> u32 {
        if let Some(id) = self.map.get(name) {
            return *id;
        }

        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = std::mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let start = self.buf.len();
        self.buf.push_str(name);
        let new_name = &self.buf[start..];
        let interned: &str;

        unsafe {
            interned = &*(new_name as *const str);
        }

        let id = self.map.len() as u32;
        self.map.insert(interned, id);
        self.vec.push(interned);

        id
    }

    pub fn lookup(&self, id: u32) -> &str {
        self.vec[id as usize]
    }
}
