use crate::value::{Value, ValueArray};

#[derive(Debug, Clone)]
pub enum OpCode {
    OpConstant(u8),
    OpConstantLong(u32),
    OpDefineGlobal(u32),
    OpGetGlobal(u32),
    OpSetGlobal(u32),
    OpNil,
    OpTrue,
    OpFalse,
    OpEqual,
    OpGreater,
    OpLess,
    OpNot,
    OpNegate,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpPrint,
    OpPop,
    OpReturn,
}

#[derive(Clone)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: ValueArray,
    pub rle: Vec<LineStart>,
    pub counter: usize,
}

#[derive(Debug, Clone)]
pub struct LineStart {
    line: usize,
    offset: usize,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: ValueArray::new(),
            rle: Vec::new(),
            counter: 0,
        }
    }

    pub fn write_chunk(&mut self, code: OpCode, line: usize) {
        self.code.push(code);
        if self.rle.len() == 0 || self.rle.last().unwrap().line != line {
            self.rle.push(LineStart {
                line: line.try_into().unwrap(),
                offset: self.counter,
            });
            self.counter += 1;
        } else {
            self.counter += 1;
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        return self.constants.len() - 1;
    }

    pub fn write_constant(&mut self, value: Value, line: usize) {
        let idx = self.add_constant(value);
        if idx < 256 {
            self.write_chunk(OpCode::OpConstant(idx.try_into().unwrap()), line);
        } else {
            self.write_chunk(OpCode::OpConstantLong(idx.try_into().unwrap()), line);
        }
    }

    pub fn get_constant(&self, idx: usize) -> Option<&Value> {
        self.constants.get(idx)
    }

    pub fn get_line(&self, offset: usize) -> usize {
        let mut left = 0;
        let mut right = self.rle.len();
        loop {
            let mid = (left + right) / 2;
            if self.rle.get(mid).unwrap().offset < offset
                && self.rle.get(mid + 1).is_some()
                && self.rle.get(mid + 1).unwrap().offset < offset
            {
                left = mid + 1;
            } else if self.rle.get(mid).unwrap().offset > offset {
                right = mid - 1;
            } else {
                return self.rle.get(mid).unwrap().line;
            }
        }
    }
}
