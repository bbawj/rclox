use chunk::Chunk;
use debug::disassemble_chunk;
use vm::Vm;

mod chunk;
mod debug;
mod value;
mod vm;

fn main() {
    let mut chunk = Chunk::new();
    chunk.write_constant(1.2, 123);
    chunk.write_constant(3.4, 123);
    chunk.write_chunk(chunk::OpCode::OpAdd, 123);
    chunk.write_constant(5.6, 123);
    chunk.write_chunk(chunk::OpCode::OpDivide, 123);
    // chunk.write_chunk(chunk::OpCode::OpConstant(constant), 123);
    chunk.write_chunk(chunk::OpCode::OpNegate, 123);
    chunk.write_chunk(chunk::OpCode::OpReturn, 123);
    // disassemble_chunk(chunk.clone(), "test chunk");
    let mut vm = Vm::new(&chunk);
    vm.interpret(false);
}
