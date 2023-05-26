use std::{
    fs::File,
    io::{self, BufReader, Read},
};

use chunk::Chunk;
use debug::disassemble_chunk;
use vm::Vm;

mod chunk;
mod compile;
mod debug;
mod scanner;
mod value;
mod vm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        repl()?;
    } else {
        run_file(&args[1])?;
    }
    Ok(())
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    loop {
        let mut input = [0; 1024];
        io::stdin().read_exact(&mut input)?;
        interpret(&String::from_utf8(input.to_vec()).unwrap());
    }
}

fn run_file(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(())
}

fn interpret(source: &str) {
    compile::compile(source);
}
