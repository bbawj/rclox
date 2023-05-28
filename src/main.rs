use std::{
    fs::File,
    io::{self, BufReader, Read},
};

use vm::Vm;

mod chunk;
mod compile;
mod debug;
mod interner;
mod object;
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
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        println!("{:?}", input);
        let mut vm = Vm::new(&input)?;
        vm.interpret(false)?;
    }
}

fn run_file(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(())
}
