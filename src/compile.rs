use crate::scanner::Scanner;

pub fn compile(source: &str) {
    let scanner = Scanner::new(source.to_string());
    let line = -1;
    loop {
        let token = scan_token();
        if token.line != line {
            print!("{4} ", token.line);
            line = token.line;
        } else {
            print!("  | ");
        }
    }
}
