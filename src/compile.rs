use std::ops::Index;

use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    scanner::{Scanner, Token, TokenType},
    value::number_val,
    vm::InterpretError,
};

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    None = 1,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn higher(&self) -> Self {
        match self {
            Precedence::None => Self::Assignment,
            Precedence::Assignment => Self::Or,
            Precedence::Or => Self::And,
            Precedence::And => Self::Equality,
            Precedence::Equality => Self::Comparison,
            Precedence::Comparison => Self::Term,
            Precedence::Term => Self::Factor,
            Precedence::Factor => Self::Unary,
            Precedence::Unary => Self::Call,
            Precedence::Call => Self::Primary,
            Precedence::Primary => Self::Primary,
        }
    }
}

struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
}

pub struct Compiler {
    scanner: Scanner,
    parser: Parser,
    compiling_chunk: Option<Chunk>,
}

pub type RuleArray = [ParseRule; 39];
static RULES: RuleArray = [
    //0
    ParseRule {
        prefix: Some(ParseFn::Grouping),
        infix: None,
        precedence: Precedence::None,
    },
    //1
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //2
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //3
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //4
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //5
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //6
    ParseRule {
        prefix: Some(ParseFn::Unary),
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Term,
    },
    //7
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Term,
    },
    //8
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //9
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Factor,
    },
    //10
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Factor,
    },
    //11
    ParseRule {
        prefix: Some(ParseFn::Unary),
        infix: None,
        precedence: Precedence::None,
    },
    //12
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Equality,
    },
    //13
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //14
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Equality,
    },
    //15
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Comparison,
    },
    //16
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Comparison,
    },
    //17
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Comparison,
    },
    //18
    ParseRule {
        prefix: None,
        infix: Some(ParseFn::Binary),
        precedence: Precedence::Comparison,
    },
    //19
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //20
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //21
    ParseRule {
        prefix: Some(ParseFn::Number),
        infix: None,
        precedence: Precedence::None,
    },
    //22
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //23
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //24
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //25
    ParseRule {
        prefix: Some(ParseFn::Literal),
        infix: None,
        precedence: Precedence::None,
    },
    //26
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //27
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //28
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //29
    ParseRule {
        prefix: Some(ParseFn::Literal),
        infix: None,
        precedence: Precedence::None,
    },
    //30
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //31
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //32
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //33
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //34
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //35
    ParseRule {
        prefix: Some(ParseFn::Literal),
        infix: None,
        precedence: Precedence::None,
    },
    //36
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //37
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
    //38
    ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    },
];

impl Compiler {
    pub fn new(source: &str) -> Self {
        let source = source.to_string();
        let scanner = Scanner::new(source);
        Self {
            scanner,
            parser: Parser {
                current: None,
                previous: None,
                had_error: false,
            },
            compiling_chunk: Some(Chunk::new()),
        }
    }

    pub fn compile(&mut self) -> Result<Chunk, InterpretError> {
        self.advance()?;
        self.expression()?;
        self.consume(TokenType::Eof, "Expect end of expression.")?;
        self.end_compiler();
        Ok(self.current_chunk().clone())
    }

    fn advance(&mut self) -> Result<(), InterpretError> {
        self.parser.previous = self.parser.current.clone();
        loop {
            self.parser.current = Some(self.scanner.scan_token());
            match self.parser.current.as_ref().unwrap().token_type {
                TokenType::Error => {
                    let current_token = self.parser.current.as_ref().unwrap();
                    self.print_error(&current_token, &current_token.lexeme)?;
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn print_error(&self, token: &Token, error_message: &str) -> Result<(), InterpretError> {
        print!("[line {}] Error", token.line);
        match token.token_type {
            TokenType::Error => (),
            TokenType::Eof => print!(" at end"),
            _ => print!(" at '{}'", token.lexeme),
        };
        print!(": {}", error_message);
        Err(InterpretError::InterpretParserError)
    }

    fn consume(
        &mut self,
        token_type: TokenType,
        error_message: &str,
    ) -> Result<(), InterpretError> {
        let current_token = self.parser.current.as_ref().unwrap();
        if current_token.token_type == token_type {
            return self.advance();
        }

        self.print_error(&current_token, error_message)
    }

    fn end_compiler(&mut self) {
        self.emit_byte(OpCode::OpReturn);
        if !self.parser.had_error {
            disassemble_chunk(self.current_chunk(), "code");
        }
    }

    fn emit_byte(&mut self, code: OpCode) {
        let mut line = 1;
        if let Some(prev_token) = self.parser.previous.as_ref() {
            line = prev_token.line
        }
        self.current_chunk().write_chunk(code, line)
    }

    fn emit_bytes(&mut self, code1: OpCode, code2: OpCode) {
        let mut line = 1;
        if let Some(prev_token) = self.parser.previous.as_ref() {
            line = prev_token.line
        }
        self.current_chunk().write_chunk(code1, line);
        self.current_chunk().write_chunk(code2, line)
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.compiling_chunk.as_mut().unwrap()
    }

    fn expression(&mut self) -> Result<(), InterpretError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self) -> Result<(), InterpretError> {
        let prev_token = self.parser.previous.as_ref().unwrap();
        let prev_token_line = self.parser.previous.as_ref().unwrap().line;
        let literal = prev_token.literal.as_ref().unwrap().clone();

        if let crate::scanner::Literal::Number(number) = literal {
            self.current_chunk()
                .write_constant(number_val(number.clone()), prev_token_line)
        }

        Ok(())
    }

    fn literal(&mut self) -> Result<(), InterpretError> {
        let prev_token = self.parser.previous.as_ref().unwrap();
        match prev_token.token_type {
            TokenType::False => self.emit_byte(OpCode::OpFalse),
            TokenType::Nil => self.emit_byte(OpCode::OpNil),
            TokenType::True => self.emit_byte(OpCode::OpTrue),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn grouping(&mut self) -> Result<(), InterpretError> {
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self) -> Result<(), InterpretError> {
        let operator = self.parser.previous.as_ref().unwrap().token_type.clone();
        // compile the operand
        self.parse_precedence(Precedence::Unary)?;

        match operator {
            TokenType::Minus => self.emit_byte(OpCode::OpNegate),
            TokenType::Bang => self.emit_byte(OpCode::OpNot),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<(), InterpretError> {
        let operator = self.parser.previous.as_ref().unwrap().token_type.clone();
        let rule = get_rule(&operator);
        self.parse_precedence(rule.precedence.higher())?;

        match operator {
            TokenType::Minus => self.emit_byte(OpCode::OpSubtract),
            TokenType::Plus => self.emit_byte(OpCode::OpAdd),
            TokenType::Slash => self.emit_byte(OpCode::OpDivide),
            TokenType::Star => self.emit_byte(OpCode::OpMultiply),
            TokenType::BangEqual => self.emit_bytes(OpCode::OpEqual, OpCode::OpNot),
            TokenType::EqualEqual => self.emit_byte(OpCode::OpEqual),
            TokenType::Greater => self.emit_byte(OpCode::OpGreater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::OpLess, OpCode::OpNot),
            TokenType::Less => self.emit_byte(OpCode::OpLess),
            TokenType::LessEqual => self.emit_bytes(OpCode::OpGreater, OpCode::OpNot),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), InterpretError> {
        self.advance()?;
        let prev = self.parser.previous.as_ref().unwrap();
        let binding = prev.token_type.clone();
        let prefix_rule = &get_rule(&binding).prefix;

        match prefix_rule {
            Some(f) => self.call_parse_function(f)?,
            None => self.print_error(&prev, "Expect expression.")?,
        }

        while precedence
            <= get_rule(&self.parser.current.as_ref().unwrap().token_type.clone()).precedence
        {
            self.advance()?;
            let infix_rule = &get_rule(&self.parser.previous.as_ref().unwrap().token_type).infix;
            if let Some(f) = infix_rule {
                self.call_parse_function(&f.clone())?;
            }
        }
        Ok(())
    }

    fn call_parse_function(&mut self, parse_fn: &ParseFn) -> Result<(), InterpretError> {
        match parse_fn {
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
            ParseFn::Number => self.number(),
            ParseFn::Literal => self.literal(),
        }
    }
}

#[derive(Debug)]
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

#[derive(Debug, Clone)]
enum ParseFn {
    Grouping,
    Unary,
    Binary,
    Number,
    Literal,
}

fn get_rule(operator_type: &TokenType) -> &ParseRule {
    &RULES[operator_type]
}

impl Index<&TokenType> for RuleArray {
    type Output = ParseRule;

    fn index(&self, index: &TokenType) -> &Self::Output {
        match index {
            TokenType::Error => unreachable!(),
            TokenType::LeftParen => &self[0],
            TokenType::RightParen => &self[1],
            TokenType::LeftBrace => &self[2],
            TokenType::RightBrace => &self[3],
            TokenType::Comma => &self[4],
            TokenType::Dot => &self[5],
            TokenType::Minus => &self[6],
            TokenType::Plus => &self[7],
            TokenType::Semicolon => &self[8],
            TokenType::Slash => &self[9],
            TokenType::Star => &self[10],
            TokenType::Bang => &self[11],
            TokenType::BangEqual => &self[12],
            TokenType::Equal => &self[13],
            TokenType::EqualEqual => &self[14],
            TokenType::Greater => &self[15],
            TokenType::GreaterEqual => &self[16],
            TokenType::Less => &self[17],
            TokenType::LessEqual => &self[18],
            TokenType::Identifier => &self[19],
            TokenType::String => &self[20],
            TokenType::Number => &self[21],
            TokenType::And => &self[22],
            TokenType::Class => &self[23],
            TokenType::Else => &self[24],
            TokenType::False => &self[25],
            TokenType::Fun => &self[26],
            TokenType::For => &self[27],
            TokenType::If => &self[28],
            TokenType::Nil => &self[29],
            TokenType::Or => &self[30],
            TokenType::Print => &self[31],
            TokenType::Return => &self[32],
            TokenType::Super => &self[33],
            TokenType::This => &self[34],
            TokenType::True => &self[35],
            TokenType::Var => &self[36],
            TokenType::While => &self[37],
            TokenType::Eof => &self[38],
        }
    }
}
