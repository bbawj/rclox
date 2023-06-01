use std::ops::Index;

use crate::{
    chunk::{Chunk, OpCode},
    // debug::disassemble_chunk,
    interner::Interner,
    object::allocate_string,
    scanner::{self, Scanner, Token, TokenType},
    value::number_val,
    vm::{ErrorData, InterpretError},
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
    pub strings: Interner,
    locals: [Option<Local>; 256],
    local_count: u8,
    scope_depth: usize,
}

#[derive(Debug)]
struct Local {
    name: Token,
    depth: usize,
    is_uninitialized: bool,
}

type RuleArray = [ParseRule; 39];

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
        prefix: Some(ParseFn::Variable),
        infix: None,
        precedence: Precedence::None,
    },
    //20
    ParseRule {
        prefix: Some(ParseFn::String),
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
        infix: Some(ParseFn::And),
        precedence: Precedence::And,
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
        infix: Some(ParseFn::Or),
        precedence: Precedence::Or,
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
    pub fn new(source: &str, interner: Interner) -> Self {
        let source = source.to_string();
        let scanner = Scanner::new(source);
        const INIT_LOCAL: Option<Local> = None;
        Self {
            scanner,
            parser: Parser {
                current: None,
                previous: None,
                had_error: false,
            },
            compiling_chunk: Some(Chunk::new()),
            strings: interner,
            locals: [INIT_LOCAL; 256],
            local_count: 0,
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self) -> Result<Chunk, InterpretError> {
        self.advance()?;

        while !self.matching(TokenType::Eof)? {
            self.declaration()?;
        }

        self.consume(TokenType::Eof, "Expect end of expression.")?;
        self.end_compiler();
        Ok(self.current_chunk().clone())
    }

    fn advance(&mut self) -> Result<(), InterpretError> {
        // println!("{:?}", self.parser.current);
        // println!("{:?}", self.parser.previous);
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

    fn matching(&mut self, token_type: TokenType) -> Result<bool, InterpretError> {
        if !self.check(token_type) {
            return Ok(false);
        }
        self.advance()?;
        Ok(true)
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.parser.current.as_ref().unwrap().token_type == token_type
    }

    fn end_compiler(&mut self) {
        self.emit_byte(OpCode::OpReturn);
        // if !self.parser.had_error {
        //     disassemble_chunk(self.current_chunk(), "code");
        // }
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

    fn emit_jump(&mut self, code: OpCode) -> usize {
        let mut line = 1;
        if let Some(prev_token) = self.parser.previous.as_ref() {
            line = prev_token.line
        }
        self.current_chunk().write_chunk(code, line);
        self.current_chunk().counter - 1
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().counter - loop_start;
        self.emit_byte(OpCode::OpLoop(offset.try_into().unwrap()))
    }

    fn patch_jump(&mut self, loc: usize) {
        let cur = self.current_chunk().counter;
        match self.current_chunk().code[loc] {
            OpCode::OpJump(ref mut offset) | OpCode::OpJumpIfFalse(ref mut offset) => {
                *offset = (cur - loc) as u16
            }
            _ => unreachable!(),
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.compiling_chunk.as_mut().unwrap()
    }

    fn declaration(&mut self) -> Result<(), InterpretError> {
        let mut steps = || -> Result<(), InterpretError> {
            if self.matching(TokenType::Var)? {
                self.var_declaration()
            } else {
                self.statement()
            }
        };

        match steps() {
            Ok(_) => Ok(()),
            Err(_) => self.synchronize(),
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while self.local_count > 0
            && self.locals[self.local_count as usize - 1]
                .as_ref()
                .unwrap()
                .depth
                > self.scope_depth
        {
            self.emit_byte(OpCode::OpPop);
            self.local_count -= 1;
        }
    }

    fn var_declaration(&mut self) -> Result<(), InterpretError> {
        let global = self.parse_variable("Expect variable name.")?;
        if self.matching(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::OpNil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        self.define_variable(global);
        Ok(())
    }

    fn parse_variable(&mut self, error_message: &str) -> Result<u32, InterpretError> {
        self.consume(TokenType::Identifier, error_message)?;
        self.declare_variable()?;
        if self.scope_depth > 0 {
            return Ok(0);
        }
        let prev_token = self.parser.previous.as_ref().unwrap().clone();
        return Ok(self.identifier_constant(&prev_token));
    }

    fn identifier_constant(&mut self, token: &Token) -> u32 {
        let value = crate::value::Value::ValObj(Box::new(allocate_string(
            &mut self.strings,
            token.lexeme.clone(),
        )));
        let id = self.current_chunk().add_constant(value);
        return id as u32;
    }

    fn declare_variable(&mut self) -> Result<(), InterpretError> {
        if self.scope_depth == 0 {
            return Ok(());
        }

        let token = self.parser.previous.as_ref().unwrap().clone();

        for local in self.locals[..self.local_count as usize].iter().rev() {
            if local.is_none() || local.as_ref().unwrap().depth < self.scope_depth {
                break;
            }

            if token.lexeme == local.as_ref().unwrap().name.lexeme {
                return Err(InterpretError::InterpretCompileError(ErrorData {
                    line: token.line,
                    message: "Already a variable with this name in this scope.".to_string(),
                }));
            }
        }
        self.add_local(token)
    }

    fn add_local(&mut self, name: Token) -> Result<(), InterpretError> {
        if self.local_count == 255 {
            return Err(InterpretError::InterpretCompileError(ErrorData {
                line: name.line,
                message: "Too many local variables in function.".to_string(),
            }));
        }
        let local = Local {
            name,
            depth: self.scope_depth,
            is_uninitialized: true,
        };
        self.locals[self.local_count as usize] = Some(local);
        self.local_count += 1;
        Ok(())
    }

    fn define_variable(&mut self, id: u32) {
        if self.scope_depth > 0 {
            self.locals[self.local_count as usize - 1]
                .as_mut()
                .unwrap()
                .is_uninitialized = false;
            return;
        }
        self.emit_byte(OpCode::OpDefineGlobal(id));
    }

    fn statement(&mut self) -> Result<(), InterpretError> {
        if self.matching(TokenType::Print)? {
            self.print_statement()?;
        } else if self.matching(TokenType::LeftBrace)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else if self.matching(TokenType::If)? {
            self.if_statement()?;
        } else if self.matching(TokenType::While)? {
            self.while_statement()?;
        } else if self.matching(TokenType::For)? {
            self.for_statement()?;
        } else {
            self.expression_statement()?;
        }
        Ok(())
    }

    fn block(&mut self) -> Result<(), InterpretError> {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration()?;
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")
    }

    fn print_statement(&mut self) -> Result<(), InterpretError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        self.emit_byte(OpCode::OpPrint);
        Ok(())
    }

    fn if_statement(&mut self) -> Result<(), InterpretError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let then_jump = self.emit_jump(OpCode::OpJumpIfFalse(u16::MAX));
        self.emit_byte(OpCode::OpPop);
        self.statement()?;
        let else_jump = self.emit_jump(OpCode::OpJump(u16::MAX));
        self.patch_jump(then_jump);
        self.emit_byte(OpCode::OpPop);

        if self.matching(TokenType::Else)? {
            self.statement()?;
        }
        self.patch_jump(else_jump);
        Ok(())
    }

    fn while_statement(&mut self) -> Result<(), InterpretError> {
        let loop_start = self.current_chunk().counter;

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let exit_jump = self.emit_jump(OpCode::OpJumpIfFalse(u16::MAX));
        self.emit_byte(OpCode::OpPop);
        self.statement()?;
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::OpPop);

        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), InterpretError> {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
        if self.matching(TokenType::Semicolon)? {
        } else if self.matching(TokenType::Var)? {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.current_chunk().counter;
        let mut exit_jump: Option<usize> = None;
        if !self.matching(TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;
            exit_jump = Some(self.emit_jump(OpCode::OpJumpIfFalse(u16::MAX)));
            self.emit_byte(OpCode::OpPop);
        }

        if !self.matching(TokenType::RightParen)? {
            let body_jump = self.emit_jump(OpCode::OpJump(u16::MAX));
            let increment_start = self.current_chunk().counter;
            self.expression()?;
            self.emit_byte(OpCode::OpPop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement()?;
        self.emit_loop(loop_start);

        if let Some(loc) = exit_jump {
            self.patch_jump(loc);
            self.emit_byte(OpCode::OpPop);
        }
        self.end_scope();

        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), InterpretError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        println!("yeet");
        self.emit_byte(OpCode::OpPop);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), InterpretError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self, can_assign: bool) -> Result<(), InterpretError> {
        let prev_token = self.parser.previous.as_ref().unwrap();
        let prev_token_line = self.parser.previous.as_ref().unwrap().line;
        let literal = prev_token.literal.as_ref().unwrap().clone();

        if let crate::scanner::Literal::Number(number) = literal {
            self.current_chunk()
                .write_constant(number_val(number.clone()), prev_token_line)
        }

        Ok(())
    }

    fn string(&mut self, can_assign: bool) -> Result<(), InterpretError> {
        let prev_token = self.parser.previous.as_mut().unwrap();
        let line = prev_token.line;
        if let Some(scanner::Literal::String(string)) = prev_token.literal.take() {
            let value = Box::new(allocate_string(&mut self.strings, string));
            self.current_chunk()
                .write_constant(crate::value::Value::ValObj(value), line)
        }
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), InterpretError> {
        let token = self.parser.previous.as_ref().unwrap().clone();
        self.named_variable(&token, can_assign)
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) -> Result<(), InterpretError> {
        let arg = self.resolve_local(&name)?;
        let get_op: OpCode;
        let set_op: OpCode;
        match arg {
            Some(idx) => {
                get_op = OpCode::OpGetLocal(idx);
                set_op = OpCode::OpSetLocal(idx);
            }
            None => {
                let arg = self.identifier_constant(name);
                get_op = OpCode::OpGetGlobal(arg);
                set_op = OpCode::OpSetGlobal(arg);
            }
        }
        if can_assign && self.matching(TokenType::Equal)? {
            self.expression()?;
            self.emit_byte(set_op);
        } else {
            self.emit_byte(get_op);
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: &Token) -> Result<Option<u8>, InterpretError> {
        for (i, maybe_local) in self.locals[..self.local_count as usize]
            .iter()
            .enumerate()
            .rev()
        {
            if let Some(local) = maybe_local {
                if local.name.lexeme == name.lexeme {
                    if local.is_uninitialized {
                        return Err(InterpretError::InterpretCompileError(ErrorData {
                            line: local.name.line,
                            message: "Can't read local variable in its own initializer."
                                .to_string(),
                        }));
                    }
                    return Ok(Some(i.try_into().unwrap()));
                }
            } else {
                break;
            }
        }
        return Ok(None);
    }

    fn literal(&mut self, can_assign: bool) -> Result<(), InterpretError> {
        let prev_token = self.parser.previous.as_ref().unwrap();
        match prev_token.token_type {
            TokenType::False => self.emit_byte(OpCode::OpFalse),
            TokenType::Nil => self.emit_byte(OpCode::OpNil),
            TokenType::True => self.emit_byte(OpCode::OpTrue),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn grouping(&mut self, can_assign: bool) -> Result<(), InterpretError> {
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, can_assign: bool) -> Result<(), InterpretError> {
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

    fn binary(&mut self, can_assign: bool) -> Result<(), InterpretError> {
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

    fn and(&mut self) -> Result<(), InterpretError> {
        let end_jump = self.emit_jump(OpCode::OpJumpIfFalse(u16::MAX));
        self.emit_byte(OpCode::OpPop);
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn or(&mut self) -> Result<(), InterpretError> {
        let else_jump = self.emit_jump(OpCode::OpJumpIfFalse(u16::MAX));
        let end_jump = self.emit_jump(OpCode::OpJump(u16::MAX));
        self.patch_jump(else_jump);
        self.emit_byte(OpCode::OpPop);
        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), InterpretError> {
        self.advance()?;
        let prev = self.parser.previous.as_ref().unwrap();
        let binding = prev.token_type.clone();
        let prefix_rule = &get_rule(&binding).prefix;

        let can_assign = precedence <= Precedence::Assignment;
        match prefix_rule {
            Some(f) => self.call_parse_function(f, can_assign)?,
            None => self.print_error(&prev, "Expect expression.")?,
        }

        while precedence <= get_rule(&self.parser.current.as_ref().unwrap().token_type).precedence {
            self.advance()?;
            let infix_rule = &get_rule(&self.parser.previous.as_ref().unwrap().token_type).infix;
            if let Some(f) = infix_rule {
                self.call_parse_function(&f.clone(), can_assign)?;
            }
        }

        if can_assign && self.matching(TokenType::Equal)? {
            return Err(InterpretError::InterpretCompileError(ErrorData {
                line: self.parser.current.as_ref().unwrap().line,
                message: "Invalid assignment target.".to_string(),
            }));
        }
        Ok(())
    }

    fn call_parse_function(
        &mut self,
        parse_fn: &ParseFn,
        can_assign: bool,
    ) -> Result<(), InterpretError> {
        match parse_fn {
            ParseFn::Grouping => self.grouping(can_assign),
            ParseFn::Unary => self.unary(can_assign),
            ParseFn::Binary => self.binary(can_assign),
            ParseFn::Number => self.number(can_assign),
            ParseFn::Literal => self.literal(can_assign),
            ParseFn::String => self.string(can_assign),
            ParseFn::Variable => self.variable(can_assign),
            ParseFn::And => self.and(),
            ParseFn::Or => self.or(),
        }
    }

    fn synchronize(&mut self) -> Result<(), InterpretError> {
        while self.parser.current.as_ref().unwrap().token_type != TokenType::Eof {
            if self.parser.previous.as_ref().unwrap().token_type == TokenType::Semicolon {
                return Ok(());
            }

            match self.parser.current.as_ref().unwrap().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return Ok(()),
                _ => self.advance()?,
            }
        }
        Ok(())
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
    String,
    Variable,
    And,
    Or,
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
