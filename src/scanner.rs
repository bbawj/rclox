use crate::vm::{InterpretError, TokenError};

pub enum Literal {
    Number(f64),
    String(String),
}

pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub literal: Option<Literal>,
}

pub enum TokenType {
    Error,
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Question,
    Colon,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

pub struct Scanner {
    source: String,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self { source, line: 1 }
    }

    pub fn scan_token(&mut self) -> Result<Token, InterpretError> {
        let mut iter = self.source.chars().peekable();
        loop {
            let mut text = String::new();
            let c = iter.next();

            if c.is_none() {
                return self.create_token(TokenType::Eof, "".to_string());
            }

            if let Some(character) = c {
                text.push(character);

                match character {
                    '(' => self.create_token(TokenType::LeftParen, text),
                    ')' => self.create_token(TokenType::RightParen, text),
                    '{' => self.create_token(TokenType::LeftBrace, text),
                    '}' => self.create_token(TokenType::RightBrace, text),
                    ',' => self.create_token(TokenType::Comma, text),
                    '.' => self.create_token(TokenType::Dot, text),
                    '-' => self.create_token(TokenType::Minus, text),
                    '+' => self.create_token(TokenType::Plus, text),
                    ';' => self.create_token(TokenType::Semicolon, text),
                    '*' => self.create_token(TokenType::Star, text),
                    '?' => self.create_token(TokenType::Question, text),
                    ':' => self.create_token(TokenType::Colon, text),
                    '!' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            return self.create_token(TokenType::BangEqual, text);
                        } else {
                            return self.create_token(TokenType::Bang, text);
                        }
                    }
                    '=' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            return self.create_token(TokenType::EqualEqual, text);
                        } else {
                            return self.create_token(TokenType::Equal, text);
                        }
                    }
                    '>' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            return self.create_token(TokenType::GreaterEqual, text);
                        } else {
                            return self.create_token(TokenType::Greater, text);
                        }
                    }
                    '<' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            return self.create_token(TokenType::LessEqual, text);
                        } else {
                            return self.create_token(TokenType::Less, text);
                        }
                    }
                    '/' => {
                        let next = iter.next_if_eq(&'/');
                        if next.is_some() {
                            while iter.next_if(|&x| x != '\n').is_some() {}
                            //discard the newline
                            iter.next();
                        }
                        return self.create_token(TokenType::Slash, text);
                    }
                    ' ' | '\r' | '\t' => continue,
                    '\n' => {
                        self.line = &self.line + 1;
                        continue;
                    }
                    '"' => {
                        loop {
                            // end of file
                            if iter.peek().is_none() {
                                return self.create_token(
                                    TokenType::Error,
                                    "Unterminated string.".to_string(),
                                );
                            }

                            let next = iter.next_if(|&x| x != '"');

                            match next {
                                Some(ch) => text.push(ch),
                                None => {
                                    // discard the '"'
                                    iter.next();
                                    break;
                                }
                            }
                        }
                        return self.create_token_with_literal(
                            TokenType::String,
                            text.clone(),
                            Literal::String(text),
                        );
                    }
                    _ => {
                        if character.is_digit(10) {
                            while let Some(ch) = iter.next_if(|x| x.is_digit(10)) {
                                text.push(ch);
                            }

                            // fractional
                            let peek = iter.peek();
                            match peek {
                                Some(ch) => {
                                    if ch == &'.' {
                                        // ensure that after '.' is digits
                                        let mut iter_clone = iter.clone();
                                        // discard '.'
                                        iter_clone.next();
                                        let next = iter_clone.peek();
                                        match next {
                                            Some(x) => {
                                                if x.is_digit(10) {
                                                    if let Some(dot) = iter.next() {
                                                        text.push(dot);
                                                    }

                                                    while let Some(ch) =
                                                        iter.next_if(|x| x.is_digit(10))
                                                    {
                                                        text.push(ch);
                                                    }
                                                }
                                            }
                                            None => (),
                                        }
                                    }
                                }
                                None => (),
                            }
                            return self.create_token_with_literal(
                                TokenType::Number,
                                text.clone(),
                                Literal::Number(
                                    text.parse::<f64>().expect("Failed parsing string to f64."),
                                ),
                            );
                        } else if character.is_alphanumeric() {
                            while let Some(ch) = iter.next_if(|x| x.is_alphanumeric()) {
                                text.push(ch);
                            }

                            match text.as_str() {
                                "and" => {
                                    return self.create_token_with_literal(
                                        TokenType::And,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "class" => {
                                    return self.create_token_with_literal(
                                        TokenType::Class,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "else" => {
                                    return self.create_token_with_literal(
                                        TokenType::Else,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "if" => {
                                    return self.create_token_with_literal(
                                        TokenType::If,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "nil" => {
                                    return self.create_token_with_literal(
                                        TokenType::Nil,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "or" => {
                                    return self.create_token_with_literal(
                                        TokenType::Or,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "print" => {
                                    return self.create_token_with_literal(
                                        TokenType::Print,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "return" => {
                                    return self.create_token_with_literal(
                                        TokenType::Return,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "super" => {
                                    return self.create_token_with_literal(
                                        TokenType::Super,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "var" => {
                                    return self.create_token_with_literal(
                                        TokenType::Var,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "while" => {
                                    return self.create_token_with_literal(
                                        TokenType::And,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "false" => {
                                    return self.create_token_with_literal(
                                        TokenType::False,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "for" => {
                                    return self.create_token_with_literal(
                                        TokenType::For,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "fun" => {
                                    return self.create_token_with_literal(
                                        TokenType::Fun,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "this" => {
                                    return self.create_token_with_literal(
                                        TokenType::This,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                "true" => {
                                    return self.create_token_with_literal(
                                        TokenType::True,
                                        text,
                                        Literal::String(text),
                                    )
                                }
                                _ => return self.create_token(TokenType::Identifier, text),
                            }
                        }
                        return self.create_token(
                            TokenType::Error,
                            format!("Unexpected character {}.", character),
                        );
                    }
                };
            }
        }
    }

    fn create_token(&self, token_type: TokenType, text: String) -> Result<Token, InterpretError> {
        match token_type {
            TokenType::Error => Err(InterpretError::InterpretTokenError(TokenError {
                message: text,
                line: self.line,
            })),
            _ => Ok(Token {
                token_type,
                lexeme: text,
                line: self.line,
                literal: None,
            }),
        }
    }

    fn create_token_with_literal(
        &self,
        token_type: TokenType,
        text: String,
        literal: Literal,
    ) -> Result<Token, InterpretError> {
        Ok(Token {
            token_type,
            lexeme: text,
            line: self.line,
            literal: Some(literal),
        })
    }
}
