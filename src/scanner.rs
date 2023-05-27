#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub literal: Option<Literal>,
}

#[derive(Debug, Clone, PartialEq)]
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
    source_pos: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            source_pos: 0,
            line: 1,
        }
    }

    fn next(&mut self) -> Option<char> {
        let mut source = self.source[self.source_pos..].chars();

        match source.next() {
            Some(c) => {
                self.source_pos += c.len_utf8();
                Some(c)
            }
            None => None,
        }
    }

    pub fn next_if(&mut self, func: impl FnOnce(&char) -> bool) -> Option<char> {
        let mut source = self.source[self.source_pos..].chars().peekable();

        match source.next_if(func) {
            Some(c) => {
                self.source_pos += c.len_utf8();
                Some(c)
            }
            None => None,
        }
    }

    pub fn next_if_eq(&mut self, expected: &char) -> Option<char> {
        self.next_if(|next| next == expected)
    }

    pub fn peek(&mut self) -> Option<char> {
        self.source[self.source_pos..]
            .chars()
            .peekable()
            .peek()
            .copied()
    }

    pub fn scan_token(&mut self) -> Token {
        loop {
            let mut text = String::new();
            let c = self.next();

            if c.is_none() {
                return self.create_token(TokenType::Eof, "".to_string());
            }

            if let Some(character) = c {
                text.push(character);

                match character {
                    '(' => return self.create_token(TokenType::LeftParen, text),
                    ')' => return self.create_token(TokenType::RightParen, text),
                    '{' => return self.create_token(TokenType::LeftBrace, text),
                    '}' => return self.create_token(TokenType::RightBrace, text),
                    ',' => return self.create_token(TokenType::Comma, text),
                    '.' => return self.create_token(TokenType::Dot, text),
                    '-' => return self.create_token(TokenType::Minus, text),
                    '+' => return self.create_token(TokenType::Plus, text),
                    ';' => return self.create_token(TokenType::Semicolon, text),
                    '*' => return self.create_token(TokenType::Star, text),
                    '!' => {
                        if let Some(c) = self.next_if_eq(&'=') {
                            text.push(c);
                            self.source_pos += c.len_utf8();
                            return self.create_token(TokenType::BangEqual, text);
                        } else {
                            return self.create_token(TokenType::Bang, text);
                        }
                    }
                    '=' => {
                        if let Some(c) = self.next_if_eq(&'=') {
                            text.push(c);
                            self.source_pos += c.len_utf8();
                            return self.create_token(TokenType::EqualEqual, text);
                        } else {
                            return self.create_token(TokenType::Equal, text);
                        }
                    }
                    '>' => {
                        if let Some(c) = self.next_if_eq(&'=') {
                            text.push(c);
                            self.source_pos += c.len_utf8();
                            return self.create_token(TokenType::GreaterEqual, text);
                        } else {
                            return self.create_token(TokenType::Greater, text);
                        }
                    }
                    '<' => {
                        if let Some(c) = self.next_if_eq(&'=') {
                            text.push(c);
                            self.source_pos += c.len_utf8();
                            return self.create_token(TokenType::LessEqual, text);
                        } else {
                            return self.create_token(TokenType::Less, text);
                        }
                    }
                    '/' => {
                        if let Some(c) = self.next_if_eq(&'/') {
                            self.source_pos += c.len_utf8();
                            while let Some(ch) = self.next_if(|&x| x != '\n') {
                                self.source_pos += ch.len_utf8();
                            }
                            //discard the newline
                            self.next();
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
                            if self.peek().is_none() {
                                return self.create_token(
                                    TokenType::Error,
                                    "Unterminated string.".to_string(),
                                );
                            }

                            let next = self.next_if(|&x| x != '"');

                            match next {
                                Some(ch) => text.push(ch),
                                None => {
                                    // discard the '"'
                                    self.next();
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
                            while let Some(ch) = self.next_if(|x| x.is_digit(10)) {
                                text.push(ch);
                            }

                            // fractional
                            let peek = self.peek();
                            match peek {
                                Some(ch) => {
                                    if ch == '.' {
                                        // ensure that after '.' is digits
                                        let mut self_clone = self.source[self.source_pos..]
                                            .chars()
                                            .peekable()
                                            .clone();
                                        // discard '.'
                                        self_clone.next();
                                        let next = self_clone.peek();
                                        match next {
                                            Some(x) => {
                                                if x.is_digit(10) {
                                                    if let Some(dot) = self.next() {
                                                        text.push(dot);
                                                    }

                                                    while let Some(ch) =
                                                        self.next_if(|x| x.is_digit(10))
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
                            while let Some(ch) = self.next_if(|x| x.is_alphanumeric()) {
                                text.push(ch);
                            }

                            match text.as_str() {
                                "and" => {
                                    return self.create_token_with_literal(
                                        TokenType::And,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "class" => {
                                    return self.create_token_with_literal(
                                        TokenType::Class,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "else" => {
                                    return self.create_token_with_literal(
                                        TokenType::Else,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "if" => {
                                    return self.create_token_with_literal(
                                        TokenType::If,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "nil" => {
                                    return self.create_token_with_literal(
                                        TokenType::Nil,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "or" => {
                                    return self.create_token_with_literal(
                                        TokenType::Or,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "print" => {
                                    return self.create_token_with_literal(
                                        TokenType::Print,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "return" => {
                                    return self.create_token_with_literal(
                                        TokenType::Return,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "super" => {
                                    return self.create_token_with_literal(
                                        TokenType::Super,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "var" => {
                                    return self.create_token_with_literal(
                                        TokenType::Var,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "while" => {
                                    return self.create_token_with_literal(
                                        TokenType::And,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "false" => {
                                    return self.create_token_with_literal(
                                        TokenType::False,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "for" => {
                                    return self.create_token_with_literal(
                                        TokenType::For,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "fun" => {
                                    return self.create_token_with_literal(
                                        TokenType::Fun,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "this" => {
                                    return self.create_token_with_literal(
                                        TokenType::This,
                                        text.clone(),
                                        Literal::String(text),
                                    )
                                }
                                "true" => {
                                    return self.create_token_with_literal(
                                        TokenType::True,
                                        text.clone(),
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

    fn create_token(&self, token_type: TokenType, text: String) -> Token {
        Token {
            token_type,
            lexeme: text,
            line: self.line,
            literal: None,
        }
    }

    fn create_token_with_literal(
        &self,
        token_type: TokenType,
        text: String,
        literal: Literal,
    ) -> Token {
        Token {
            token_type,
            lexeme: text,
            line: self.line,
            literal: Some(literal),
        }
    }
}
