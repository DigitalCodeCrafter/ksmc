use super::AssembleError;
use crate::Logger;

#[derive(Debug, Clone)]
pub enum TokenType {
    Ident(String),          // ABCDE_0123
    IntegerLiteral(i32),    // 12345
    FloatingLiteral(f64),   // 9.876
    StringLiteral(String),  // "..."
    Colon,                  // :
    Comma,                  // ,
    Dot,                    // .
    NewLine,                // \n
    Whitespace,             // ' '
    Hash,                   // #
    At,                     // @
    MacroParam,             // $
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenType,
    pub line: usize,
}

pub struct Lexer {
    chars: Vec<char>,
    pos: usize,

    line: usize,
}
impl Lexer {
    pub fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect(),
            pos: 0,
            line: 1,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).cloned()
    }

    fn next_char(&mut self) -> Option<char> {
        if self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            self.pos += 1;
            Some(c)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.pos += 1;
        }
    }

    fn next_token(&mut self) -> Option<Result<Token, AssembleError>> {
        let c = self.next_char()?;

        match c {
            ' ' | '\t' | '\r' => {
                self.skip_whitespace();
                return Some(Ok(Token { kind: TokenType::Whitespace, line: self.line }));
            }
            ';' => {
                self.skip_comment();
                return self.next_token();
            }
            ':' => return Some(Ok(Token { kind: TokenType::Colon, line: self.line })),
            ',' => return Some(Ok(Token { kind: TokenType::Comma, line: self.line })),
            '.' if !matches!(self.peek(), Some('0'..='9')) => return Some(Ok(Token { kind: TokenType::Dot, line: self.line })),
            '#' => return Some(Ok(Token { kind: TokenType::Hash, line: self.line })),
            '@' => return Some(Ok(Token { kind: TokenType::At, line: self.line })),
            '$' => return Some(Ok(Token { kind: TokenType::MacroParam, line: self.line })),
            '\n' => {
                self.line += 1;
                return Some(Ok(Token { kind: TokenType::NewLine, line: self.line }))
            }
            '"' => {
                // string literal
                let start_line = self.line;
                let mut s = String::new();
                loop {
                    let Some(ch) = self.next_char() else { return Some(Err(AssembleError::UnterminatedString(self.line))); };
                    match ch {
                        '"' => break,
                        '\n' => {
                            self.line += 1;
                            s.push('\n');
                        }
                        '\\' => {
                            match self.next_char() {
                                Some('\'') => s.push('\''),
                                Some('\\') => s.push('\\'),
                                Some('"') => s.push('"'),
                                // if escaping a new line that is started with \r\n, consume the \n too
                                Some('\r') if matches!(self.peek(), Some('\n')) => { self.line += 1; self.pos += 1; },
                                Some('\n') => { self.line += 1; }
                                Some('n') => s.push('\n'),
                                Some('r') => s.push('\r'),
                                Some('t') => s.push('\t'),
                                Some('x') => {
                                    // get two hex digits
                                    let hi = self.next_char();
                                    let lo = self.next_char();

                                    match (hi, lo) {
                                        (Some(hi), Some(lo)) if hi.is_ascii_hexdigit() && lo.is_ascii_hexdigit() => {
                                            let hex_str: String = [hi, lo].iter().collect();
                                            let value = u8::from_str_radix(&hex_str, 16)
                                                .map_err(|_| AssembleError::InvalidEscapeCharacter(self.line, 'x'));
                                            match value {
                                                Ok(value) => s.push(value as char),
                                                Err(e) => return Some(Err(e))
                                            };
                                        }
                                        _ => {
                                            return Some(Err(AssembleError::InvalidEscapeCharacter(self.line, 'x')));
                                        }
                                    }
                                },
                                Some(c) => return Some(Err(AssembleError::InvalidEscapeCharacter(self.line, c))),
                                None => return Some(Err(AssembleError::UnterminatedString(self.line))),
                            }
                        },
                        _ => s.push(ch),
                    }         
                }
                return Some(Ok(Token { kind: TokenType::StringLiteral(s), line: start_line }));
            }
            '0'..='9' | '.' => {
                let mut number = c.to_string();
                let mut is_float = c == '.';

                while let Some(n) = self.peek() {
                    if n.is_ascii_digit() {
                        number.push(n);
                        self.pos += 1;
                    } else if n == '.' {
                        if is_float { return Some(Err(AssembleError::MultimpleDecimalPoints(self.line))); }
                        is_float = true;
                        number.push(n);
                        self.pos += 1;
                    } else {
                        break;
                    }
                }

                if is_float {
                    let val: f64 = number.parse().unwrap();
                    return Some(Ok(Token { kind: TokenType::FloatingLiteral(val), line: self.line }));
                } else {
                    let val: i32 = number.parse().unwrap();
                    return Some(Ok(Token { kind: TokenType::IntegerLiteral(val), line: self.line }));
                }
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut ident = c.to_string();
                while let Some(n) = self.peek() {
                    if n.is_alphanumeric() || n == '_' {
                        ident.push(n);
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                return Some(Ok(Token { kind: TokenType::Ident(ident), line: self.line }));
            }
            _ => {
                return Some(Err(AssembleError::UnexpectedChar(self.line, c)));
            }
        }
    }

    pub fn lex_all(&mut self, logger: &Logger) -> Result<Vec<Token>, AssembleError> {
        let mut tokens = Vec::new();

        while let Some(result) = self.next_token() {
            match result {
                Ok(token) => {
                    logger.log(3, format!("[Lexer] Token: {:?} (line: {})", token.kind, token.line));
                    tokens.push(token);
                },
                Err(e) => return Err(e),
            }
        }

        logger.log(2, format!("[Lexer] Lexed {} tokens", tokens.len()));

        Ok(tokens)
    }
}
