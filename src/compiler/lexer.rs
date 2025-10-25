// Source text -> Tokens

use std::{fs::File, io::Read, path::Path};
use crate::compiler::{ast::{Pos, Span}, CompilerError, ToCompileResult};

const CASE_SENSITIVITY: bool = true;

#[derive(Debug, Clone)]
pub struct LexError {
    pub span: Span,
    pub message: String,
}

impl<T> ToCompileResult<T> for Result<T, Vec<LexError>> {
    fn into_cresult(self) -> Result<T, super::CompilerError> {
        self.map_err(|err| CompilerError::LexError(err))
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Identifiers and Literals
    Identifier(String),
    Int(i32),
    Float(f64),
    String(String),

    // Keywords
    Mod,
    Use,
    Pub,
    True,
    False,
    Let,
    Mut,
    Fn,
    Return,
    If,
    Else,
    While,
    Loop,
    Break,

    // Operators
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Eq,         // =
    EqEq,       // ==
    NotEq,      // !=
    Lt,         // <
    LtEq,       // <=
    Gt,         // >
    GtEq,       // >=
    AndAnd,     // &&
    OrOr,       // ||
    Not,        // !
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    Arrow,      // ->
    
    // Delimiters
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    Comma,      // ,
    Colon,      // :
    ColCol,     // ::
    Semi,       // ;
    Underscore, // _
    Dot,        // .

    // Misc
    Invalid(String),
    UnterminatedString(String),

    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn lex_file(path: impl AsRef<Path>) -> std::io::Result<Result<Vec<Token>, Vec<LexError>>> {
    let mut file = File::open(path)?;
    let mut src = String::new();
    file.read_to_string(&mut src)?;

    let mut lexer = Lexer::new(&src);
    Ok(lexer.lex_all())
}

pub struct Lexer {
    src: Vec<char>,
    pos: usize,
    errors: Vec<LexError>,
    line: usize,
    col: usize,
}
impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            src: input.chars().collect(),
            pos: 0,
            errors: Vec::new(),
            line: 1,
            col: 1,
        }
    }

    pub fn lex_all(&mut self) -> Result<Vec<Token>, Vec<LexError>> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            if matches!(tok.kind, TokenKind::EOF) {
                tokens.push(tok); break;
            } else {
                tokens.push(tok);
            }
        }
        if self.errors.is_empty() {
            Ok(tokens)
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comment();

        let start_line = self.line;
        let start_col = self.col;

        let Some(c) = self.peek() else {
            return self.make_token(TokenKind::EOF, start_line, start_col);
        };

        if c.is_alphabetic() {
            self.lex_identifier_or_keyword(start_line, start_col)
        } else if c.is_ascii_digit() {
            self.lex_number(start_line, start_col)
        } else if c == '.' && self.peek_ahead(1).map_or(false, |n| n.is_ascii_digit()) {
            self.lex_number(start_line, start_col)
        } else if c == '-' && self.peek_ahead(1).map_or(false, |n| n.is_ascii_digit()) {
            self.lex_number(start_line, start_col)
        } else if c == '"' {
            self.lex_string(start_line, start_col)
        } else {
            self.lex_symbol(start_line, start_col)
        }
    }

    fn make_token(&self, kind: TokenKind, line: usize, col: usize) -> Token {
        Token { kind, span: self.make_span(line, col) }
    }

    fn make_span(&self, line: usize, col: usize) -> Span {
        Span { start: Pos { line, col }, end: Pos { line: self.line, col: self.col } }
    }

    // --------- Iteration ---------

    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).copied()
    }
    
    fn peek_ahead(&self, n: usize) -> Option<char> {
        self.src.get(self.pos + n).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += 1;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    // --------- Skipping whitespaces & comments ---------

    fn skip_whitespace_and_comment(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => { self.advance(); }
                Some('/') if self.peek_ahead(1) == Some('/') =>  {
                    while let Some(c) = self.peek() {
                        self.advance();
                        if c == '\n' { break; }
                    }
                }
                Some('/') if self.peek_ahead(1) == Some('*') => {
                    let start_line = self.line;
                    let start_col = self.col;
                    let mut block_count = 0;
                    loop {
                        if self.peek() == Some('/') && self.peek_ahead(1) == Some('*') {
                            self.advance(); self.advance(); // consume "/*"
                            block_count += 1;
                        }
                        if self.peek() == Some('*') && self.peek_ahead(1) == Some('/') {
                            self.advance(); self.advance(); // consume "*/"
                            block_count -= 1;
                        }
                        if block_count <= 0 { break; }
                        
                        if self.advance().is_none() {
                            self.error(self.make_span(start_line, start_col), "Unterminated block comment");
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
    }

    // --------- Identifiers & Keywords ---------

    fn lex_identifier_or_keyword(&mut self, line: usize, col: usize) -> Token {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if !CASE_SENSITIVITY {
            s.make_ascii_lowercase();
        }
        
        let kind = match s.as_str() {
            "mod" => TokenKind::Mod,
            "use" => TokenKind::Use,
            "pub" => TokenKind::Pub,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            _ => TokenKind::Identifier(s),
        };

        self.make_token(kind, line, col)
    }

    // --------- Numbers ---------

    fn lex_number(&mut self, line: usize, col: usize) -> Token {
        let mut num_str = String::new();
        let mut has_dot = false;
        let mut has_exp = false;

        // prefixes
        if self.peek() == Some('0') {
            if let Some(next) = self.peek_ahead(1) {
                match next {
                    'x' | 'X' => {
                        self.advance(); self.advance(); // consume "0x"
                        return self.lex_based_number(16, line, col);
                    }
                    'b' | 'B' => {
                        self.advance(); self.advance(); // consume "0b"
                        return self.lex_based_number(2, line, col);
                    }
                    'o' | 'O' => {
                        self.advance(); self.advance(); // consume "0o"
                        return self.lex_based_number(8, line, col);
                    }
                    _ => {}
                }
            }
        }

        // decimal or floating
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '_' {
                num_str.push(c);
                self.advance();
            } else if c == '.' && !has_dot && self.peek_ahead(1).map_or(false, |n| n.is_ascii_digit()) {
                has_dot = true;
                num_str.push(c);
                self.advance();
            } else if c == '-' && num_str.len() == 0 && self.peek_ahead(1).map_or(false, |n| n.is_ascii_digit()) {
                num_str.push(c);
                self.advance();
            } else if (c == 'e' || c == 'E') && !has_exp {
                if let Some(next) = self.peek_ahead(1) {
                    if next.is_ascii_digit() {
                        num_str.push(c);
                        self.advance();
                        has_exp = true;
                    } else if (next == '+' || next == '-') && self.peek_ahead(2).map_or(false, |n| n.is_ascii_digit()) {
                        num_str.push(c);
                        self.advance();
                        num_str.push(next);
                        self.advance();
                        has_exp = true;
                    }
                }
            } else {
                break;
            }
        }

        let cleaned = num_str.replace("_", "");
        let kind = if has_dot || has_exp {
            TokenKind::Float(cleaned.parse().unwrap_or(0.0))
        } else {
            TokenKind::Int(cleaned.parse().unwrap_or(0))
        };

        self.make_token(kind, line, col)
    }

    fn lex_based_number(&mut self, base: u32,line: usize, col: usize) -> Token {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            match c {
                '_' => {
                    s.push(c);
                    self.advance();
                }
                '0'..='1' if base >= 2 => {
                    s.push(c);
                    self.advance();
                }
                '2'..='7' if base >= 8 => {
                    s.push(c);
                    self.advance();
                }
                '8'..='9' | 'A'..='F' | 'a'..='f' if base >= 16 => {
                    s.push(c);
                    self.advance();
                }
                _ => break,
            }
        }

        let cleaned = s.replace("_", "");
        let value = i32::from_str_radix(&cleaned, base).unwrap_or(0);
        self.make_token(TokenKind::Int(value), line, col)
    }

    // --------- Strings ---------

    fn lex_string(&mut self, line: usize, col: usize) -> Token {
        self.advance(); // consume '"'
        let mut s = String::new();

        loop {
            match self.advance() {
                Some('"') => break,
                Some('\\') => {
                    if let Some(escaped) = self.advance() {
                        match escaped {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '"' => s.push('"'),
                            '\\' => s.push('\\'),
                            '\r' if self.peek() == Some('\n') => { self.advance(); }
                            '\r' => {},
                            '\n' => {},
                            _ => s.push(escaped),
                        }
                    }
                }
                Some(c) => s.push(c),
                None => {
                    self.error(self.make_span(line, col), "Unterminated string literal");
                    return self.make_token(TokenKind::UnterminatedString(s), line, col);
                }
            }
        }
        
        self.make_token(TokenKind::String(s), line, col)
    }

    // --------- Symbols & Operators ---------

    fn lex_symbol(&mut self, line: usize, col: usize) -> Token {
        use TokenKind::*;
        let c = self.advance().unwrap();

        let kind = match c {
            '+' if self.match_next('=') => PlusEq,
            '+' => Plus,
            '-' if self.match_next('>') => Arrow,
            '-' if self.match_next('=') => MinusEq,
            '-' => Minus,
            '*' if self.match_next('=') => StarEq,
            '*' => Star,
            '/' if self.match_next('=') => SlashEq,
            '/' => Slash,
            '=' if self.match_next('=') => EqEq,
            '=' => Eq,
            '!' if self.match_next('=') => NotEq,
            '!' => Not,
            '<' if self.match_next('=') => LtEq,
            '<' => Lt,
            '>' if self.match_next('=') => GtEq,
            '>' => Gt,
            '&' if self.match_next('&') => AndAnd,
            '|' if self.match_next('|') => OrOr,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '[' => LBracket,
            ']' => RBracket,
            ',' => Comma,
            ';' => Semi,
            ':' if self.match_next(':') => ColCol,
            ':' => Colon,
            '.' => Dot,
            '_' => Underscore,
            c => {
                self.error(self.make_span(line, col), format!("Unexpected character '{}'", c));
                Invalid(c.to_string())
            }
        };

        self.make_token(kind, line, col)
    }

    // --------- Errors ---------

    fn error(&mut self, span: Span, message: impl Into<String>) {
        self.errors.push(LexError {
            span,
            message: message.into(),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexing_1() {
        let src = r#"
            let x = 3.14;
            if x > 1.0 { print("ok"); }
        "#;
        let expected = vec![
            TokenKind::Let, TokenKind::Identifier("x".into()), TokenKind::Eq, TokenKind::Float(3.14), TokenKind::Semi,
            TokenKind::If, TokenKind::Identifier("x".into()), TokenKind::Gt, TokenKind::Float(1.0), TokenKind::LBrace,
            TokenKind::Identifier("print".into()), TokenKind::LParen, TokenKind::String("ok".to_string()), TokenKind::RParen, TokenKind::Semi,
            TokenKind::RBrace, TokenKind::EOF,
        ];

        let mut lexer = Lexer::new(src);
        let mut i = 0;
        loop {
            let tok = lexer.next_token();
            println!("{:?}", tok);
            assert_eq!(tok.kind, expected[i]);
            if matches!(tok.kind, TokenKind::EOF) { break; }
            i += 1;
        }
        assert!(lexer.errors.is_empty(), "ERRORS: {:?}", lexer.errors);
    }

    #[test]
    fn test_lexing_numbers() {
        let src = r#"
        1_23 3.14 1e-3 0b1010 0o755 0xFFAA
        "#;

        let expected = vec![
            TokenKind::Int(123), TokenKind::Float(3.14), TokenKind::Float(0.001), TokenKind::Int(10), TokenKind::Int(493), TokenKind::Int(65450),
            TokenKind::EOF,
        ];
        let mut lexer = Lexer::new(src);
        let mut i = 0;
        loop {
            let tok = lexer.next_token();
            println!("{:?}", tok);
            assert_eq!(tok.kind, expected[i]);
            if matches!(tok.kind, TokenKind::EOF) { break; }
            i += 1;
        }
        assert!(lexer.errors.is_empty(), "ERRORS: {:?}", lexer.errors);
    }

    #[test]
    fn test_error() {
        let src = r#"
        ° "adsjd
        "#;

        let expected = vec![
            TokenKind::Invalid("°".into()), TokenKind::UnterminatedString("adsjd\n        ".into()), TokenKind::EOF
        ];
        let mut lexer = Lexer::new(src);
        let mut i = 0;
        loop {
            let tok = lexer.next_token();
            println!("{:?}", tok);
            assert_eq!(tok.kind, expected[i]);
            if matches!(tok.kind, TokenKind::EOF) { break; }
            i += 1;
        }
        assert_eq!(lexer.errors.len(), 2, "Error length mismatch: {:?}", lexer.errors);
        println!("Errors: {:?}", lexer.errors)
    }
}
