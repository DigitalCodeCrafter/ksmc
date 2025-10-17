
// lexer    [MVP]
// parser   [MVP]
// encoder  [ ]

mod lexer;
mod parser;
mod ast;

type CResult<T> = Result<T, CompilerError>;
#[derive(Debug)]
pub enum CompilerError {
    Error { text: String },

    LexerUnexpectedChar { line: usize, col: usize, c: char },
    LexerUnterminatedString { line: usize, col: usize },

    ParseError(Vec<parser::ParseError>)
}
impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CompilerError::*;
        match self {
            Error { text } => write!(f, "Error: {}", text),
            
            LexerUnexpectedChar { line, col, c } => write!(f, "Error: Unexpected Char {} at line {}, column {}", c, line, col),
            LexerUnterminatedString { line, col } => write!(f, "Error: Unterminated String starting at line {}, column {}", line, col),
            
            ParseError(err) => write!(f, "Error: {:?}", err),
        }
    }
}
impl std::error::Error for CompilerError {}

pub fn compile(input: &str) -> Result<(), CompilerError> {
    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer.lex_all()?;

    let mut parser = parser::Parser::new(tokens);
    let root = parser.parse_program().map_err(|errs| CompilerError::ParseError(errs))?;

    todo!()
}
