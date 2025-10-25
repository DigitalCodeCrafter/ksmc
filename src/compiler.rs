
// [X] lexer
// [/] parser
// [ ] expander
// [ ] resolver
// [ ] validator
// [ ] type checker
// [ ] lowerer
// [ ] optimizer
// [ ] code generator

use std::path::Path;

mod lexer;
mod parser;
mod ast;

type CResult<T> = Result<T, CompilerError>;
#[derive(Debug)]
pub enum CompilerError {
    Error { text: String },

    LexError(Vec<lexer::LexError>),
    ParseError(Vec<parser::ParseError>),

    IoError(std::io::Error),
}
impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CompilerError::*;
        match self {
            Error { text } => write!(f, "Error: {}", text),

            LexError(err) => write!(f, "Error: {:?}", err),
            ParseError(err) => write!(f, "Error: {:?}", err),

            IoError(e) => write!(f, "{}", e),
        }
    }
}
impl std::error::Error for CompilerError {}

trait ToCompileResult<T> {
    fn into_cresult(self) -> CResult<T>;
}


pub fn compile(file_path: impl AsRef<Path>) -> Result<(), CompilerError> {
    let tokens = lexer::lex_file(file_path).map_err(|err| CompilerError::IoError(err))?.into_cresult()?;

    let mut parser = parser::Parser::new(tokens);
    let root = parser.parse_program().into_cresult()?;

    todo!()
}
