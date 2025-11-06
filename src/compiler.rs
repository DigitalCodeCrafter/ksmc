// TODO: centralized tables

// [x] lexer
// [/] parser
// [/] expander
// [/] resolver
// [x] validator
// [ ] constant evaluation ?
// [ ] type checker
// [x] KOS FORK WITH POINTERS!!
// [ ] lowerer
// [ ] optimizer
// [ ] code generator

const FILE_SUFFIX: &str = "kep";

use std::path::Path;

mod lexer;
mod parser;
mod ast;
mod expander;
mod resolver;
mod validator;
mod type_checker;

#[derive(Debug)]
pub enum CompilerError {
    Error { text: String },

    LexError(Vec<lexer::LexError>),
    ParseError(Vec<parser::ParseError>),
    ExpandError(expander::ExpandError),
    ResolveError(Vec<resolver::ResolveError>),
    ValidationError(Vec<validator::ValidationError>),

    IoError(std::io::Error),
}
impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CompilerError::*;
        match self {
            Error { text } => write!(f, "Error: {}", text),

            LexError(err) => write!(f, "Error: {:?}", err),
            ParseError(err) => write!(f, "Error: {:?}", err),
            ExpandError(err) => write!(f, "Error: {:?}", err),
            ResolveError(err) => write!(f, "Error: {:?}", err),
            ValidationError(err) => write!(f, "Error: {:?}", err),

            IoError(e) => write!(f, "{}", e),
        }
    }
}
impl std::error::Error for CompilerError {}

pub fn compile(file_path: impl AsRef<Path>) -> Result<(), CompilerError> {
    let src = std::fs::read_to_string(&file_path)
        .map_err(|e| CompilerError::IoError(e))?;

    let tokens = lexer::lex_all(&src)?;

    let mut ast = parser::parse_all(tokens)?;

    expander::expand_all(&mut ast, file_path.as_ref().parent().unwrap())?;

    let symbols = resolver::resolve_all(&ast)?;
   
    validator::validate_all(&ast)?;

    todo!()
}
