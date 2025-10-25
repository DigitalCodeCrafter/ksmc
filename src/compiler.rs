
// [X] lexer
// [/] parser
// [/] expander
// [ ] resolver
// [ ] validator
// [ ] type checker
// [ ] lowerer
// [ ] optimizer
// [ ] code generator

const FILE_SUFFIX: &str = "kep";

use std::path::Path;

mod lexer;
mod parser;
mod ast;
mod expander;

type CResult<T> = Result<T, CompilerError>;
#[derive(Debug)]
pub enum CompilerError {
    Error { text: String },

    LexError(Vec<lexer::LexError>),
    ParseError(Vec<parser::ParseError>),
    ExpandError(expander::ExpandError),

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

            IoError(e) => write!(f, "{}", e),
        }
    }
}
impl std::error::Error for CompilerError {}

trait ToCompileResult<T> {
    fn into_cresult(self) -> CResult<T>;
}


pub fn compile(file_path: impl AsRef<Path>) -> Result<(), CompilerError> {
    let tokens = lexer::lex_file(&file_path).map_err(|err| CompilerError::IoError(err))?.into_cresult()?;

    let mut ast = parser::parse_tokens(tokens).into_cresult()?;

    let mut expander = expander::Expander::new(&mut ast, file_path.as_ref().parent().unwrap(), |src| {
        let mut lexer = lexer::Lexer::new(src);
        lexer.lex_all().into_cresult()
    }, |tokens| {
        parser::parse_tokens(tokens).into_cresult()
    });

    expander.expand_modules()?;

    todo!()
}
