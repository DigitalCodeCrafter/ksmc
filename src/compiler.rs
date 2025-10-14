
// lexer    []
// parser   []
// encoder  []

mod lexer;

#[derive(Debug)]
enum CompilerError {

}
impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => writeln!(f, "Error: Unknown Error"),
        }
    }
}
impl std::error::Error for CompilerError {}