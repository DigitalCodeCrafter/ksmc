
pub mod assembler;
pub mod disassemble;
pub mod compiler;

pub struct Logger {
    level: u8,
}
impl Logger {
    pub fn new(level: u8) -> Self { Logger { level } }
    pub fn log(&self, level: u8, msg: impl std::fmt::Display) {
        if self.level >= level {
            println!("{}", msg);
        }
    }
}
