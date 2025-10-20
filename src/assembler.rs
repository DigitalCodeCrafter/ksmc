use std::{fmt::Display, fs::File, io::{self, Read, Write}, path::Path};

use flate2::{write::GzEncoder, Compression};

use lexer::TokenType;
use parser::{Instruction, OperandValue};
use preprocessor::{AddressKind, ValueType};
use crate::Logger;

// lexer        [X]
// parser       [X]
// preprocessor [X]
// - macros             [X]
// - directives         [X]
// - symbol resolution  [X]
// - constant eval.     [ ]
// - type checking      [X]
// encoder      [X]

mod lexer;
mod parser;
mod preprocessor;
mod encoder;

#[derive(Debug)]
pub enum AssembleError {
    InvalidEscapeCharacter(usize, char),
    UnterminatedString(usize),
    UnterminatedMacro(usize),
    MultimpleDecimalPoints(usize),
    UnexpectedChar(usize, char),
    ExpectedIdentifier(usize),
    ExpectedComma(usize),
    ExpectedWhitespace(usize),
    UnexpectedToken(usize, TokenType),
    UnexpectedEOF,
    InvalidDirective(usize, String),
    IncorrectArgumentCount(usize, String, usize, usize),
    ArgumentTypeMismatch(usize, ValueType, &'static [ValueType]),
    TypeMismatch(usize, OperandValue, ValueType),
    InvalidTypeCast(usize, OperandValue, ValueType),
    InvalidAddresKind(usize, AddressKind),
    InvalidTypeSpecifier(usize, String),
    UnresolvedSymbol(usize, String),
    UnknownInstruction(Instruction),
    UnknownSymbol(usize, String),
    UnknownMacro(usize, String),
    UnableToInferr(usize),
    FileIO(io::Error)
}
impl Display for AssembleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AssembleError::*;
        match self {
            InvalidEscapeCharacter(line, char)   => write!(f, "Error: Invalid Escape Character '\\{char}' in line {line}!"),
            UnterminatedString(line)                    => write!(f, "Error: The String beginning line {line} is missing termination '\"' !"),
            UnterminatedMacro(line)                     => write!(f, "Error: The Macro in line {line} is never terminated using '.endm'!"),
            MultimpleDecimalPoints(line)                => write!(f, "Error: Tried to use multiple decimal points in the float on line {line}!"),
            UnexpectedChar(line, char)           => write!(f, "Error: Unexpected Character '{char}' in line {line}"),
            ExpectedIdentifier(line)                    => write!(f, "Error: Expected an identifier in line {line}"),
            ExpectedComma(line)                         => write!(f, "Error: Missing comma in argument list in line {line}"),
            ExpectedWhitespace(line)                    => write!(f, "Error: Missing Whitespace after descriptor in line {line}"),
            UnexpectedToken(line, token)    => write!(f, "Error: Unexpected Token '{token:?}' in line {line}"),
            UnexpectedEOF                                       => write!(f, "Error: Unexptected End Of File"),
            InvalidDirective(line, dir)        => write!(f, "Error: The directive '.{dir}' in line {line} does not exist or was used incorrectly!"),
            UnknownInstruction(instr)             => write!(f, "Error: Unknown instruction '{}' in line {}!", instr.name, instr.line),
            UnknownSymbol(line, name)          => write!(f, "Error: Unknown Symbol '{name}' in line {line}"),
            UnknownMacro(line, name)           => write!(f, "Error: Unknown Macro '{name}' in line {line}"),
            InvalidTypeSpecifier(line, spec)   => write!(f, "Error: Invalid type specifier '.{spec}' for argument in line {line}!"),
            UnresolvedSymbol(line, name)       => write!(f, "Error: Unresolved symbol '{name}' in line {line}!"),
            UnableToInferr(line)                        => write!(f, "Error: Unable to inferr type / address kind of argument in line {line}!"),
            InvalidAddresKind(line, ak)   => write!(f, "Error: Invalid address kind '{ak:?}' for symbol in line {line}!"),
            IncorrectArgumentCount(line, inst, expected, got) => write!(f, "Error: '{inst}' in line {line} expects {expected} arguments, got {got} instead!"),
            ArgumentTypeMismatch(line, ty, allowed) => write!(f, "Error: Argument of type '{ty:?}' in line {line} is not one of the allowed argument types: {allowed:?}!"),
            TypeMismatch(line, op, ty)    => write!(f, "Error: Argument '{op:?}' in line {line} does not match with it's inferred type '{ty:?}'!"),
            InvalidTypeCast(line, op, ty) => write!(f, "Error: Cannot cast Operand '{op:?}' as type '{ty:?}' in line {line}"),
            FileIO(e) => write!(f, "{}", e),
        }
    }
}
impl std::error::Error for AssembleError {}

#[derive(Debug)]
pub struct OperandTypeSet(&'static [ValueType]);

impl OperandTypeSet {
    pub fn allows(&self, actual: ValueType) -> bool {
        self.0.contains(&actual)
    }
}

#[derive(Debug)]
struct OpSigniture {
    opcode: u8,
    operand_types: &'static [OperandTypeSet]
}

macro_rules! create_signature {
    ($op:literal, [$( ( $( $ot:ident ),+ ) ),*]) => {
        OpSigniture {
            opcode: $op,
            operand_types: &[$(
                OperandTypeSet(&[$( $ot ),+])
            ),*]
        }
    };
}

fn get_opcode_signature(name: &str) -> Option<OpSigniture> {
    use ValueType::*;

    Some(match name.to_ascii_lowercase().as_str() {
        "push"  => create_signature!(0x4e, [(Bool, Int16, Int32, Double, String, ArgMarker)]),
        "pushv" => create_signature!(0x4e, [(BooleanValue, ScalarIntValue, ScalarDoubleValue, StringValue)]),
        "pop"   => create_signature!(0x4f, []),
        "dup"   => create_signature!(0x50, []),
        "swap"  => create_signature!(0x51, []),
        "eval"  => create_signature!(0x52, []),
        "argb"  => create_signature!(0x60, []),
        "targ"  => create_signature!(0x61, []),

        "add"   => create_signature!(0x3c, []),
        "sub"   => create_signature!(0x3d, []),
        "mul"   => create_signature!(0x3e, []),
        "div"   => create_signature!(0x3f, []),
        "pow"   => create_signature!(0x40, []),
        "neg"   => create_signature!(0x47, []),
        "bool"  => create_signature!(0x48, []),

        "cgt"   => create_signature!(0x41, []),
        "clt"   => create_signature!(0x42, []),
        "cge"   => create_signature!(0x43, []),
        "cle"   => create_signature!(0x44, []),
        "ceq"   => create_signature!(0x45, []),
        "cne"   => create_signature!(0x46, []),
        "not"   => create_signature!(0x49, []),
        "and"   => create_signature!(0x4a, []),
        "or"    => create_signature!(0x4b, []),
        
        // "eof"   => create_signature!(0x31, []),
        "eop"   => create_signature!(0x32, []),
        "nop"   => create_signature!(0x33, []),
        "jmp"   => create_signature!(0x3b, [(String, Int32)]),
        "call"  => create_signature!(0x4c, [(String, Null), (String, Int32, Null)]),
        "ret"   => create_signature!(0x4d, [(Int16)]),
        "btr"   => create_signature!(0x5e, [(String, Int32)]),
        "bfa"   => create_signature!(0x3a, [(String, Int32)]),
        "bst"   => create_signature!(0x63, []),
        "wait"  => create_signature!(0x55, []),

        "sto"   => create_signature!(0x34, [(String)]),
        "uns"   => create_signature!(0x35, []),
        "stol"  => create_signature!(0x58, [(String)]),
        "stog"  => create_signature!(0x59, [(String)]),
        "stoe"  => create_signature!(0x5c, [(String)]),
        "exst"  => create_signature!(0x5f, []),

        "gmb"   => create_signature!(0x36, [(String)]),
        "smb"   => create_signature!(0x37, [(String)]),
        "gidx"  => create_signature!(0x38, []),
        "sidx"  => create_signature!(0x39, []),
        "gmet"  => create_signature!(0x57, [(String)]),

        "addt"  => create_signature!(0x53, [(Bool), (Int32)]),
        "rmvt"  => create_signature!(0x54, []),

        "bscp"  => create_signature!(0x5a, [(Int16), (Int16)]),
        "escp"  => create_signature!(0x5b, [(Int16)]),

        "pdl"   => create_signature!(0x5d, [(Int32), (Bool)]),
        "prl"   => create_signature!(0xce, [(String)]),
        "pdrl"  => create_signature!(0xcd, [(String), (Bool)]),

        "lbrt"  => create_signature!(0xf0, [(String)]),
        "tcan"  => create_signature!(0x62, []),

        _ => return None,
    })
}

pub fn assemble<P: AsRef<Path>, O: AsRef<Path>>(input_path: P, output_path: O, logger: &Logger) -> Result<(), AssembleError> {
    let input = load(input_path).map_err(|e| AssembleError::FileIO(e))?;

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer.lex_all(logger)?;

    let mut parser = parser::Parser::new(tokens);
    let statements = parser.parse_all(logger)?;

    let instructions = preprocessor::semantic_analysis(statements, logger)?;

    let output = encoder::encode(&instructions, logger)?;
    compress(output_path, &output).map_err(|e| AssembleError::FileIO(e))?;
    Ok(())
}

fn load<P: AsRef<Path>>(file_path: P) -> io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn compress<P: AsRef<Path>>(file_path: P, bytes: &[u8]) -> io::Result<()> {
    let file = File::create(file_path)?;
    let mut encoder = GzEncoder::new(file, Compression::best());
    encoder.write_all(bytes)?;
    encoder.finish()?;
    Ok(())
}

