use std::fs::File;
use std::io::{self, BufReader, Read, Write, Cursor, Seek};
use std::ops::Shl;
use std::path::Path;

use flate2::read::{GzDecoder};

#[derive(Debug)]
pub struct ByteReader {
    cursor: Cursor<Vec<u8>>,
}
impl ByteReader {
    pub fn new(data: Vec<u8>) -> Self {
        Self { cursor: Cursor::new(data) }
    }

    pub fn position(&self) -> u64 {
        self.cursor.position()
    }
    
    pub fn seek(&mut self, pos: u64) {
        self.cursor.seek(std::io::SeekFrom::Start(pos)).unwrap();
    }

    /// Reads 1 byte as a bool
    pub fn read_bool(&mut self) -> io::Result<bool> {
        let mut buf = [0u8; 1];
        self.cursor.read_exact(&mut buf)?;
        Ok(buf[0] != 0)
    }

    /// Reads 1 byte as a u8
    pub fn read_u8(&mut self) -> io::Result<u8> {
        let mut buf = [0u8; 1];
        self.cursor.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    /// reads 2 bytes as a le u16
    pub fn read_u16(&mut self) -> io::Result<u16> {
        let mut buf = [0u8; 2];
        self.cursor.read_exact(&mut buf)?;
        Ok(u16::from_le_bytes(buf))
    }

    /// reads 2 bytes as a le i16
    pub fn read_i16(&mut self) -> io::Result<i16> {
        let mut buf = [0u8; 2];
        self.cursor.read_exact(&mut buf)?;
        Ok(i16::from_le_bytes(buf))
    }

    /// reads 4 bytes as a le u32
    pub fn read_u32(&mut self) -> io::Result<u32> {
        let mut buf = [0u8; 4];
        self.cursor.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    /// reads 4 bytes as a le i32
    pub fn read_i32(&mut self) -> io::Result<i32> {
        let mut buf = [0u8; 4];
        self.cursor.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    /// reads 4 bytes as a le f32
    pub fn read_f32(&mut self) -> io::Result<f32> {
        let mut buf = [0u8; 4];
        self.cursor.read_exact(&mut buf)?;
        Ok(f32::from_le_bytes(buf))
    }

    /// reads 8 bytes as a le f64
    pub fn read_f64(&mut self) -> io::Result<f64> {
        let mut buf = [0u8; 8];
        self.cursor.read_exact(&mut buf)?;
        Ok(f64::from_le_bytes(buf))
    }

    /// reads a u8 length coded utf-8 string
    pub fn read_string(&mut self) -> io::Result<String> {
        let length = self.read_u8()? as usize;
        let mut buf = vec![0u8; length];
        self.cursor.read_exact(&mut buf)?;
        Ok(String::from_utf8(buf).expect("Invalid UTF-8 in string"))
    }

    /// reads (N <= 8) bytes as le u64
    pub fn read_un(&mut self, n: u8) -> io::Result<u64> {
        assert!(n > 0 && n <= 8);
        let mut buf = vec![0u8; n as usize];
        self.cursor.read_exact(&mut buf)?;
        let mut out = 0;
        for i in 0..n {
            out += (*buf.get(i as usize).unwrap_or(&0) as u64).shl((n-i-1) * 8);
        }
        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub enum KOSArgument {
    Null,
    Bool(bool),
    Byte(u8),
    Int16(i16),
    Int32(i32),
    Float(f32),
    Double(f64),
    String(String),
    ArgMarker,
    ScalarIntValue(i32),
    ScalarDoubleValue(f64),
    BooleanValue(bool),
    StringValue(String),
}
impl KOSArgument {
    pub fn read(reader: &mut ByteReader) -> io::Result<Self> {
        let type_id = reader.read_u8()?;
        Ok(match type_id {
            0 => Self::Null,
            1 => Self::Bool(reader.read_bool()?),
            2 => Self::Byte(reader.read_u8()?),
            3 => Self::Int16(reader.read_i16()?),
            4 => Self::Int32(reader.read_i32()?),
            5 => Self::Float(reader.read_f32()?),
            6 => Self::Double(reader.read_f64()?),
            7 => Self::String(reader.read_string()?),
            8 => Self::ArgMarker,
            9 => Self::ScalarIntValue(reader.read_i32()?),
            10 => Self::ScalarDoubleValue(reader.read_f64()?),
            11 => Self::BooleanValue(reader.read_bool()?),
            12 => Self::StringValue(reader.read_string()?),
            other => panic!("unknown type ID: {other}")
        })
    }

    pub fn read_at(reader: &mut ByteReader, addr: u64) -> io::Result<Self> {
        let cur = reader.cursor.position();
        reader.seek(addr);
        let out = Self::read(reader)?;
        reader.seek(cur);
        Ok(out)
    }
}

macro_rules! back_to_enum {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $( $(#[$vmeta])* $vname  $(= $val)?,)*
        }
        impl std::convert::TryFrom<u8> for $name {
            type Error = ();
            
            fn try_from(v: u8) -> Result<Self, Self::Error> {
                match v {
                    $(x if x == $name::$vname as u8 => Ok($name::$vname),)*
                    _ => Err(()),
                }
            }
        }
    };
}

back_to_enum! {
#[derive(Debug, Clone, Copy)]
enum Opcode {
    EOF            = 0x31,
    EOP            = 0x32,
    NOP            = 0x33,
    STORE          = 0x34,
    UNSET          = 0x35,
    GETMEMBER      = 0x36,
    SETMEMBER      = 0x37,
    GETINDEX       = 0x38,
    SETINDEX       = 0x39,
    BRANCHFALSE    = 0x3a,
    JUMP           = 0x3b,
    ADD            = 0x3c,
    SUB            = 0x3d,
    MULT           = 0x3e,
    DIV            = 0x3f,
    POW            = 0x40,
    GT             = 0x41,
    LT             = 0x42,
    GTE            = 0x43,
    LTE            = 0x44,
    EQ             = 0x45,
    NE             = 0x46,
    NEGATE         = 0x47,
    BOOL           = 0x48,
    NOT            = 0x49,
    AND            = 0x4a,
    OR             = 0x4b,
    CALL           = 0x4c,
    RETURN         = 0x4d,
    PUSH           = 0x4e,
    POP            = 0x4f,
    DUP            = 0x50,
    SWAP           = 0x51,
    EVAL           = 0x52,
    ADDTRIGGER     = 0x53,
    REMOVETRIGGER  = 0x54,
    WAIT           = 0x55,

    GETMETHOD      = 0x57,
    STORELOCAL     = 0x58,
    STOREGLOBAL    = 0x59,
    PUSHSCOPE      = 0x5a,
    POPSCOPE       = 0x5b,
    STOREEXIST     = 0x5c,
    PUSHDELEGATE   = 0x5d,
    BRANCHTRUE     = 0x5e,
    EXISTS         = 0x5f,
    ARGBOTTOM      = 0x60,
    TESTARGBOTTOM  = 0x61,
    TESTCANCELLED  = 0x62,
    JUMPSTACK      = 0x63,

    PUSHRELOCATELATER = 0xce,
    PUSHDELEGATERELOCATELATER = 0xcd,
    LABELRESET = 0xf0,
}
}

struct KOSInstruction {
    label: String,
    opcode: Opcode,
    args: Vec<KOSArgument>,
}
impl KOSInstruction {
    fn argument_count(opcode: Opcode) -> u8 {
        match opcode {
            Opcode::EOF         => 0,
            Opcode::EOP         => 0,
            Opcode::ADD         => 0,
            Opcode::ADDTRIGGER  => 2,
            Opcode::AND         => 0,
            Opcode::ARGBOTTOM   => 0,
            Opcode::BOOL        => 0,
            Opcode::BRANCHFALSE => 1, // label / distance
            Opcode::BRANCHTRUE  => 1, // label / distance
            Opcode::CALL        => 2, // label, destination
            Opcode::DIV         => 0,
            Opcode::DUP         => 0,
            Opcode::EQ          => 0,
            Opcode::EVAL        => 0,
            Opcode::EXISTS      => 0,
            Opcode::GETINDEX    => 0,
            Opcode::GETMEMBER   => 1, // identifier
            Opcode::GETMETHOD   => 1, // identifier
            Opcode::GT          => 0,
            Opcode::GTE         => 0,
            Opcode::JUMP        => 1, // destination
            Opcode::LABELRESET  => 1, // next label
            Opcode::LT          => 0,
            Opcode::LTE         => 0,
            Opcode::MULT        => 0,
            Opcode::NE          => 0,
            Opcode::NEGATE      => 0,
            Opcode::NOP         => 0,
            Opcode::NOT         => 0,
            Opcode::OR          => 0,
            Opcode::POP         => 0,
            Opcode::POPSCOPE    => 1, // levels
            Opcode::POW         => 0,
            Opcode::PUSH        => 1, // val
            Opcode::PUSHDELEGATE    => 2, // entrypoint, withClosure
            Opcode::PUSHDELEGATERELOCATELATER   => 2, // label, withClosure
            Opcode::PUSHRELOCATELATER => 1, // label
            Opcode::PUSHSCOPE   => 2, // scopeId, parentScopeId
            Opcode::REMOVETRIGGER   => 0,
            Opcode::RETURN      => 1, // depth
            Opcode::SETINDEX    => 0,
            Opcode::SETMEMBER   => 1, // identifier
            Opcode::STORE       => 1, // identifier
            Opcode::STOREEXIST  => 1, // identifier
            Opcode::STOREGLOBAL => 1, // identifier
            Opcode::STORELOCAL  => 1, // identifier
            Opcode::SUB         => 0,
            Opcode::SWAP        => 0,
            Opcode::TESTARGBOTTOM   => 0,
            Opcode::TESTCANCELLED   => 0,
            Opcode::UNSET       => 0,
            Opcode::WAIT        => 0,
            Opcode::JUMPSTACK   => 0,
        }
    }
}


pub fn disassemble<P: AsRef<Path>, O: AsRef<Path>>(input_path: P, output_path: O) -> io::Result<()> {
    let file = File::open(input_path)?;
    let mut decoder = GzDecoder::new(BufReader::new(file));

    let mut contents = Vec::new();
    decoder.read_to_end(&mut contents)?;

    println!("Decompressed {} bytes", contents.len());

    let mut reader = ByteReader::new(contents);

    // validate magic
    let mut buf = [0u8; 4];
    reader.cursor.read_exact(&mut buf).unwrap();
    if buf != [b'k', 0x03, b'X', b'E'] { panic!("Invalid Magic number") };

    let (arg_offset, width) = prepare_argument_section(&mut reader).unwrap();

    println!("{} byte indexed Argument Section starts at: {:#06X}", width, arg_offset);

    let instructions = parse_instructions(&mut reader, width, arg_offset);

    println!("Parsed {} Instructions", instructions.len());

    let kasm_source = to_kasm(&instructions);
    let mut file = File::create(output_path)?;
    file.write_all(kasm_source.as_bytes())?;
    println!("Wrote disassembly to file");
    Ok(())
}

fn prepare_argument_section(reader: &mut ByteReader) -> Option<(u64, u8)> {
    let pos = reader.position();
    
    // validate that this is the argument section
    let mut buf = [0u8; 2];
    reader.cursor.read_exact(&mut buf).ok()?;
    if buf != [b'%', b'A'] { return None };

    let width = reader.read_u8().ok()?;

    loop {
        let mut buf = [0u8; 1];
        reader.cursor.read_exact(&mut buf).unwrap();
        reader.seek(reader.position() - 1);
        if buf == [b'%'] {
            break;
        }
        // discard output, only used to traverse section
        KOSArgument::read(reader).ok()?;
    }

    Some((pos, width))
}

fn parse_instructions(reader: &mut ByteReader, arg_width: u8, arg_offset: u64) -> Vec<KOSInstruction> {
    let mut instructions = Vec::new();

    let mut f_section = Vec::new();
    let mut i_section = Vec::new();
    let mut m_section = Vec::new();

    while let Some((inst, section_type)) = parse_instruction_section(reader, arg_width, arg_offset) {
        match section_type {
            b'F' => f_section.extend(inst),
            b'I' => i_section.extend(inst),
            b'M' => m_section.extend(inst),
            _ => {}
        }
    }
    instructions.extend(f_section);
    instructions.extend(i_section);
    instructions.extend(m_section);
    add_labels(&mut instructions);

    instructions
}

fn parse_instruction_section(reader: &mut ByteReader, arg_width: u8, arg_offset: u64) -> Option<(Vec<KOSInstruction>, u8)> {
    let mut instructions = Vec::new();
    
    // validate code section
    let mut buf = [0u8; 2];
    reader.cursor.read_exact(&mut buf).ok()?;
    if buf[0] != b'%' || ![b'F', b'I', b'M'].contains(&buf[1]) { return None };
    let section_type = buf[1];

    while let Ok(byte) = reader.read_u8() {
        if byte == b'%' {
            reader.seek(reader.position() - 1);
            break;
        }

        let opcode = Opcode::try_from(byte).unwrap_or(Opcode::NOP);
        let arg_count = KOSInstruction::argument_count(opcode);


        let mut args = Vec::new();
        for _ in 0..arg_count {
            let addr = reader.read_un(arg_width).ok()?;
            args.push(KOSArgument::read_at(reader, addr + arg_offset).ok()?);
        }

        
        instructions.push(KOSInstruction { opcode, args, label: "#####".to_string() });
    }

    Some((instructions, section_type))
}

fn add_labels(instructions: &mut [KOSInstruction]) {
    let mut next_label = "@0001".to_string();

    for inst in instructions {
        inst.label = next_label.clone();

        next_label = match inst.opcode {
            Opcode::LABELRESET => {
                match inst.args.first() {
                    Some(KOSArgument::String(label)) => {
                        label.clone()
                    },
                    _ => {
                        next_consecutive_label(&next_label)
                    }
                }
            }
            _ => next_consecutive_label(&next_label),
        };
    }
}

fn next_consecutive_label(label: &str) -> String {
    // find digit start
    let mut digit_start = label.len();
    for (i, ch) in label.char_indices().rev() {
        if !ch.is_ascii_digit() {
            break;
        }
        digit_start = i;
    }

    if digit_start == label.len() {
        // no trailing digits -> append "1"
        return format!("{label}1");
    }

    let (prefix, num_str) = label.split_at(digit_start);

    match num_str.parse::<u32>() {
        Ok(number) => {
            let width = num_str.len();
            format!("{prefix}{:0width$}", number + 1)
        },
        Err(_) => format!("{label}1")
    }
}

fn format_arg(arg: &KOSArgument) -> String {
    match arg {
        KOSArgument::Null => "#".to_string(),
        KOSArgument::Bool(v) => v.to_string(),
        KOSArgument::Byte(v) => v.to_string(),
        KOSArgument::Int16(v) => v.to_string(),
        KOSArgument::Int32(v) => v.to_string(),
        KOSArgument::Float(v) => v.to_string(),
        KOSArgument::Double(v) => v.to_string(),
        KOSArgument::String(s) => format!("\"{s}\""),
        KOSArgument::ArgMarker => "@".to_string(),
        KOSArgument::ScalarIntValue(v) => v.to_string(),
        KOSArgument::ScalarDoubleValue(v) => v.to_string(),
        KOSArgument::BooleanValue(v) => v.to_string(),
        KOSArgument::StringValue(s) => format!("\"{s}\""),
    }
}

fn format_instruction(inst: &KOSInstruction) -> String {
    let inst_name = match inst.opcode {
        Opcode::PUSH                        => {
            match inst.args.first() {
                Some(KOSArgument::StringValue(_)) => "pushv",
                Some(KOSArgument::BooleanValue(_)) => "pushv",
                Some(KOSArgument::ScalarIntValue(_)) => "pushv",
                Some(KOSArgument::ScalarDoubleValue(_)) => "pushv",
                _ => "push"
            }
        }
        Opcode::POP                         => "pop",
        Opcode::DUP                         => "dup",
        Opcode::SWAP                        => "swap",
        Opcode::EVAL                        => "eval",
        Opcode::ARGBOTTOM                   => "argb",
        Opcode::TESTARGBOTTOM               => "targ",
        Opcode::ADD                         => "add",
        Opcode::SUB                         => "sub",
        Opcode::MULT                        => "mul",
        Opcode::DIV                         => "div",
        Opcode::POW                         => "pow",
        Opcode::NEGATE                      => "neg",
        Opcode::BOOL                        => "bool",
        Opcode::GT                          => "cgt",
        Opcode::GTE                         => "cge",
        Opcode::EQ                          => "ceq",
        Opcode::LTE                         => "cle",
        Opcode::LT                          => "clt",
        Opcode::NE                          => "cne",
        Opcode::NOT                         => "not",
        Opcode::AND                         => "and",
        Opcode::OR                          => "or",
        Opcode::EOP                         => "eop",
        Opcode::EOF                         => "eof",
        Opcode::NOP                         => "nop",
        Opcode::JUMP                        => "jmp",
        Opcode::CALL                        => "call",
        Opcode::RETURN                      => "ret",
        Opcode::BRANCHTRUE                  => "btr",
        Opcode::BRANCHFALSE                 => "bfa",
        Opcode::JUMPSTACK                   => "bst",
        Opcode::WAIT                        => "wait",
        Opcode::STORE                       => "sto",
        Opcode::UNSET                       => "uns",
        Opcode::STORELOCAL                  => "stol",
        Opcode::STOREGLOBAL                 => "stog",
        Opcode::STOREEXIST                  => "stoe",
        Opcode::EXISTS                      => "exst",
        Opcode::GETMEMBER                   => "gmb",
        Opcode::SETMEMBER                   => "smb",
        Opcode::GETINDEX                    => "gidx",
        Opcode::SETINDEX                    => "sidx",
        Opcode::GETMETHOD                   => "gmtd",
        Opcode::ADDTRIGGER                  => "addt",
        Opcode::REMOVETRIGGER               => "rmvt",
        Opcode::PUSHSCOPE                   => "bscp",
        Opcode::POPSCOPE                    => "escp",
        Opcode::PUSHDELEGATE                => "pdl",
        Opcode::PUSHRELOCATELATER           => "prl",
        Opcode::PUSHDELEGATERELOCATELATER   => "pdrl",
        Opcode::TESTCANCELLED               => "tcan",
        Opcode::LABELRESET                  => "lbrt",
    };
    let args_str = inst.args.iter()
        .map(format_arg)
        .collect::<Vec<_>>()
        .join(", ");
    
    if args_str.is_empty() {
        format!("{}", inst_name)
    } else {
        format!("{} {}", inst_name, args_str)
    }
}

fn to_kasm(instructions: &[KOSInstruction]) -> String {
    let mut output = String::new();
    for inst in instructions {
        output.push_str(&format_instruction(inst));
        output.push('\n');
    }
    output
}
