from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple
from enum import IntEnum, Enum
import gzip
import struct
import io

MAGIC = b'k\x03XE'
SECTION_HEADERS = {b'%A', b'%F', b'%I', b'%M'}

class KOSValueType(IntEnum):
    PSEUDO_NULL = 0
    BOOL = 1
    BYTE = 2
    INT16 = 3
    INT32 = 4
    FLOAT = 5
    DOUBLE = 6
    STRING = 7
    ARG_MARKER = 8
    SCALAR_INT = 9
    SCALAR_DOUBLE = 10
    BOOLEAN_VALUE = 11
    STRING_VALUE = 12

class ByteCode(Enum):
    BOGUS = 0
    DELIMITER = 0x25
    EOF            = 0x31
    EOP            = 0x32
    NOP            = 0x33
    STORE          = 0x34
    UNSET          = 0x35
    GETMEMBER      = 0x36
    SETMEMBER      = 0x37
    GETINDEX       = 0x38
    SETINDEX       = 0x39
    BRANCHFALSE    = 0x3a
    JUMP           = 0x3b
    ADD            = 0x3c
    SUB            = 0x3d
    MULT           = 0x3e
    DIV            = 0x3f
    POW            = 0x40
    GT             = 0x41
    LT             = 0x42
    GTE            = 0x43
    LTE            = 0x44
    EQ             = 0x45
    NE             = 0x46
    NEGATE         = 0x47
    BOOL           = 0x48
    NOT            = 0x49
    AND            = 0x4a
    OR             = 0x4b
    CALL           = 0x4c
    RETURN         = 0x4d
    PUSH           = 0x4e
    POP            = 0x4f
    DUP            = 0x50
    SWAP           = 0x51
    EVAL           = 0x52
    ADDTRIGGER     = 0x53
    REMOVETRIGGER  = 0x54
    WAIT           = 0x55

    GETMETHOD      = 0x57
    STORELOCAL     = 0x58
    STOREGLOBAL    = 0x59
    PUSHSCOPE      = 0x5a
    POPSCOPE       = 0x5b
    STOREEXIST     = 0x5c
    PUSHDELEGATE   = 0x5d
    BRANCHTRUE     = 0x5e
    EXISTS         = 0x5f
    ARGBOTTOM      = 0x60
    TESTARGBOTTOM  = 0x61
    TESTCANCELLED  = 0x62

    PUSHRELOCATELATER = 0xce
    PUSHDELEGATERELOCATELATER = 0xcd
    LABELRESET = 0xf0

ARG_COUNT: Dict[ByteCode, int] = {
    ByteCode.NOP:           0,
    ByteCode.STORE:         1,
ByteCode.UNSET:         0,
ByteCode.GETMEMBER:     0,
ByteCode.SETMEMBER:     0,
ByteCode.GETINDEX:      0,
ByteCode.SETINDEX:      0,
    ByteCode.BRANCHFALSE:   1,
ByteCode.JUMP:          0,
ByteCode.ADD:           0,
ByteCode.SUB:           0,
ByteCode.MULT:          0,
ByteCode.DIV:           0,
ByteCode.POW:           0,
ByteCode.GT:            0,
ByteCode.LT:            0,
ByteCode.GTE:           0,
ByteCode.LTE:           0,
ByteCode.EQ:            0,
ByteCode.NE:            0,
ByteCode.NEGATE:        0,
ByteCode.BOOL:          0,
ByteCode.NOT:           0,
ByteCode.AND:           0,
ByteCode.OR:            0,
    ByteCode.CALL:          2,
    ByteCode.RETURN:        1,
    ByteCode.PUSH:          1,
    ByteCode.POP:           0,
ByteCode.DUP:           0,
ByteCode.SWAP:          0,
ByteCode.EVAL:          0,
ByteCode.ADDTRIGGER:    0,
ByteCode.REMOVETRIGGER: 0,
ByteCode.WAIT:          0,
    # 56: removed
ByteCode.GETMETHOD:     0,
    ByteCode.STORELOCAL:    1,
ByteCode.STOREGLOBAL:   0,
    ByteCode.PUSHSCOPE:     2,
    ByteCode.POPSCOPE:      1,
ByteCode.STOREEXIST:    0,
ByteCode.PUSHDELEGATE:  0,
ByteCode.BRANCHTRUE:    0,
ByteCode.EXISTS:        0,
    ByteCode.ARGBOTTOM:     0,
    ByteCode.TESTARGBOTTOM: 0,
ByteCode.TESTCANCELLED: 0,

    # These only exist in the program temporarily
    # or in the ML file but never actually can be executed.
ByteCode.PUSHRELOCATELATER: 0,
    ByteCode.PUSHDELEGATERELOCATELATER: 2,
    ByteCode.LABELRESET: 1,
}

class ByteReader:
    def __init__(self, source: bytes | io.BytesIO) -> None:
        if isinstance(source, bytes):
            self.stream = io.BytesIO(source)
        else:
            self.stream = source

    def tell(self) -> int:
        return self.stream.tell() # type: ignore

    def seek(self, pos: int) -> None:
        self.stream.seek(pos) # type: ignore

    def read(self, size: int) -> bytes:
        data = self.stream.read(size) # type: ignore
        if len(data) != size:   # type: ignore
            raise EOFError(f"Unexpected EOF (wanted {size} bytes, got {len(data)})")    # type: ignore
        return data # type: ignore

    def read_bool(self) -> bool:
        return bool(self.read_byte())

    def read_byte(self) -> int:
        return struct.unpack("<B", self.read(1))[0]

    def read_i16(self) -> int:
        return struct.unpack("<h", self.read(2))[0]
    
    def read_i32(self) -> int:
        return struct.unpack("<i", self.read(4))[0]
        
    def read_u16(self) -> int:
        return struct.unpack("<H", self.read(2))[0]
    
    def read_u32(self) -> int:
        return struct.unpack("<I", self.read(4))[0]
      
    def read_float(self) -> float:
        return struct.unpack("<f", self.read(4))[0]
        
    def read_double(self) -> float:
        return struct.unpack("<d", self.read(8))[0]
    
    def read_string(self) -> str:
        length = self.read_byte()
        if length == 0:
            return ""
        data = self.read(length)
        return data.decode("utf-8")
  
    def eof(self) -> bool:
        cur = self.stream.tell()    # type: ignore
        self.stream.seek(0, io.SEEK_END)    # type: ignore
        end = self.stream.tell()    # type: ignore
        self.stream.seek(cur)       # type: ignore
        return cur >= end           # type: ignore

@dataclass
class Argument:
    type_id: KOSValueType
    data: Any
    
    def __init__(self, type_id: KOSValueType, data: Any) -> None:
        self.type_id = type_id
        self.data = data
    
    def __repr__(self) -> str:
        return f"{self.type_id.name}: {self.data}"

@dataclass
class Instruction:
    label: str
    opcode: int
    name: str
    args: List[int]

def main() -> None:
    data = decompress('D:/KSP Instances/Vs Valnuss/Ships/Script/compile_test.ksm')

    reader = ByteReader(data)
    next_hdr: bytes | None = None
    args, index_byte_width, next_hdr = parse_argument_section(reader)

    print("Parsed arguments:")
    for i, arg in args.items():
        print(f"{i:04x}: {arg!r}")

    print("Next section header:", next_hdr)

    instructions_all: List[Instruction] = []
    while next_hdr in {b'%F', b'%I', b'%M'}:
        instructions, next_hdr = parse_code_section(reader, index_byte_width)
        instructions_all.extend(instructions)

        print("Instructions:")
        for ins in instructions:
            print(f"{ins.label}: {ins.name} {ins.args}")
        
        print("Next section header:", next_hdr)
    
    disassemble(instructions_all, args, "disassembly.txt")

def decompress(ksm_path: str, out_path: str = "./raw.dksm") -> bytes:
    with gzip.open(ksm_path, mode='rb') as file:
        machine_code = file.read()
    with open(out_path, mode='wb') as out:
        out.write(machine_code)
    return machine_code

def read_typed_value(reader: ByteReader) -> Argument:
    """Reads one (type_id, data) pair."""
    type_id = reader.read_byte()
    try:
        t = KOSValueType(type_id)
    except ValueError:
        raise ValueError(f"Unknown type id: {type_id}")

    data: None | bool | int | float | str = None
    match t:
        case KOSValueType.PSEUDO_NULL:
            data = None
        case KOSValueType.BOOL:
            data = reader.read_bool()
        case KOSValueType.BYTE:
            data = reader.read_byte()
        case KOSValueType.INT16:
            data = reader.read_i16()
        case KOSValueType.INT32:
            data = reader.read_i32()
        case KOSValueType.FLOAT:
            data = reader.read_float()
        case KOSValueType.DOUBLE:
            data = reader.read_double()
        case KOSValueType.STRING:
            data = reader.read_string()
        case KOSValueType.SCALAR_INT:
            data = reader.read_i32()
        case KOSValueType.SCALAR_DOUBLE:
            data = reader.read_double()
        case KOSValueType.BOOLEAN_VALUE:
            data = reader.read_bool()
        case KOSValueType.STRING_VALUE:
            data = reader.read_string()
        case KOSValueType.ARG_MARKER:
            data = "<ARG_MARKER>"
        case _:
            raise NotImplementedError(f"Type {t.name} not yet implemented")
    
    return Argument(t, data)

def parse_argument_section(reader: ByteReader) -> Tuple[Dict[int, Argument], int, bytes]:
    """Parses the argument section and returns (arguments, next_header)"""

    # --- 1. Verify magic number ---
    magic = reader.read(4)
    if magic != MAGIC:
        raise ValueError(f"Bad magic number: {magic!r}")

    # --- 2. Read argument header ---
    hdr = reader.read(2)
    if hdr != b'%A':
        raise ValueError(f"Expected argument header %A, found {hdr!r}")

    # --- 3. Argument index byte width ---
    index_byte_width = reader.read_byte()

    args: Dict[int, Argument] = {}

    # --- 4. Read arguments until next section header ---
    while True:
        # Peek next two bytes — if it’s a section header, stop
        peek = reader.read(2)
        if peek in SECTION_HEADERS:
            # rewind so next parser can see it
            reader.seek(reader.tell() - 2)
            next_header = peek
            break

        # Otherwise, this is an argument — rewind to re-read type_id
        reader.seek(reader.tell() - 2)
        ptr = reader.tell() - 4
        # Read one typed argument
        value = read_typed_value(reader)
        args[ptr] = value

    return args, index_byte_width, next_header

def read_index(reader: ByteReader, width: int) -> int:
    """Read an integer index with specified byte width."""
    if width == 1:
        return reader.read_byte()
    elif width == 2:
        return reader.read_u16()
    elif width == 4:
        return reader.read_u32()
    else:
        raise ValueError(f"Unsupported index byte width: {width}")

def parse_code_section(reader: ByteReader, index_byte_width: int) -> Tuple[List[Instruction], Optional[bytes]]:
    """Parse one code section (%F / %I / %M). Returns (instructions, next_header)."""

    # --- 1. Read header ---
    header = reader.read(2)
    if header not in {b'%F', b'%I', b'%M'}:
        raise ValueError(f"Expected code section header, found {header!r}")

    instructions: List[Instruction] = []

    label_counter = 0
    while not reader.eof():
        # Peek next byte: could be '%' (next section)
        peek = reader.read_byte()
        if peek == ByteCode.DELIMITER.value:
            # Rewind and stop (let outer parser handle next section)
            reader.seek(reader.tell() - 1)
            next_header = reader.read(2)
            reader.seek(reader.tell() - 2)
            break
        else:
            # Rewind so opcode can be read normally
            reader.seek(reader.tell() - 1)

        opcode = reader.read_byte()
        try:
            bc = ByteCode(opcode)
        except ValueError:
            bc = ByteCode.BOGUS
        
        # Lookup info
        arg_count = ARG_COUNT.get(bc, 0)
        # Read opcode-specific arguments
        args = [read_index(reader, index_byte_width) for _ in range(arg_count)]

        instructions.append(Instruction(f"@{label_counter:04}", opcode, bc.name, args))

        label_counter += 1
    else:
        # hit EOF cleanly
        next_header = None

    return instructions, next_header

def format_argument(arg: Any) -> str:
    """Format argument for readable output."""
    if arg is None:
        return "NULL"
    elif isinstance(arg, str):
        # quote strings
        return f'"{arg}"'
    elif isinstance(arg, bool):
        return "TRUE" if arg else "FALSE"
    elif isinstance(arg, (int, float)):
        return str(arg)
    else:
        return repr(arg)
    
def disassemble(instructions: List[Instruction], args: Dict[int, Argument], output_path: str) -> None:
    """
    Writes a disassembly to a text file.
    Uses argument table for resolving opcode operands.
    """
    lines: List[str] = []
    lines.append(f"{'Offset':<8} {'Opcode':<12} Arguments")

    for ins in instructions:
        # Resolve each operand
        resolved: List[str] = []
        for operand in ins.args:
            if (resolved_val := args.get(operand)) is not None:
                resolved.append(format_argument(resolved_val.data))
            else:
                # if index is out of range, display numeric form
                resolved.append(f"#{operand}")

        # Construct line
        arg_str = ", ".join(resolved)
        lines.append(f"{ins.label:<8} {ins.name:<12} {arg_str}")

    # Write to file
    with open(output_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))

    print(f"Wrote disassembly to {output_path}")

main()
