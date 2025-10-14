from enum import Enum, IntEnum

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

class ArgMarker:
    ...

class KOSTypeToPython(Enum):
    PSEUDO_NULL = None
    BOOL = bool
    BYTE = int
    INT16 = int
    INT32 = int
    FLOAT = float
    DOUBLE = float
    STRING = str
    ARG_MARKER = ArgMarker
    SCALAR_INT = int
    SCALAR_DOUBLE = float
    BOOLEAN_VALUE = bool
    STRING_VALUE = str

class Argument[T]:
    type_id: KOSValueType
    data: T

class KSMInterpreter:
    def __init__(self, instructions, labels, args) -> None:
        self.instructions = instructions
        self.labels = labels
        self.args = args
        self.stack = []
        self.scopes = [{}]
        self.ip = 0
        self.running = True
    
    def step(self):
        ins = self.instructions[self.ip]
        match ins:
            
        
    def run(self):
        while self.running and self.ip < len(self.instructions):
            self.step()

    def resolve_arg(self, address):
        return self.args[address]
