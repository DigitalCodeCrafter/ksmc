
// --- AST Nodes ---

pub type NodeId = usize;

#[derive(Debug, Clone)]
pub enum NodeKind {
    // Expressions
    Literal(Literal),
    Variable(String),
    Unary { op: UnaryOp, expr: NodeId },
    Binary { op: BinaryOp, lhs: NodeId, rhs: NodeId },
    Call { callee: NodeId, args: Vec<NodeId> },
    Block { nodes: Vec<NodeId> },
    If { cond: NodeId, then_block: NodeId, else_block: Option<NodeId> },
    Loop { block: NodeId },
    Return { expr: Option<NodeId> },
    Break { expr: Option<NodeId> }, // labels?
    Tuple { elements: Vec<NodeId> },
    Array { elements: Vec<NodeId> },
    ArrayRepeat { value: NodeId, count: NodeId },
    UnderscoreExpr,
    IndexExpression { array: NodeId , index: NodeId },
    TupleIndexExpression { tuple: NodeId , index: i32 },
    
    // Statements
    LetStmt { name: String, mutable: bool, ty: Option<TypeId>, value: Option<NodeId> },
    ExprStmt { expr: NodeId },

    // Items
    Function { public:  bool, name: String, params: Vec<(String, TypeId)>, return_type: Option<TypeId>, body: NodeId },
    Module { public:  bool, name: String, items: Vec<NodeId> },
    UseDecl { public:  bool,  },

    // Program
    Program { items: Vec<NodeId> },
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(String),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,    // -
    Not,    // !
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add, Sub, Mul, Div,
    Eq, Ne, Lt, Gt, Le, Ge,
    And, Or,
    Assign,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}
impl Span {
    pub fn empty() -> Self {
        Self {
            start: Pos { line: 0, col: 0 },
            end: Pos { line: 0, col: 0 }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

// --- Types ---

pub type TypeId = usize;

#[derive(Debug, Clone)]
pub enum TypeKind {
    Simple(String),
    Generic {
        base: String,
        args: Vec<TypeId>,
    },
    Tuple(Vec<TypeId>),
    Array {
        ty: TypeId,
        len: Option<u32>,
    },
    Function {
        params: Vec<TypeId>,
        ret: Option<TypeId>,
    },
}
