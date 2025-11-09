
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone)]
pub struct AST {
    pub nodes: Vec<Node>,
    pub items: Vec<NodeId>,
}
impl AST {
    pub fn get(&self, node: NodeId) -> Option<&Node> {
        self.nodes.get(node.0)
    }

    pub fn get_mut(&mut self, node: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(node.0)
    }
}

// --- AST Nodes ---

#[derive(Debug, Clone)]
pub enum NodeKind {
    // Expressions
    Literal(Literal),
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
    IndexExpression { array: NodeId , index: NodeId },
    TupleIndexExpression { tuple: NodeId , index: u32 },
    PathExpression { qself: Option<NodeId>, segments: Vec<NodeId> },
    ErrorExpr,
    
    // Types
    TypePath { qself: Option<NodeId>, segments: Vec<NodeId> },
    TypeTuple { elements: Vec<NodeId> },
    TypeArray { ty: NodeId, len: NodeId },
    TypeSlice { ty: NodeId },
    ErrorType,
    
    PathSegment { ident: String, args: Vec<NodeId> },
    
    // Statements
    LetStmt { name: String, mutable: bool, ty: Option<NodeId>, value: Option<NodeId> },
    ExprStmt { expr: NodeId },
    EmptyStmt,
    
    // Items
    Function { public: bool, name: String, params: Vec<(String, NodeId)>, return_type: Option<NodeId>, body: NodeId },
    Module { public: bool, name: String, items: Option<Vec<NodeId>> },
    UseDecl { public: bool, use_tree: NodeId },
    
    UsePath { ident: String, tree: NodeId },
    UseGroup { trees: Vec<NodeId> },
    UseName { ident: String },
    UseRename { ident: String, name: String },
    UseGlob,
}
impl NodeKind {
    /// returns an iterator over the children of a node
    pub fn children(&self) ->impl Iterator<Item = NodeId> {
        use NodeKind::*;

        enum Children<'a> {
            None(std::iter::Empty<NodeId>),
            One(std::iter::Once<NodeId>),
            Two(std::array::IntoIter<NodeId, 2>),
            Three(std::array::IntoIter<NodeId, 3>),
            Slice(std::iter::Copied<std::slice::Iter<'a, NodeId>>),
            Chain(std::iter::Chain<std::iter::Once<NodeId>, std::iter::Copied<std::slice::Iter<'a, NodeId>>>),
            ChainOpt(std::iter::Chain<std::iter::Copied<std::option::Iter<'a, NodeId>>, std::iter::Copied<std::option::Iter<'a, NodeId>>>),
            FuncChain(
                std::iter::Chain<
                    std::iter::Chain<
                        std::iter::Once<NodeId>,
                        std::iter::Copied<std::option::Iter<'a, NodeId>>
                    >, 
                    std::iter::Map<
                        std::slice::Iter<'a, (String, NodeId)>,
                        fn(&(String, NodeId)) -> NodeId
                    >
                >
            ),
            PathChain(std::iter::Chain<std::iter::Copied<std::option::Iter<'a, NodeId>>, std::iter::Copied<std::slice::Iter<'a, NodeId>>>)
        }

        impl<'a> Iterator for Children<'a> {
            type Item = NodeId;
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Children::None(i) => i.next(),
                    Children::One(i) => i.next(),
                    Children::Two(i) => i.next(),
                    Children::Three(i) => i.next(),
                    Children::Slice(i) => i.next(),
                    Children::Chain(i) => i.next(),
                    Children::ChainOpt(i) => i.next(),
                    Children::FuncChain(i) => i.next(),
                    Children::PathChain(i) => i.next(),
                }
            }
        }

        match self {
            Unary { expr: id, .. }
            | Loop { block: id }
            | ExprStmt { expr: id }
            | UseDecl { use_tree: id , ..}
            | TupleIndexExpression { tuple: id , .. }
            | TypeSlice { ty: id }
            | Return { expr: Some(id) }
            | Break { expr: Some(id) }
            | UsePath {tree: id , ..} => Children::One(std::iter::once(*id)),

            Block { nodes: ids }
            | Tuple { elements: ids }
            | Array { elements: ids }
            | TypeTuple { elements: ids }
            | UseGroup { trees: ids }
            | Module { items: Some(ids) , .. }
            | PathSegment {args: ids , ..} => Children::Slice(ids.iter().copied()),

            Binary { lhs: id0, rhs: id1, .. }
            | ArrayRepeat { value: id0, count: id1 }
            | IndexExpression { array: id0 , index: id1 }
            | If { cond: id0, then_block: id1, else_block: None }
            | TypeArray { ty: id0, len: id1 } => Children::Two([*id0, *id1].into_iter()),
            
            Call { callee: id, args: ids } => Children::Chain(std::iter::once(*id).chain(ids.iter().copied())),
            
            PathExpression { qself: optid, segments: ids }
            | TypePath { qself: optid, segments: ids } => Children::PathChain(
                optid.iter().copied().chain(ids.iter().copied())
            ),

            If { cond, then_block, else_block: Some(else_block) } => Children::Three(
                [*cond, *then_block, *else_block].into_iter()
            ),
            
            LetStmt {ty: optid0, value: optid1 , .. } => Children::ChainOpt(
                optid0.iter().copied().chain(optid1.iter().copied())
            ),

            Function { params, return_type: optid, body: id, .. } => {
                let map_fn: fn(&(String, NodeId)) -> NodeId = |(_, id)| *id;
                Children::FuncChain(
                    std::iter::once(*id)
                        .chain(optid.iter().copied())
                        .chain(params.iter().map(map_fn))
                )
            }

            Return { expr: None }
            | Break { expr: None }
            | Module { items: None, .. }
            | Literal(_)
            | ErrorExpr
            | ErrorType
            | EmptyStmt
            | UseName { .. }
            | UseRename { .. }
            | UseGlob => Children::None(std::iter::empty()),
        }
    }

    /// returns an iterator over the children of a node for in place mutation
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut NodeId> {
        use NodeKind::*;

        enum ChildrenMut<'a> {
            None(std::iter::Empty<&'a mut NodeId>),
            One(std::iter::Once<&'a mut NodeId>),
            Two(std::array::IntoIter<&'a mut NodeId, 2>),
            Three(std::array::IntoIter<&'a mut NodeId, 3>),
            Slice(std::slice::IterMut<'a, NodeId>),
            Chain(std::iter::Chain<std::iter::Once<&'a mut NodeId>, std::slice::IterMut<'a, NodeId>>),
            ChainOpt(std::iter::Chain<std::option::IterMut<'a, NodeId>, std::option::IterMut<'a, NodeId>>),
            FuncChain(
                std::iter::Chain<
                    std::iter::Chain<
                        std::iter::Once<&'a mut NodeId>,
                        std::option::IterMut<'a, NodeId>
                    >, 
                    std::iter::Map<
                        std::slice::IterMut<'a, (String, NodeId)>,
                        fn(&'a mut (String, NodeId)) -> &'a mut NodeId
                    >
                >
            ),
            PathChain(std::iter::Chain<std::option::IterMut<'a, NodeId>,std::slice::IterMut<'a, NodeId>>)
        }

        impl<'a> Iterator for ChildrenMut<'a> {
            type Item = &'a mut NodeId;
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    ChildrenMut::None(i) => i.next(),
                    ChildrenMut::One(i) => i.next(),
                    ChildrenMut::Two(i) => i.next(),
                    ChildrenMut::Three(i) => i.next(),
                    ChildrenMut::Slice(i) => i.next(),
                    ChildrenMut::Chain(i) => i.next(),
                    ChildrenMut::ChainOpt(i) => i.next(),
                    ChildrenMut::FuncChain(i) => i.next(),
                    ChildrenMut::PathChain(i) => i.next(),
                }
            }
        }

        match self {
            Unary { expr: id, .. }
            | Loop { block: id }
            | ExprStmt { expr: id }
            | UseDecl { use_tree: id , ..}
            | TupleIndexExpression { tuple: id , .. }
            | TypeSlice { ty: id }
            | Return { expr: Some(id) }
            | Break { expr: Some(id) }
            | UsePath {tree: id , ..} => ChildrenMut::One(std::iter::once(id)),

            Block { nodes: ids }
            | Tuple { elements: ids }
            | Array { elements: ids }
            | TypeTuple { elements: ids }
            | UseGroup { trees: ids }
            | Module { items: Some(ids) , .. }
            | PathSegment {args: ids , ..} => ChildrenMut::Slice(ids.iter_mut()),

            Binary { lhs: id0, rhs: id1, .. }
            | ArrayRepeat { value: id0, count: id1 }
            | IndexExpression { array: id0 , index: id1 }
            | If { cond: id0, then_block: id1, else_block: None }
            | TypeArray { ty: id0, len: id1 } => ChildrenMut::Two([id0, id1].into_iter()),
            
            Call { callee: id, args: ids } => ChildrenMut::Chain(std::iter::once(id).chain(ids.iter_mut())),
            
            PathExpression { qself: optid, segments: ids }
            | TypePath { qself: optid, segments: ids } => ChildrenMut::PathChain(
                optid.iter_mut().chain(ids.iter_mut())
            ),

            If { cond, then_block, else_block: Some(else_block) } => ChildrenMut::Three(
                [cond, then_block, else_block].into_iter()
            ),
            
            LetStmt {ty: optid0, value: optid1 , .. } => ChildrenMut::ChainOpt(
                optid0.iter_mut().chain(optid1.iter_mut())
            ),

            Function { params, return_type: optid, body: id, .. } => {
                let map_fn: for<'a> fn(&'a mut (String, NodeId)) -> &'a mut NodeId = |(_, id)| id;
                ChildrenMut::FuncChain(
                    std::iter::once(id)
                        .chain(optid.iter_mut())
                        .chain(params.iter_mut().map(map_fn))
                )
            }

            Return { expr: None }
            | Break { expr: None }
            | Module { items: None, .. }
            | Literal(_)
            | ErrorExpr
            | ErrorType
            | EmptyStmt
            | UseName { .. }
            | UseRename { .. }
            | UseGlob => ChildrenMut::None(std::iter::empty()),
        }
    }

    pub fn is_expression(&self) -> bool {
        // exhaustive to make sure an error gets reported if a new node kind is added
        match self {
            NodeKind::Literal(_)
            | NodeKind::Unary { .. }
            | NodeKind::Binary { .. }
            | NodeKind::Call { .. }
            | NodeKind::Block { .. }
            | NodeKind::If { .. }
            | NodeKind::Loop { .. }
            | NodeKind::Return { .. }
            | NodeKind::Break { .. }
            | NodeKind::Tuple { .. }
            | NodeKind::Array { .. }
            | NodeKind::ArrayRepeat { .. }
            | NodeKind::IndexExpression { .. }
            | NodeKind::TupleIndexExpression { .. }
            | NodeKind::PathExpression { .. }
            | NodeKind::ErrorExpr => true,

            NodeKind::TypePath { .. }
            | NodeKind::TypeTuple { .. }
            | NodeKind::TypeArray { .. }
            | NodeKind::TypeSlice { .. }
            | NodeKind::ErrorType
            | NodeKind::PathSegment { .. }
            | NodeKind::LetStmt { .. }
            | NodeKind::ExprStmt { .. }
            | NodeKind::EmptyStmt
            | NodeKind::Function { .. }
            | NodeKind::Module { .. }
            | NodeKind::UseDecl { .. }
            | NodeKind::UsePath { .. }
            | NodeKind::UseGroup { .. }
            | NodeKind::UseName { .. }
            | NodeKind::UseRename { .. }
            | NodeKind::UseGlob => false,
        }
    }

    pub fn is_statement(&self) -> bool {
        // exhaustive to make sure an error gets reported if a new node kind is added
        match self {
            NodeKind::LetStmt { .. }
            | NodeKind::ExprStmt { .. }
            | NodeKind::EmptyStmt
            | NodeKind::Function { .. }
            | NodeKind::Module { .. }
            | NodeKind::UseDecl { .. } => true,

            NodeKind::Literal(_)
            | NodeKind::Unary { .. }
            | NodeKind::Binary { .. }
            | NodeKind::Call { .. }
            | NodeKind::Block { .. }
            | NodeKind::If { .. }
            | NodeKind::Loop { .. }
            | NodeKind::Return { .. }
            | NodeKind::Break { .. }
            | NodeKind::Tuple { .. }
            | NodeKind::Array { .. }
            | NodeKind::ArrayRepeat { .. }
            | NodeKind::IndexExpression { .. }
            | NodeKind::TupleIndexExpression { .. }
            | NodeKind::PathExpression { .. }
            | NodeKind::ErrorExpr
            | NodeKind::TypePath { .. }
            | NodeKind::TypeTuple { .. }
            | NodeKind::TypeArray { .. }
            | NodeKind::TypeSlice { .. }
            | NodeKind::ErrorType
            | NodeKind::PathSegment { .. }
            | NodeKind::UsePath { .. }
            | NodeKind::UseGroup { .. }
            | NodeKind::UseName { .. }
            | NodeKind::UseRename { .. }
            | NodeKind::UseGlob => false,
        }
    }

    pub fn is_type(&self) -> bool {
        // exhaustive to make sure an error gets reported if a new node kind is added
        match self {
            NodeKind::TypePath { .. }
            | NodeKind::TypeTuple { .. }
            | NodeKind::TypeArray { .. }
            | NodeKind::TypeSlice { .. }
            | NodeKind::ErrorType => true,

            NodeKind::Literal(_)
            | NodeKind::Unary { .. }
            | NodeKind::Binary { .. }
            | NodeKind::Call { .. }
            | NodeKind::Block { .. }
            | NodeKind::If { .. }
            | NodeKind::Loop { .. }
            | NodeKind::Return { .. }
            | NodeKind::Break { .. }
            | NodeKind::Tuple { .. }
            | NodeKind::Array { .. }
            | NodeKind::ArrayRepeat { .. }
            | NodeKind::IndexExpression { .. }
            | NodeKind::TupleIndexExpression { .. }
            | NodeKind::PathExpression { .. }
            | NodeKind::ErrorExpr
            | NodeKind::PathSegment { .. }
            | NodeKind::LetStmt { .. }
            | NodeKind::ExprStmt { .. }
            | NodeKind::EmptyStmt
            | NodeKind::Function { .. }
            | NodeKind::Module { .. }
            | NodeKind::UseDecl { .. }
            | NodeKind::UsePath { .. }
            | NodeKind::UseGroup { .. }
            | NodeKind::UseName { .. }
            | NodeKind::UseRename { .. }
            | NodeKind::UseGlob => false,
        }
    }

    pub fn is_item(&self) -> bool {
        // exhaustive to make sure an error gets reported if a new node kind is added
        match self {
            NodeKind::Function { .. }
            | NodeKind::Module { .. }
            | NodeKind::UseDecl { .. } => true,
            
            NodeKind::Literal(_)
            | NodeKind::Unary { .. }
            | NodeKind::Binary { .. }
            | NodeKind::Call { .. }
            | NodeKind::Block { .. }
            | NodeKind::If { .. }
            | NodeKind::Loop { .. }
            | NodeKind::Return { .. }
            | NodeKind::Break { .. }
            | NodeKind::Tuple { .. }
            | NodeKind::Array { .. }
            | NodeKind::ArrayRepeat { .. }
            | NodeKind::IndexExpression { .. }
            | NodeKind::TupleIndexExpression { .. }
            | NodeKind::PathExpression { .. }
            | NodeKind::ErrorExpr
            | NodeKind::TypePath { .. }
            | NodeKind::TypeTuple { .. }
            | NodeKind::TypeArray { .. }
            | NodeKind::TypeSlice { .. }
            | NodeKind::ErrorType
            | NodeKind::PathSegment { .. }
            | NodeKind::LetStmt { .. }
            | NodeKind::ExprStmt { .. }
            | NodeKind::EmptyStmt
            | NodeKind::UsePath { .. }
            | NodeKind::UseGroup { .. }
            | NodeKind::UseName { .. }
            | NodeKind::UseRename { .. }
            | NodeKind::UseGlob => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}
impl Node {
    /// get the children of a node
    pub fn children(&self) -> impl Iterator<Item = NodeId> {
        self.kind.children()
    }

    /// get the children of a node for in place mutation
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut NodeId>  {
        self.kind.children_mut()
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(u32),
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
    Add, Sub, Mul, Div,     // +, -, *, /
    Eq, Ne, Lt, Gt, Le, Ge, // ==, !=, <, >, <=, >=
    And, Or,                // &&, ||
    Assign,                 // =
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
