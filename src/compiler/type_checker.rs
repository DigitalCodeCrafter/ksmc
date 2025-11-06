
// CURRENT GOAL: a typevisiter for primitive types and type inferrence
// primitive types include
// integers, floats, booleans, strings, arrays, tuples, unit, function

// Plan:
// Use Type variables for unknowns
// collect constraints
// unify vars and types

// exclude for now:
// generics, user types

// Validify:
// exactly one concrete type for every var
// evaluate types of all expressions
// bools in if statements
// function parameters
// mutability

// Algorithm:
// start at items
// for every node in worklist:
// - collect constraints
// - if a required var isn't found, add to worklist and reshcedule
// resolve constraints

use std::collections::HashMap;

use crate::compiler::{ast::{AST, BinaryOp, Literal, NodeId, NodeKind, UnaryOp}, resolver::{SymbolId, SymbolTable}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TypeVarId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ConstraintId(usize);

enum Type {
    Int,
    Float,
    Bool,
    String,
    Array { elem: Box<Type>, len: u32 },
    Tuple(Vec<Type>),
    Unit,
    Func { params: Vec<Type>, ret: Box<Type> }
}

enum TypeVarKind {
    Unknown
}

struct TypeVar {
    kind: TypeVarKind,
}

enum Constraint {
    // validate equality
    Equal(TypeVarId, TypeVarId),    // a == b
    // validate consistency
    Resolved(TypeVarId, Type),      // a == primitive
    // validate possibility
    BinaryOp { op: BinaryOp, lhs: TypeVarId, rhs: TypeVarId, out: TypeVarId },  // c == a (+ | > | && ...) b
    // valdidate possibility
    UnaryOp { op: UnaryOp, expr: TypeVarId, out: TypeVarId },                   // b == (! | -) a
    // validate indexable
    Indexing { indexee: TypeVarId, index: TypeVarId, out: TypeVarId },          // c == a[b]
    // validate tuple
    TupleIndexing { indexee: TypeVarId, index: u32, out: TypeVarId },           // c == a.idx
}

struct TypeChecker<'a> {
    ast: &'a AST,
    symbols: &'a SymbolTable,
    vars: Vec<TypeVar>,
    constraints: Vec<Constraint>,
    node_types: HashMap<NodeId, TypeVarId>,
    symbol_types: HashMap<SymbolId, TypeVarId>,

    worklist: Vec<NodeId>,
}
impl<'a> TypeChecker<'a> {
    fn new(ast: &'a AST, symbols: &'a SymbolTable) -> Self {
        Self {
            ast,
            symbols,
            vars: Vec::new(),
            constraints: Vec::new(),
            node_types: HashMap::new(),
            symbol_types: HashMap::new(),
            worklist: ast.items.clone(),
        }
    }

    fn new_var(&mut self) -> TypeVarId {
        let id = TypeVarId(self.vars.len());
        self.vars.push(TypeVar { kind: TypeVarKind::Unknown });
        id
    }

    fn push_constraint(&mut self, constraint: Constraint) -> ConstraintId {
        let id = ConstraintId(self.constraints.len());
        self.constraints.push(constraint);
        id
    }

    // bool for "try again later"
    fn visit_expr(&mut self, node_id: NodeId) -> Result<bool, String> {
        let Some(node) = self.ast.get(node_id) else { return Err(format!("{:?} does not exist", node_id)); };
        if !node.kind.is_expression() { return Err(format!("{:?} is not an expression", node.kind)); }

        // get or create var
        let var = self.node_types
            .get(&node_id)
            .copied()
            .unwrap_or_else(|| self.new_var());

        if match &node.kind {
            NodeKind::Literal(lit) => self.visit_literal(var, lit),
            NodeKind::Unary { op, expr } => self.visit_unary(var, *op, *expr),
            NodeKind::Binary { op, lhs, rhs } => self.visit_binary(var, *op, *lhs, *rhs),
            NodeKind::Call { callee, args } => self.visit_call(var, *callee, args),
            NodeKind::Block { nodes } => todo!("blocks"),
            NodeKind::If { cond, then_block, else_block } => todo!("if"),
            NodeKind::Loop { block } => todo!("if"),
            NodeKind::Return { expr } => todo!("return"),
            NodeKind::Break { expr } => todo!("break"),
            NodeKind::Tuple { elements } => todo!("tuple"),
            NodeKind::Array { elements } => todo!("array"),
            NodeKind::ArrayRepeat { value, count } => todo!("array"),
            NodeKind::IndexExpression { array, index } => todo!("index"),
            NodeKind::TupleIndexExpression { tuple, index } => todo!("tupel idx"),
            NodeKind::PathExpression { qself, segments } => todo!("vars"),
            _ => false,
        } {
            return Ok(true);
        }

        self.node_types.insert(node_id, var);
        Ok(false)
    }
}

impl<'a> TypeChecker<'a> {
    fn visit_literal(&mut self, var: TypeVarId, lit: &Literal) -> bool {
        let constraint = match lit {
            Literal::Bool(_) => Constraint::Resolved(var, Type::Bool),
            Literal::Int(_) => Constraint::Resolved(var, Type::Int),
            Literal::Float(_) => Constraint::Resolved(var, Type::Float),
            Literal::Str(_) => Constraint::Resolved(var, Type::String),
        };

        self.constraints.push(constraint);
        false
    }

    fn visit_unary(&mut self, var: TypeVarId, op: UnaryOp, expr: NodeId) -> bool {
        let ty = match self.node_types.get(&expr) {
            Some(ty) => *ty,
            None => {
                self.worklist.push(expr);
                return true;
            }
        };

        self.constraints.push(Constraint::UnaryOp { op, expr: ty, out: var });
        false
    }

    fn visit_binary(&mut self, var: TypeVarId, op: BinaryOp, lhs: NodeId, rhs: NodeId) -> bool {
        let lty = match self.node_types.get(&lhs) {
            Some(ty) => *ty,
            None => {
                self.worklist.push(lhs);
                return true;
            }
        };
        let rty = match self.node_types.get(&rhs) {
            Some(ty) => *ty,
            None => {
                self.worklist.push(rhs);
                return true;
            }
        };

        self.constraints.push(Constraint::BinaryOp { op, lhs: lty, rhs: rty, out: var });
        false
    }

    fn visit_call(&mut self, var: TypeVarId, calle: NodeId, args: &[NodeId]) -> bool {
        todo!("function calls")
        // plan: look up definition from symbol table and types
        // equal constraints for arg types -> arg = param
        // equal constraint for return type and var
    }
}
