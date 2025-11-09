
// CURRENT GOAL: a type checker for primitive types and type inferrence
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


use std::collections::HashMap;

use crate::compiler::{ast::{AST, BinaryOp, Literal, NodeId, NodeKind, UnaryOp}, resolver::{SymbolId, SymbolKind, SymbolTable}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TypeVarId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ConstraintId(usize);

#[derive(Debug, Clone)]
enum Type {
    Int,
    Float,
    Bool,
    String,
    Array { elem: Box<Type>, len: u32 },
    Tuple(Vec<Type>),
    Unit,
    Func { def: SymbolId }
}

enum TypeVarKind {
    Unknown,
    Resolved(Type),
    Error,
}

struct TypeVar {
    kind: TypeVarKind,
}

enum Constraint {
    // validate equality
    Equal(TypeVarId, TypeOrVar),    // a == b
    // validate possibility
    BinaryOp { op: BinaryOp, lhs: TypeVarId, rhs: TypeVarId, out: TypeVarId },  // c == a (+ | > | && ...) b
    // valdidate possibility
    UnaryOp { op: UnaryOp, expr: TypeVarId, out: TypeVarId },                   // b == (! | -) a
    // validate indexable
    Indexing { indexee: TypeVarId, index: TypeVarId, out: TypeVarId },          // c == a[b]
    // validate args and callabel
    Callable { callee: TypeVarId, call_result: TypeVarId, args: Vec<TypeVarId> }
}
enum TypeOrVar {
    Var(TypeVarId),
    Type(Type),
}

struct TypeChecker<'a> {
    ast: &'a AST,
    symbols: &'a SymbolTable,
    vars: Vec<TypeVar>,
    constraints: Vec<Constraint>,
    node_types: HashMap<NodeId, TypeVarId>,
    symbol_types: HashMap<SymbolId, TypeVarId>,

    errors: Vec<String>,
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

            errors: Vec::new(),
        }
    }

    fn check_all(&mut self) {

    }

    fn new_var(&mut self) -> TypeVarId {
        let id = TypeVarId(self.vars.len());
        self.vars.push(TypeVar { kind: TypeVarKind::Unknown });
        id
    }

    fn new_unit(&mut self) -> TypeVarId {
        let id = TypeVarId(self.vars.len());
        self.vars.push(TypeVar { kind: TypeVarKind::Resolved(Type::Unit) });
        id
    }

    fn new_error(&mut self) -> TypeVarId {
        let id = TypeVarId(self.vars.len());
        self.vars.push(TypeVar { kind: TypeVarKind::Error });
        id
    }

    fn push_constraint(&mut self, constraint: Constraint) -> ConstraintId {
        let id = ConstraintId(self.constraints.len());
        self.constraints.push(constraint);
        // insert unification here
        id
    }

    fn error(&mut self, var: TypeVarId, msg: String) {
        self.errors.push(msg);
        self.vars.get_mut(var.0).map(|v| v.kind = TypeVarKind::Error);
    }

}

impl<'a> TypeChecker<'a> {
    fn check_expr(&mut self, node_id: NodeId) -> Result<TypeVarId, String> {
        let Some(node) = self.ast.get(node_id) else { return Err(format!("{:?} does not exist", node_id)); };
        if !node.kind.is_expression() { return Err(format!("{:?} is not an expression", node)); };

        let var = self.node_types
            .get(&node_id)
            .copied()
            .unwrap_or_else(|| self.new_var());

        match &node.kind {
            NodeKind::Literal(lit) => self.check_lit(var, lit),
            NodeKind::Unary { op, expr } => self.check_unary(var, *op, *expr),
            NodeKind::Binary { op, lhs, rhs } => self.check_binary(var, *op, *lhs, *rhs),
            NodeKind::Call { callee, args } => self.check_call(var, *callee, args),
            NodeKind::Block { nodes } => todo!("blocks"),
            NodeKind::If { cond, then_block, else_block } => todo!("if"),
            NodeKind::Loop { block } => todo!("loops"),
            NodeKind::Return { expr } => todo!("returns"),
            NodeKind::Break { expr } => todo!("breaks"),
            NodeKind::Tuple { elements } => todo!("tuples"),
            NodeKind::Array { elements } => todo!("arrays"),
            NodeKind::ArrayRepeat { value, count } => todo!("arrays"),
            NodeKind::IndexExpression { array, index } => todo!("indexing"),
            NodeKind::TupleIndexExpression { tuple, index } => todo!("indexing"),
            NodeKind::PathExpression { qself, segments } => todo!("paths"),
            NodeKind::ErrorExpr => {
                self.error(var, format!("{:?} is an invalid expression", node_id));
            }
            _ => {}
        }

        self.node_types.insert(node_id, var);
        Ok(var)
    }

    fn check_lit(&mut self, var: TypeVarId, lit: &Literal) {
        let ty = match lit {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
            Literal::Str(_) => Type::String,
        };

        self.push_constraint(Constraint::Equal(var, TypeOrVar::Type(ty)));
    }

    fn check_unary(&mut self, var: TypeVarId, op: UnaryOp, expr: NodeId) {
        let evar = match self.check_expr(expr) {
            Ok(var) => var,
            Err(e) => {
                self.error(var, e);
                return;
            }
        };

        self.push_constraint(Constraint::UnaryOp { op, expr: evar, out: var });
    }

    fn check_binary(&mut self, var: TypeVarId, op: BinaryOp, lhs: NodeId, rhs: NodeId) {
        let lvar = match self.check_expr(lhs) {
            Ok(var) => var,
            Err(e) => {
                self.error(var, e);
                return;
            }
        };
        let rvar = match self.check_expr(rhs) {
            Ok(var) => var,
            Err(e) => {
                self.error(var, e);
                return;
            }
        };

        self.push_constraint(Constraint::BinaryOp { op, lhs: lvar, rhs: rvar, out: var });
    }

    fn check_call(&mut self, var: TypeVarId, callee: NodeId, args: &[NodeId]) {
        // expr type = function return type
        // get function from callees resolved type
        // resolve arg types = param types

        let callee_var = match self.check_expr(callee) {
            Ok(var) => var,
            Err(e) => {
                self.error(var, e);
                return;
            }
        };

        let mut arg_vars = Vec::with_capacity(args.len());
        for arg in args {
            match self.check_expr(*arg) {
                Ok(var) => arg_vars.push(var),
                Err(e) => {
                    self.error(var, e);
                    return;
                }
            }
        }

        self.push_constraint(Constraint::Callable {
            callee: callee_var,
            call_result: var,
            args: arg_vars,
        });
    }

    fn check_block(&mut self, node: NodeId) {

    }
}

impl<'a> TypeChecker<'a> {  
    fn try_resolve_constraint(&mut self, _constraint: ConstraintId) -> Result<bool, String> {
        
        todo!("constraint resolution")
    }
}