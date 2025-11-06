use crate::compiler::{CompilerError, ast::{AST, BinaryOp, Node, NodeId, NodeKind}};

#[derive(Debug)]
pub enum ValidationErrorKind {
    InvalidAssignmentLhs(NodeId),
    InvalidPathSegment(NodeId),
    InvalidUseSegment(NodeId),
    ModuleNotInlined(String),
    NotABlock(NodeId),
    NotAnExpression(NodeId),
    NotAType(NodeId),
    NotAStatement(NodeId),
    NotAnItem(NodeId),
    PlaceholderNode(NodeId),
    NamingViolation { name: String, expected: &'static str },
}

#[derive(Debug)]
pub struct ValidationError {
    pub node_id: NodeId,
    pub kind: ValidationErrorKind,
}
impl From<Vec<ValidationError>> for CompilerError {
    fn from(value: Vec<ValidationError>) -> Self {
        CompilerError::ValidationError(value)
    }
}

pub fn validate_all(ast: &AST) -> Result<(), Vec<ValidationError>> {
    let mut validator = AstValidator::new(ast);
    if validator.validate().is_ok() {
        Ok(())
    } else {
        Err(validator.errors)
    }
}

pub struct AstValidator<'a> {
    ast: &'a AST,
    errors: Vec<ValidationError>,
}
impl<'a> AstValidator<'a> {
    pub fn new(ast: &'a AST) -> Self {
        Self { ast, errors: Vec::new() }
    }

    pub fn validate(&mut self) -> Result<(), &[ValidationError]> {
        let mut worklist = self.ast.items.clone();

        for item in &self.ast.items {
            if !self.ast.nodes[item.0].kind.is_item() {
                self.errors.push(ValidationError {
                    node_id: *item,
                    kind: ValidationErrorKind::NotAnItem(*item),
                });
            }
        }

        while let Some(node_id) = worklist.pop() {
            let node = &self.ast.nodes[node_id.0];

            self.check_node(node_id, node);

            worklist.extend(node.children());
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(&self.errors)
        }
    }
}

impl<'a> AstValidator<'a> {
    fn check_node(&mut self, node_id: NodeId, node: &Node) {
        match &node.kind {
            NodeKind::Literal(_)
            | NodeKind::EmptyStmt
            | NodeKind::UseGlob
            | NodeKind::UseName { ident: _ }
            | NodeKind::UseRename { ident: _, name: _ } => {}
            NodeKind::ErrorExpr
            | NodeKind::ErrorType => {
                self.errors.push(ValidationError {
                    node_id,
                    kind: ValidationErrorKind::PlaceholderNode(node_id)
                });
            }
            NodeKind::Unary { expr, op: _ } => {
                self.check_expression(node_id, *expr);
            }
            NodeKind::Binary { op, lhs, rhs } => {
                self.check_expression(node_id, *lhs);
                self.check_expression(node_id, *rhs);

                if matches!(op, BinaryOp::Assign) {
                    if !self.is_valid_lhs(&self.ast.nodes[lhs.0]) {
                        self.errors.push(ValidationError {
                            node_id,
                            kind: ValidationErrorKind::InvalidAssignmentLhs(*lhs)
                        });
                    }
                }
            }
            NodeKind::Call { callee, args } => {
                self.check_expression(node_id, *callee);
                for arg in args {
                    self.check_expression(node_id, *arg);
                }
            }
            NodeKind::Block { nodes } => {
                if nodes.len() >= 2 {
                    for stmt in &nodes[..nodes.len() - 1] {
                        if !self.ast.nodes[stmt.0].kind.is_statement() {
                            self.errors.push(ValidationError {
                                node_id,
                                kind: ValidationErrorKind::NotAStatement(*stmt)
                            });
                        }
                    }
                }
                if let Some(last) = nodes.last() {
                    let node = &self.ast.nodes[last.0];
                    if !node.kind.is_expression() && !node.kind.is_statement() {
                        self.errors.push(ValidationError {
                            node_id,
                            kind: ValidationErrorKind::NotAnExpression(*last)
                        });
                        self.errors.push(ValidationError {
                            node_id,
                            kind: ValidationErrorKind::NotAStatement(*last)
                        });
                    }
                }
            }
            NodeKind::If { cond, then_block, else_block } => {
                self.check_expression(node_id, *cond);
                if !matches!(self.ast.nodes[then_block.0].kind, NodeKind::Block { .. }) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::NotABlock(*then_block)
                    });
                }
                if let Some(if_or_block) = else_block {
                    if !matches!(self.ast.nodes[if_or_block.0].kind, NodeKind::Block { .. } | NodeKind::If { .. }) {
                        self.errors.push(ValidationError {
                            node_id,
                            kind: ValidationErrorKind::NotABlock(*if_or_block)
                        });
                    }
                }
            }
            NodeKind::Loop { block } => {
                if !matches!(self.ast.nodes[block.0].kind, NodeKind::Block { .. }) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::NotABlock(*block)
                    });
                }
            }
            NodeKind::Return { expr }
            | NodeKind::Break { expr } => {
                if let Some(expr) = expr {
                    self.check_expression(node_id, *expr);
                }
            }
            NodeKind::Tuple { elements }
            | NodeKind::Array { elements } => {
                for expr in elements {
                    self.check_expression(node_id, *expr);
                }
            }
            NodeKind::ArrayRepeat { value, count } => {
                self.check_expression(node_id, *value);
                self.check_expression(node_id, *count);
            }
            NodeKind::IndexExpression { array, index } => {
                self.check_expression(node_id, *array);
                self.check_expression(node_id, *index);
            }
            NodeKind::TupleIndexExpression { tuple, index: _ } => {
                self.check_expression(node_id, *tuple);
            }
            NodeKind::PathExpression { qself, segments }
            | NodeKind::TypePath { qself, segments } => {
                for seg_id in segments {
                    if !matches!(self.ast.nodes[seg_id.0].kind, NodeKind::PathSegment { .. }) {
                        self.errors.push(ValidationError {
                            node_id,
                            kind: ValidationErrorKind::InvalidPathSegment(*seg_id),
                        });
                    }
                }
                if let Some(qself) = qself {
                    self.check_type(node_id, *qself);
                }
            }
            NodeKind::TypeTuple { elements } => {
                for ty_id in elements {
                    self.check_type(node_id, *ty_id);
                }
            }
            NodeKind::TypeArray { ty, len } => {
                self.check_type(node_id, *ty);
                self.check_expression(node_id, *len);
            }
            NodeKind::TypeSlice { ty } => {
                self.check_type(node_id, *ty);
            }
            NodeKind::PathSegment { args, ident: _ } => {
                for ty in args {
                    self.check_type(node_id, *ty);
                }
            }
            NodeKind::LetStmt { name, ty, value , mutable: _ } => {
                if !Self::is_snake_case(name) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::NamingViolation {
                            name: name.to_string(),
                            expected: "snake_case"
                        }
                    });
                }
                if let Some(ty) = ty {
                    self.check_type(node_id, *ty);
                }
                if let Some(expr) = value {
                    self.check_expression(node_id, *expr);
                }
            }
            NodeKind::ExprStmt { expr } => {
                self.check_expression(node_id, *expr);
            }
            NodeKind::Function { name, params, return_type, body, public: _ } => {
                if !Self::is_snake_case(name) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::NamingViolation {
                            name: name.to_string(),
                            expected: "snake_case"
                        }
                    });
                }
                if let Some(ty) = return_type {
                    self.check_type(node_id, *ty);
                }
                for (name, ty) in params {
                    if !Self::is_snake_case(name) {
                        self.errors.push(ValidationError {
                            node_id,
                            kind: ValidationErrorKind::NamingViolation {
                                name: name.to_string(),
                                expected: "snake_case"
                            }
                        });
                    }
                    self.check_type(node_id, *ty);
                }
                if !matches!(self.ast.nodes[body.0].kind, NodeKind::Block { .. }) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::NotABlock(*body)
                    });
                }
            }
            NodeKind::Module { name, items, public: _ } => {
                if !Self::is_snake_case(name) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::NamingViolation {
                            name: name.to_string(),
                            expected: "snake_case"
                        }
                    });
                }
                if let Some(items) = items {
                    for item in items {
                        if !self.ast.nodes[item.0].kind.is_item() {
                            self.errors.push(ValidationError {
                                node_id,
                                kind: ValidationErrorKind::NotAnItem(*item),
                            });
                        }
                    }
                } else {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::ModuleNotInlined(name.clone())
                    });
                }
            }
            NodeKind::UseDecl { use_tree, public: _ } => {
                if !matches!(
                    self.ast.nodes[use_tree.0].kind,
                    NodeKind::UsePath { .. } |
                    NodeKind::UseGroup { .. } |
                    NodeKind::UseName { .. } |
                    NodeKind::UseRename { .. } |
                    NodeKind::UseGlob
                ) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::InvalidUseSegment(*use_tree)
                    });
                }
            }
            NodeKind::UsePath { tree, ident: _ } => {
                if !matches!(
                    self.ast.nodes[tree.0].kind,
                    NodeKind::UsePath { .. } |
                    NodeKind::UseGroup { .. } |
                    NodeKind::UseName { .. } |
                    NodeKind::UseRename { .. } |
                    NodeKind::UseGlob
                ) {
                    self.errors.push(ValidationError {
                        node_id,
                        kind: ValidationErrorKind::InvalidUseSegment(*tree)
                    });
                }
            }
            NodeKind::UseGroup { trees } => {
                for tree in trees {
                    if !matches!(
                        self.ast.nodes[tree.0].kind,
                        NodeKind::UsePath { .. } |
                        NodeKind::UseGroup { .. } |
                        NodeKind::UseName { .. } |
                        NodeKind::UseRename { .. } |
                        NodeKind::UseGlob
                    ) {
                        self.errors.push(ValidationError {
                            node_id,
                            kind: ValidationErrorKind::InvalidUseSegment(*tree)
                        });
                    }
                }
            }
        }
    }

    fn check_expression(&mut self, node_id: NodeId, check: NodeId) {
        if !self.ast.nodes[check.0].kind.is_expression() {
            self.errors.push(ValidationError {
                node_id,
                kind: ValidationErrorKind::NotAnExpression(check)
            });
        }
    }

    fn check_type(&mut self, node_id: NodeId, check: NodeId) {
        if !self.ast.nodes[check.0].kind.is_type() {
            self.errors.push(ValidationError {
                node_id,
                kind: ValidationErrorKind::NotAType(check)
            });
        }
    }
}

impl<'a> AstValidator<'a> {
    fn is_valid_lhs(&self, node: &Node) -> bool {
        matches!(
            node.kind,
            NodeKind::PathExpression { .. } |       // variable
            NodeKind::IndexExpression { .. } |      // write into array
            NodeKind::TupleIndexExpression { .. }   // write into tuple
        )
    }

    fn is_snake_case(name: &str) -> bool {
        !name.is_empty() && name.chars().all(|c| c.is_lowercase() || c == '_' || c.is_numeric())
    }

    fn is_pascal_case(name: &str) -> bool {
        !name.is_empty()
            && name.chars().next().unwrap().is_uppercase()
            && !name.contains('_')
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_names_and_assign() {
        let source = r#"
        fn main() {
            fn BadName() {}
            let MyVar = 42;
            let x = (); let y = ();
            x + 5 = y; // invalid assignment
        }
        "#;

        let tokens = super::super::lexer::lex_all(source).unwrap();
        let ast = super::super::parser::parse_all(tokens).unwrap();
        let mut validator = AstValidator::new(&ast);
        let errors = validator.validate().unwrap_err();
        assert_eq!(errors.len(), 3);
        for e in errors {
            println!("{:?}", e);
        }
    }
}