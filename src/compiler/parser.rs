use super::lexer::{Token, TokenKind};
use super::ast::*;


#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
    pub expected: Vec<TokenKind>, // optional
}
impl ParseError {
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            expected: vec![]
        }
    }

    pub fn expected(span: Span, expected: Vec<TokenKind>, found: TokenKind) -> Self {
        Self {
            span,
            message: format!("expected one of {:?}, found {:?}", expected, found),
            expected,
        }
    }
}
type PResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    arena: Vec<Node>,
    types: Vec<TypeKind>,
    pos_stack: Vec<Pos>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            arena: Vec::new(),
            types: Vec::new(),
            pos_stack: Vec::new(),
        }
    }

    pub fn parse_program(&mut self) -> Result<NodeId, Vec<ParseError>> {
        let mut items = Vec::new();
        let mut errors = Vec::new();

        self.start_span();
        while !matches!(self.peek(), TokenKind::EOF) {
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(err) => {
                    errors.push(err);
                    self.syncronize();
                }
            }
        }
        
        let program = match self.push_node(NodeKind::Program { items }) {
            Ok(program) => Some(program),
            Err(err) => {
                errors.push(err);
                None
            }
        };

        if errors.is_empty() {
            Ok(program.unwrap())
        } else {
            Err(errors)
        }
    }

    fn syncronize(&mut self) {
        // try to recover by going to a seperator and dropping all lower spans.
        while !matches!(self.peek(), TokenKind::EOF | TokenKind::Semi | TokenKind::RBrace) {
            self.next();
        }
        while self.pos_stack.len() > 1 {
            self.pos_stack.pop().unwrap();
        }
        self.next();
    }

    fn peek(&self) -> &TokenKind {
        self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::EOF)
    }

    fn peek_with_span(&self) -> (&TokenKind, Span) {
        self.tokens
            .get(self.pos)
            .map(|t| (&t.kind, t.span))
            .unwrap_or((&TokenKind::EOF, Span::empty()))
    }

    fn next(&mut self) -> &TokenKind {
        let tok =  self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::EOF);
        self.pos += 1;
        tok
    }

    fn next_with_span(&mut self) -> (&TokenKind, Span) {
        let ts =  self.tokens
            .get(self.pos)
            .map(|t| (&t.kind, t.span))
            .unwrap_or((&TokenKind::EOF, Span::empty()));
        self.pos += 1;
        ts
    }

    fn start_span(&mut self) {
        self.pos_stack.push(self.tokens.get(self.pos).map(|t| t.span.start).unwrap_or(Pos { line: 0, col: 0 }));
    }

    fn end_span(&mut self) -> PResult<Span> {
        let Some(start) = self.pos_stack.pop() else {
            return Err(ParseError::new(Span::empty(), "No spans open to close"));
        };
        let end = self.tokens.get(self.pos - 1).map(|t| t.span.end).unwrap_or(Pos { line: 0, col: 0 });
        Ok(Span { start, end })
    }

    fn expect(&mut self, kind: TokenKind) -> PResult<()> {
        let (tok, span) = self.next_with_span();
        if std::mem::discriminant(tok) == std::mem::discriminant(&kind) {
            Ok(())
        } else {
            Err(ParseError::new(span, format!("Expected {:?}, found {:?}", kind, tok)))
        }
    }

    fn push_node(&mut self, kind: NodeKind) -> PResult<NodeId> {
        let id = self.arena.len();
        let span = self.end_span()?;
        self.arena.push(Node { kind, span });
        Ok(id)
    }

    fn push_type(&mut self, kind: TypeKind) -> TypeId {
        let id = self.types.len();
        self.types.push(kind);
        id
    }
}

// --- Items ---

impl Parser {
    fn parse_item(&mut self) -> PResult<NodeId> {
        let public = match self.peek() {
            TokenKind::Pub => {
                self.next();
                true
            },
            _ => false
        };
        
        match self.peek_with_span() {
            (TokenKind::Mod, _) => self.parse_module(public),
            (TokenKind::Use, _) => self.parse_use(public),
            (TokenKind::Fn, _) => self.parse_function(public),
            (other, span) => Err(ParseError::expected(span, vec![TokenKind::Fn], other.clone())),
        }
    }

    fn parse_function(&mut self, public: bool) -> PResult<NodeId> {
        self.start_span();
        self.expect(TokenKind::Fn)?;

        let name = match self.next_with_span() {
            (TokenKind::Identifier(name), _) => name.clone(),
            (other, span) => return Err(ParseError::new(span, format!("expected function name, found {:?}", other))),
        };

        let params = self.parse_parameters()?;

        let return_type = if matches!(self.peek(), TokenKind::Arrow) {
            self.next();
            Some(self.parse_type()?)
        } else { None };

        let body = self.parse_block_expression()?;

        self.push_node(NodeKind::Function { public, name, params, return_type, body })
    }

    fn parse_parameters(&mut self) -> PResult<Vec<(String, TypeId)>> {
        let mut params = Vec::new();
        self.expect(TokenKind::LParen)?;

        if !matches!(self.peek(), TokenKind::RParen) {
            loop {
                let name = match self.next_with_span() {
                    (TokenKind::Identifier(name), _) => name.clone(),
                    (other, span) => return Err(ParseError::new(span, format!("expected parameter name, found {:?}", other))),
                };
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type()?;
                params.push((name, ty));

                if matches!(self.peek(), TokenKind::Comma) {
                    self.next();
                } else {
                    break;
                }
                if matches!(self.peek(), TokenKind::RParen) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RParen)?;
        Ok(params)
    }

    fn parse_module(&mut self, public: bool) -> PResult<NodeId> {
        self.start_span();
        self.expect(TokenKind::Mod)?;

        let name = match self.next_with_span() {
            (TokenKind::Identifier(name), _) => name.clone(),
            (other, span) => return Err(ParseError::new(span, format!("expected module name, found {:?}", other))),
        };

        let mut items = Vec::new();

        match self.next_with_span() {
            (TokenKind::Semi, _) => {},
            (TokenKind::LBrace, _) => {
                while !matches!(self.peek(), TokenKind::EOF) {
                    items.push(self.parse_item()?)
                }
                self.expect(TokenKind::RBrace)?;
            }

            (other, span) => return Err(ParseError::expected(span, vec![TokenKind::Semi, TokenKind::LBrace], other.clone())),
        }

        self.push_node(NodeKind::Module { public, name, items })
    }

    fn parse_use(&mut self, public: bool) -> PResult<NodeId> {
        self.start_span();
        self.expect(TokenKind::Use)?;

        // Use tree
        todo!();

        self.expect(TokenKind::Semi)?;

        self.push_node(NodeKind::UseDecl { public })
    }
}

// --- Statements ---

impl Parser {
    fn parse_statement(&mut self) -> PResult<NodeId> {
        match self.peek() {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Fn | TokenKind::Use | TokenKind::Mod | TokenKind::Pub => self.parse_item(),
            _ => {
                self.start_span();
                let expr = self.parse_expression(0)?;
                // optional ';' if end of block
                if matches!(self.peek(), TokenKind::RBrace | TokenKind::EOF) {
                    if matches!(self.peek(), TokenKind::Semi) {
                        self.next();
                        self.push_node(NodeKind::ExprStmt { expr })
                    } else {
                        self.end_span()?; // manualy discard, as theres no node to write to.
                        Ok(expr)
                    }
                } else {
                    self.expect(TokenKind::Semi)?;
                    self.push_node(NodeKind::ExprStmt { expr })
                }
            }
        }
    }

    fn parse_let_statement(&mut self) -> PResult<NodeId> {
        self.start_span();
        self.next();

        let mutable = matches!(self.peek(), TokenKind::Mut);
        if mutable { self.next(); }

        let name = match self.next_with_span() {
            (TokenKind::Identifier(name), _) => name.clone(),
            (other, span) => return Err(ParseError::new(span, format!("Expected identifier after let, found {:?}", other))),
        };

        let ty = if matches!(self.peek(), TokenKind::Colon) {
            self.next();
            Some(self.parse_type()?)
        } else { None };

        let value = if matches!(self.peek(), TokenKind::Eq) {
            self.next();
            Some(self.parse_expression(0)?)
        } else { None };

        self.expect(TokenKind::Semi)?;

        self.push_node(NodeKind::LetStmt { name, mutable, ty, value })
    }
}

// --- Expressions ---

impl Parser {
    fn parse_expression(&mut self, min_bp: u8) -> PResult<NodeId> {
        let start = self.tokens[self.pos].span.start;
        let mut lhs = self.parse_prefix()?;

        loop {
            let op = self.peek().clone();
            
            if let Some(expr) = self.try_parse_postfix(&op, lhs, start)? {
                lhs = expr;
                continue;
            }

            if !is_infix_operator(&op) {
                break;
            }
            
            let (lbp, rbp) = infix_binding_power(&op);
            if lbp < min_bp {
                break;
            }
            
            self.next(); // consume operator
            let rhs = self.parse_expression(rbp)?;
            
            self.pos_stack.push(start);
            lhs = self.push_node(NodeKind::Binary {
                op: token_to_binary_op(&op),
                lhs, 
                rhs 
            })?;
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> PResult<NodeId> {
        match self.peek_with_span() {
            // Literals
            (TokenKind::Int(v), _) => {
                let kind = NodeKind::Literal(Literal::Int(*v));
                self.start_span();
                self.next();
                self.push_node(kind)
            }
            (TokenKind::Float(v), _) =>{
                let kind = NodeKind::Literal(Literal::Float(*v));
                self.start_span();
                self.next();
                self.push_node(kind)
            }
            (TokenKind::True, _) => {
                let kind = NodeKind::Literal(Literal::Bool(true));
                self.start_span();
                self.next();
                self.push_node(kind)
            }
            (TokenKind::False, _) => {
                let kind = NodeKind::Literal(Literal::Bool(false));
                self.start_span();
                self.next();
                self.push_node(kind)
            }
            (TokenKind::Identifier(name), _) => {
                let kind = NodeKind::Variable(name.clone());
                self.start_span();
                self.next();
                self.push_node(kind)
            }
            (TokenKind::Underscore, _) => {
                self.start_span();
                self.next();
                self.push_node(NodeKind::UnderscoreExpr)
            }

            // Unary ops
            (TokenKind::Minus | TokenKind::Not, _) => {
                let op = token_to_unary_op(self.peek());
                self.start_span();
                self.next();
                let rhs = self.parse_expression(14)?;
                self.push_node(NodeKind::Unary { op, expr: rhs })
            }

            (TokenKind::LBracket, _) => self.parse_array_expression(),

            // Tuple or grouped expressions
            (TokenKind::LParen, _) => {
                self.start_span();
                self.next();
                if matches!(self.peek(), TokenKind::RParen) {
                    self.next();
                    return self.push_node(NodeKind::Tuple { elements: vec![]});
                }

                let first = self.parse_expression(0)?;

                match self.peek_with_span() {
                    (TokenKind::Comma, _) => {
                        // tuple
                        let mut elements = vec![first];
                        while matches!(self.peek(), TokenKind::Comma) {
                            self.next();
                            if matches!(self.peek(), TokenKind::Comma) {
                                break;
                            }
                            elements.push(self.parse_expression(0)?);
                        }
                        self.expect(TokenKind::RParen)?;
                        self.push_node(NodeKind::Tuple { elements })
                    }

                    (TokenKind::RParen, _) => {
                        // grouped expression
                        self.next();
                        Ok(first)
                    }

                    (other, span) => return Err(ParseError::expected(span, vec![TokenKind::Comma, TokenKind::RParen], other.clone())),
                }
            }

            // if expression
            (TokenKind::If, _) => self.parse_if_expression(),

            // Loop expression
            (TokenKind::Loop, _) => self.parse_loop_expression(),

            // Block expression
            (TokenKind::LBrace, _) => self.parse_block_expression(),

            // break expression
            (TokenKind::Break, _) => {
                self.start_span();
                self.next();
                if matches!(self.peek(), TokenKind::Semi | TokenKind::RBrace | TokenKind::EOF) {
                    self.push_node(NodeKind::Break { expr: None })
                } else {
                    let expr = self.parse_expression(0)?;
                    self.push_node(NodeKind::Break { expr: Some(expr) })
                }
            }

            // return expression
            (TokenKind::Return, _) => {
                self.start_span();
                self.next();
                if matches!(self.peek(), TokenKind::Semi | TokenKind::RBrace | TokenKind::EOF) {
                    self.push_node(NodeKind::Return { expr: None })
                } else {
                    let expr = self.parse_expression(0)?;
                    self.push_node(NodeKind::Return { expr: Some(expr) })
                }
            }

            (other, span) => return Err(ParseError::new(span, format!("Unexpected Token in prefix: {:?}", other))),
        }
    }

    fn try_parse_postfix(&mut self, op: &TokenKind, lhs: NodeId, start: Pos) -> PResult<Option<NodeId>> {
        // Function call
        if matches!(op, TokenKind::LParen) {
            self.next();
            
            let mut args = Vec::new();
            if !matches!(self.peek(), TokenKind::RParen) {
                loop {
                    let arg = self.parse_expression(0)?;
                    args.push(arg);
                    if matches!(self.peek(), TokenKind::Comma) {
                        self.next();
                    } else {
                        break;
                    }
                    if matches!(self.peek(), TokenKind::RParen) {
                        break;
                    }
                }
            }
            self.expect(TokenKind::RParen)?;
            
            self.pos_stack.push(start);
            return Ok(Some(self.push_node(NodeKind::Call { callee: lhs, args })?));
        }
        // array indexing
        if matches!(op, TokenKind::LBracket) {
            self.next();
            
            let index = self.parse_expression(0)?;
            
            self.expect(TokenKind::RBracket)?;
            
            self.pos_stack.push(start);
            return Ok(Some(self.push_node(NodeKind::IndexExpression { array: lhs, index })?));
        }
        // tuple indexing / field access
        if matches!(op, TokenKind::Dot) {
            self.next();
            
            match self.next_with_span() {
                (TokenKind::Identifier(_field), _) => todo!("Field Access is not yet implemented"),
                (TokenKind::Int(i), _) => {
                    let index = *i;
                    self.pos_stack.push(start);
                    return Ok(Some(self.push_node(NodeKind::TupleIndexExpression { tuple: lhs, index } )?));
                }
                (other, span) => return Err(ParseError::new(span, format!("expected field name or tuple index, found {:?}", other))),
            }
        }

        Ok(None)
    }

    fn parse_if_expression(&mut self) -> PResult<NodeId> {
        self.start_span();
        self.expect(TokenKind::If)?;
        let cond = self.parse_expression(0)?;
        let then_block = self.parse_block_expression()?;
        let else_block = if matches!(self.peek(), TokenKind::Else) {
            self.next();
            if matches!(self.peek(), TokenKind::If) {
                Some(self.parse_if_expression()?)
            } else {
                Some(self.parse_block_expression()?)
            }
        } else {
            None
        };

        self.push_node(NodeKind::If { cond, then_block, else_block })
    }

    fn parse_loop_expression(&mut self) -> PResult<NodeId> {
        self.start_span();
        self.expect(TokenKind::Loop)?;

        let block = self.parse_block_expression()?;

        self.push_node(NodeKind::Loop { block })
    }

    fn parse_block_expression(&mut self) -> PResult<NodeId> {
        self.start_span();
        self.expect(TokenKind::LBrace)?;
        let mut nodes = Vec::new();
        
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::EOF) {
            nodes.push(self.parse_statement()?);
        }

        self.expect(TokenKind::RBrace)?;

        self.push_node(NodeKind::Block { nodes })
    }

    fn parse_array_expression(&mut self) -> PResult<NodeId> {
        self.start_span();
        self.expect(TokenKind::LBracket)?;
        if matches!(self.peek(), TokenKind::RBracket) {
            self.next();
            return self.push_node(NodeKind::Array { elements: vec![] });
        }

        let first = self.parse_expression(0)?;

        match self.peek_with_span() {
            (TokenKind::Semi, _) => {
                // repetition: [ value ; count ]
                self.next();
                let count = self.parse_expression(0)?;
                self.expect(TokenKind::RBracket)?;
                self.push_node(NodeKind::ArrayRepeat { value: first, count })
            }

            (TokenKind::Comma, _) => {
                // comma separated list
                let mut elements = vec![first];
                while matches!(self.peek(), TokenKind::Comma) {
                    self.next();
                    if matches!(self.peek(), TokenKind::RBracket) {
                        break;
                    }
                    elements.push(self.parse_expression(0)?);
                }
                self.expect(TokenKind::RBracket)?;
                self.push_node(NodeKind::Array { elements })
            }

            (TokenKind::RBracket, _) => {
                // single element
                self.next();
                self.push_node(NodeKind::Array { elements: vec![first] })
            }

            (other, span) => return Err(ParseError::expected(span, vec![TokenKind::Semi, TokenKind::Comma, TokenKind::RBracket], other.clone())),
        }
    }
}

fn is_infix_operator(tok: &TokenKind) -> bool {
    match tok {
        TokenKind::Eq |
        TokenKind::OrOr |
        TokenKind::AndAnd |
        TokenKind::EqEq | TokenKind::NotEq |
        TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq |
        TokenKind::Plus | TokenKind::Minus |
        TokenKind::Star | TokenKind::Slash => true,
        _ => false
    }
}

fn infix_binding_power(tok: &TokenKind) -> (u8, u8) {
    match tok {
        TokenKind::Eq           => (1, 0), // right-associative
        TokenKind::OrOr         => (2, 3),
        TokenKind::AndAnd       => (4, 5),
        TokenKind::EqEq | TokenKind::NotEq => (6, 7),
        TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => (8, 9),
        TokenKind::Plus | TokenKind::Minus => (10, 11),
        TokenKind::Star | TokenKind::Slash => (12, 13),
        _ => (0, 0),
    }
}

fn token_to_binary_op(tok: &TokenKind) -> BinaryOp {
    match tok {
        TokenKind::Plus => BinaryOp::Add,
        TokenKind::Minus => BinaryOp::Sub,
        TokenKind::Star => BinaryOp::Mul,
        TokenKind::Slash => BinaryOp::Div,
        TokenKind::Lt   => BinaryOp::Lt,
        TokenKind::LtEq => BinaryOp::Le,
        TokenKind::Gt   => BinaryOp::Gt,
        TokenKind::GtEq => BinaryOp::Ge,
        TokenKind::EqEq => BinaryOp::Eq,
        TokenKind::NotEq => BinaryOp::Ne,
        TokenKind::OrOr => BinaryOp::Or,
        TokenKind::AndAnd => BinaryOp::And,
        TokenKind::Eq   => BinaryOp::Assign,
        _ => panic!("Token '{:?}' is not a binary op!", tok)
    }
}

fn token_to_unary_op(tok: &TokenKind) -> UnaryOp {
    match tok {
        TokenKind::Not  => UnaryOp::Not,
        TokenKind::Minus => UnaryOp::Neg,
        _ => panic!("Token '{:?}' is not a unary op!", tok)
    }
}

// --- Types ---

impl Parser {
    fn parse_type(&mut self) -> PResult<TypeId> {
        Ok(match self.next_with_span() {
            (TokenKind::Identifier(name), _) => {
                let name = name.clone();    // borrowing...
                self.parse_simple_generic_type(name)?
            },

            (TokenKind::LBracket, _) => self.parse_array_type()?,

            (TokenKind::LParen, _) => self.parse_tuple_type()?,

            (TokenKind::Fn, _) => self.parse_func_type()?,

            (other, span) => return Err(ParseError::new(span, format!("Unexpected token in type: {:?}", other))),
        })
    }

    fn parse_simple_generic_type(&mut self, name: String) -> PResult<TypeId> {
        if matches!(self.peek(), TokenKind::Lt) {
            self.next();
            let mut args = Vec::new();

            if !matches!(self.peek(), TokenKind::Gt) {
                loop {
                    args.push(self.parse_type()?);
                    if matches!(self.peek(), TokenKind::Comma) {
                        self.next();
                    } else {
                        break;
                    }
                    if matches!(self.peek(), TokenKind::Gt) {
                        break;
                    }
                }
            }

            self.expect(TokenKind::Gt)?;
            return Ok(self.push_type(TypeKind::Generic { base: name, args: args }));
        }

        Ok(self.push_type(TypeKind::Simple(name)))
    }

    fn parse_array_type(&mut self) -> PResult<TypeId> {
        let ty = self.parse_type()?;

        let len = if matches!(self.peek(), TokenKind::Semi) {
            self.next();
            match self.next_with_span() {
                (TokenKind::Int(i), _) => Some(*i as u32),
                (other, span) => return Err(ParseError::new(span, format!("Expected integer length after ';' in array type, found: {:?}", other))),
            }
        } else { None };

        self.expect(TokenKind::RBracket)?;
        Ok(self.push_type(TypeKind::Array { ty, len }))
    }

    fn parse_tuple_type(&mut self) -> PResult<TypeId> {
        let mut elements = Vec::new();
        let mut last_was_comma = false;
        if !matches!(self.peek(), TokenKind::RParen) {
            loop {
                elements.push(self.parse_type()?);
                if matches!(self.peek(), TokenKind::Comma) {
                    self.next();
                } else {
                    break;
                }
                if matches!(self.peek(), TokenKind::RParen) {
                    last_was_comma = true;
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;

        if elements.len() == 1 && !last_was_comma {
            return Ok(elements.pop().unwrap());
        };

        Ok(self.push_type(TypeKind::Tuple(elements)))
    }

    fn parse_func_type(&mut self) -> PResult<TypeId> {
        let mut elements = Vec::new();
        if !matches!(self.peek(), TokenKind::RParen) {
            loop {
                elements.push(self.parse_type()?);
                if matches!(self.peek(), TokenKind::Comma) {
                    self.next();
                } else {
                    break;
                }
                if matches!(self.peek(), TokenKind::RParen) {
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;

        let ret = if matches!(self.peek(), TokenKind::Arrow) {
            self.next();
            Some(self.parse_type()?)
        } else { None };

        Ok(self.push_type(TypeKind::Function { params: elements, ret }))
    }
}

// --- Tests ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression() {
        use TokenKind::*;

        let tokens = vec![
            Token { span: Span { start: Pos { line: 1, col: 1 }, end: Pos { line: 1, col: 2 } }, kind: Identifier("a".into()) },
            Token { span: Span { start: Pos { line: 1, col: 3 }, end: Pos { line: 1, col: 4 } }, kind: Plus },
            Token { span: Span { start: Pos { line: 1, col: 5 }, end: Pos { line: 1, col: 6 } }, kind: Int(2) },
            Token { span: Span { start: Pos { line: 1, col: 7 }, end: Pos { line: 1, col: 8 } }, kind: Star },
            Token { span: Span { start: Pos { line: 1, col: 9 }, end: Pos { line: 1, col:10 } }, kind: Identifier("b".into()) },
            Token { span: Span { start: Pos { line: 1, col:10 }, end: Pos { line: 1, col:10 } }, kind: EOF },
        ];

        let mut parser = Parser::new(tokens);

        let root = parser.parse_expression(0).unwrap();

        println!("AST Root Node ID: {:?}", root);
        for (i, node) in parser.arena.iter().enumerate() {
            println!("{:>2}: {:?}", i, node.kind);
        }
    }

    #[test]
    fn test_parse_block() {
        use crate::compiler::lexer::Lexer;

        let src = r#"
            {
                let x = 5;
                x = x - 1;
                x + 1 * 4 + x
            }
        "#;

        let mut lexer = Lexer::new(src);
        let tokens = lexer.lex_all().unwrap();
        
        let mut parser = Parser::new(tokens);
        let root = parser.parse_block_expression().unwrap();

        println!("AST Root Node ID: {:?}", root);
        for (i, node) in parser.arena.iter().enumerate() {
            println!("{:>2}: {:?}", i, node.kind);
        }

        assert!(parser.pos_stack.is_empty(), "Pos stack isn't cleared! Stack:\n{:?}", parser.pos_stack)
    }

    #[test]
    fn test_parse_program() {
        use crate::compiler::lexer::Lexer;

        let src = r#"
            fn main() {
                fn local() -> Type { 1 + 5 }
                let x = 5;
                x + 5 = x - 1;
            }
            
            fn main2() -> Int {
                2 + 2 * 2
            }

            fn main3() -> Int {
                (2 + 2) * 2
            }
        "#;

        let mut lexer = Lexer::new(src);
        let tokens = lexer.lex_all().unwrap();
        
        let mut parser = Parser::new(tokens);
        let root = parser.parse_program().unwrap();

        println!("AST Root Node ID: {:?}", root);
        for (i, node) in parser.arena.iter().enumerate() {
            println!("{:>2}: {:?}", i, node.kind);
        }
    }

    #[test]
    fn test_parse_types() {
        use crate::compiler::lexer::Lexer;

        let src = r#"
            fn main() {
                fn local(x: Int) -> Int { x * 2 }
                let mut x: [(List<Int>, Int); 3] = [(List(_), 923); 3];
                x + 5 = x - 1;
            }
        "#;

        let mut lexer = Lexer::new(src);
        let tokens = lexer.lex_all().unwrap();
        
        let mut parser = Parser::new(tokens);
        let root = parser.parse_program().unwrap();

        println!("AST Root Node ID: {:?}", root);
        for (i, node) in parser.arena.iter().enumerate() {
            println!("{:>2}: {:?}", i, node.kind);
        }
        println!("\nType Arena:");
        for (i, ty) in parser.types.iter().enumerate() {
            println!("{:>2}: {:?}", i, ty);
        }
    }
}
