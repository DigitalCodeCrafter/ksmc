use super::{lexer::{Token, TokenType}, semantics::ValueType, AssembleError};
use crate::Logger;

#[derive(Debug, Clone)]
pub enum OperandValue {
    Ident(String),
    Bool(bool),
    Int(i32),
    Float(f64),
    Str(String),
    ArgMarker,
    Null,
    TypeDirective(String),
}

#[derive(Debug, Clone)]
pub struct Operand {
    pub value: OperandValue,
    pub resolved_type: Option<ValueType>,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub name: String,
    pub operands: Vec<Operand>,
    pub line: usize,
}

#[derive(Debug)]
pub enum Statement {
    Instruction(Instruction),
    Label(String),
    Directive(usize, String, Vec<Operand>)
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    fn expect_ident(&mut self) -> Result<String, AssembleError> {
        if let Some(tok) = self.next() {
            if let TokenType::Ident(s) = &tok.kind {
                return Ok(s.clone());
            }
            Err(AssembleError::ExpectedIdentifier(tok.line))
        } else {
            Err(AssembleError::UnexpectedEOF)
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenType::Whitespace => self.pos += 1,
                _ => break,
            }
        }
    }

    fn skip_newlines(&mut self) {
        while let Some(tok) = self.peek() {
            match tok.kind {
                TokenType::NewLine | TokenType::Whitespace => self.pos += 1,
                _ => break,
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, AssembleError>> {
        self.skip_newlines();
        
        let tok = self.peek()?;
        let line = tok.line;
        let is_directive = match tok.kind {
            TokenType::Dot => {
                self.pos += 1;
                true
            }
            _ => false
        };

        let name = match self.expect_ident() {
            Ok(n) => n,
            Err(e) => return Some(Err(e))
        };
        
        // valid cases:
        // Ident White (Ops)* -> multi operand instruction
        // Ident NewLine -> zero operand instruction
        // Ident EOF -> zero operand instruction
        // Dot [all of the above] -> directive instead of instruction
        // Ident Colon -> Label
        
        if let Some(next_tok) = self.peek() {
            match next_tok.kind {
                TokenType::Colon if !is_directive => {
                    self.pos += 1;
                    Some(Ok(Statement::Label(name)))
                }
                TokenType::Whitespace => {
                    let operands = match self.parse_operands(is_directive) {
                        Ok(o) => o,
                        Err(e) => return Some(Err(e)),
                    };
                    Some(Ok(if is_directive {
                        Statement::Directive(line, name, operands)
                    } else {
                        Statement::Instruction(Instruction { name, operands, line })
                    }))
                }
                TokenType::NewLine => {
                     Some(Ok(if is_directive {
                        Statement::Directive(line, name, Vec::new())
                    } else {
                        Statement::Instruction(Instruction { name, operands: Vec::new(), line })
                    }))
                }
                _ => Some(Err(AssembleError::UnexpectedToken(next_tok.line, next_tok.kind.clone())))
            }
        } else {
             Some(Ok(if is_directive {
                Statement::Directive(line, name, Vec::new())
            } else {
                Statement::Instruction(Instruction { name, operands: Vec::new(), line })
            }))
        }
    }

    fn parse_operands(&mut self, is_direcitve: bool) -> Result<Vec<Operand>, AssembleError> {
        let mut ops = Vec::new();

        loop {
            self.skip_whitespace();

            let tok = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            let mut is_td = false;

            match tok.kind {
                TokenType::IntegerLiteral(v) => {
                    self.pos += 1;
                    ops.push(OperandValue::Int(v));
                }
                TokenType::FloatingLiteral(v) => {
                    self.pos += 1;
                    ops.push(OperandValue::Float(v));
                }
                TokenType::StringLiteral(ref s) => {
                    self.pos += 1;
                    ops.push(OperandValue::Str(s.clone()));
                }
                TokenType::Ident(ref s) => {
                    self.pos += 1;
                    let lower = s.to_lowercase();
                    if lower == "true" {
                        ops.push(OperandValue::Bool(true));
                    } else if lower == "false" {
                        ops.push(OperandValue::Bool(false));
                    } else {
                        ops.push(OperandValue::Ident(s.clone()));
                    }
                }
                TokenType::At => {
                    self.pos += 1;
                    ops.push(OperandValue::ArgMarker);
                }
                TokenType::Hash => {
                    self.pos += 1;
                    ops.push(OperandValue::Null);
                }
                TokenType::Dot => {
                    self.pos += 1; // consume
                    let s = self.expect_ident()?;
                    ops.push(OperandValue::TypeDirective(s));
                    is_td = true;
                }
                _ => break,
            }

            if !is_td && !is_direcitve {
                self.skip_whitespace();
            }

            if let Some(next_tok) = self.peek() {
                match next_tok.kind {
                    TokenType::Comma if !is_td && !is_direcitve => {
                        self.pos += 1;
                    }
                    TokenType::NewLine if !is_td => break,
                    
                    TokenType::Whitespace if is_td || is_direcitve => {
                        self.pos += 1;
                        if !is_td { continue; }
                        // check for a valid next token if declaring a type
                        if let Some(next_tok) = self.peek() {
                            match &next_tok.kind {
                                TokenType::At | TokenType::Hash | TokenType::IntegerLiteral(_) | TokenType::FloatingLiteral(_) | TokenType::Ident(_) | TokenType::StringLiteral(_) => {}
                                other => return Err(AssembleError::UnexpectedToken(next_tok.line, other.clone()))
                            }
                        } else {
                            return Err(AssembleError::UnexpectedEOF);
                        }
                    }
                    _ if is_td || is_direcitve => return Err(AssembleError::ExpectedWhitespace(next_tok.line)),
                    _ => return Err(AssembleError::ExpectedComma(next_tok.line)),
                }
            } else {
                break;
            }
        }

        Ok(ops.into_iter().map(|value| Operand { value, resolved_type: None }).collect())
    }

    pub fn parse_all(&mut self, logger: &Logger) -> Result<Vec<Statement>, AssembleError> {
        let mut stmts = Vec::new();

        while let Some(result) = self.parse_statement() {
            match result {
                Ok(stmt) => {
                    logger.log(3, format!("[Parser] Parsed statement: {:?}", stmt));
                    stmts.push(stmt);
                },
                Err(e) => return Err(e),
            }
        }

        logger.log(2, format!("[Parser] Parsed {} statements", stmts.len()));

        Ok(stmts)
    }
}
