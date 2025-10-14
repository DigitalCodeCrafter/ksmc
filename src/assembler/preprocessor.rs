use std::collections::HashMap;

use super::{get_opcode_signature, parser::{Instruction, Operand, OperandValue, Statement}, AssembleError, OperandTypeSet};
use crate::Logger;


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    // Primitives
    Bool        = 0x01,
    Int16       = 0x03,
    Int32       = 0x04,
    Double      = 0x06,
    String      = 0x07,
    Null        = 0x00,
    ArgMarker   = 0x08,

    // Structures
    BooleanValue        = 0x0b,
    ScalarIntValue      = 0x09,
    ScalarDoubleValue   = 0x0a,
    StringValue         = 0x0c,
}

fn get_type_from_key(key: &str) -> Option<ValueType> {
    Some(match key.to_ascii_lowercase().as_str() {
        "b"     => ValueType::Bool,
        "i16"   => ValueType::Int16,
        "i32"   => ValueType::Int32,
        "f64"   => ValueType::Double,
        "s"     => ValueType::String,
        "bv"    => ValueType::BooleanValue,
        "i32v"  => ValueType::ScalarIntValue,
        "f64v"  => ValueType::ScalarDoubleValue,
        "sv"    => ValueType::StringValue,
        _ => return None,
    })
}

fn get_address_type_from_key(key: &str) -> Option<AddressKind> {
    Some(match key.to_ascii_lowercase().as_str() {
        "a" => AddressKind::Absolute,
        "r" => AddressKind::Relative,
        "rn" => AddressKind::RelativeNext,
        _ => return None,
    })
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressKind {
    Absolute,
    Relative,
    // uses the position of the next instruction instead of the current one
    // useful for `PUSH .rn label -> BST`
    RelativeNext,
}

pub fn semantic_analysis(mut stmts: Vec<Statement>, logger: &Logger) -> Result<Vec<Instruction>, AssembleError> {

    stmts = expand_macros(&stmts)?;

    resolve_symbols(&mut stmts, logger)?;

    type_check(&mut stmts, logger)?;

    let mut instructions = Vec::new();
    for stmt in stmts.drain(..) {
        if let Statement::Instruction(instr) = stmt {
            instructions.push(instr);
        }
    }

    Ok(instructions)
}

// Macros

struct MacroDef {
    name: String,
    params: Vec<String>,
    body: Vec<Statement>
}

fn expand_macros(stmts: &[Statement]) -> Result<Vec<Statement>, AssembleError> {
    let mut macros = HashMap::new();
    let mut output = Vec::new();
    let mut iter = stmts.iter().peekable();

    while let Some(stmt) = iter.next() {
        match stmt {
            Statement::Directive(line, name, args) if name == "macro" => {
                let macro_def = collect_macro(&mut iter, args, *line)?;
                macros.insert(macro_def.name.clone(), macro_def);
            }
            Statement::Instruction(instr) if macros.contains_key(&instr.name) => {
                let expanded = expand_invocation(instr, &macros)?;
                output.extend(expanded);
            }
            _ => output.push(stmt.clone()),
        }
    }

    Ok(output)
}

fn collect_macro<'a, I: Iterator<Item = &'a Statement>>(iter: &mut std::iter::Peekable<I>, header_args: &[Operand], line: usize) -> Result<MacroDef, AssembleError> {
    let (name, params) = parse_macro_header(header_args, line)?;

    let mut body = Vec::new();
    while let Some(stmt) = iter.next() {
        match stmt {
            Statement::Directive(_, dname, _) if dname == "endm" => {
                return Ok(MacroDef { name, params, body });
            }
            _ => body.push(stmt.clone()),
        }
    }

    Err(AssembleError::UnterminatedMacro(line))
}

fn parse_macro_header(args: &[Operand], line: usize) -> Result<(String, Vec<String>), AssembleError> {
    if args.is_empty() {
        return Err(AssembleError::IncorrectArgumentCount(line, ".macro".into(), 1, 0));
    }

    let name = match &args[0].value {
        OperandValue::Ident(id) => id.clone(),
        _ => return Err(AssembleError::ExpectedIdentifier(line))
    };

    let params = args.iter()
        .skip(1)
        .filter_map(|op| match &op.value {
            OperandValue::MacroParam(p) => Some(p.clone()),
            _ => None,
        })
        .collect();

    Ok((name, params))
}

fn expand_invocation(instr: &Instruction, macros: &HashMap<String, MacroDef>) -> Result<Vec<Statement>, AssembleError> {
    let macro_def = macros.get(&instr.name)
        .ok_or_else(|| AssembleError::UnknownMacro(instr.line, instr.name.clone()))?;

    if macro_def.params.len() != instr.operands.len() {
        return Err(AssembleError::IncorrectArgumentCount(
            instr.line, 
            instr.name.clone(), 
            macro_def.params.len(), 
            instr.operands.len()
        ));
    }

    let mut subst = HashMap::new();
    for (param, arg) in macro_def.params.iter().zip(instr.operands.iter()) {
        subst.insert(param.clone(), arg.clone());
    }

    let mut expanded_body = Vec::new();
    for stmt in &macro_def.body {
        expanded_body.push(substitute_operands(stmt.clone(), &subst)?);
    }

    Ok(expanded_body)
}

fn substitute_operands(stmt: Statement, subst: &HashMap<String, Operand>) -> Result<Statement, AssembleError> {
    Ok(match stmt {
        Statement::Instruction(Instruction { name, mut operands, line }) => {
            for op in &mut operands {
                *op = substitute_operand(op.clone(), subst, line)?;
            }
            Statement::Instruction(Instruction { name, operands, line })
        }
        Statement::Directive(line, name, mut args) => {
            for op in &mut args {
                *op = substitute_operand(op.clone(), subst, line)?;
            }
            Statement::Directive(line, name, args)
        }
        Statement::Label(lbl) => {
            Statement::Label(lbl)
        }
    })
}

fn substitute_operand(op: Operand, subst: &HashMap<String, Operand>, line: usize) -> Result<Operand, AssembleError> {
    match op.value {
        OperandValue::MacroParam(s) => {
            if subst.contains_key(&s) {
                Ok(subst[&s].clone())
            } else {
                Err(AssembleError::UnknownSymbol(line, s))
            }
        }
        _ => Ok(op)
    }
}

// symbol resolution

enum Symbol {
    Const(Operand),
    Label(u32),
}

const START_OFFSET: u32 = 25;

fn resolve_symbols(stmts: &mut [Statement], logger: &Logger) -> Result<(), AssembleError> {
    let mut symbols = HashMap::new();
    let mut offset = START_OFFSET;

    // Pass 1
    for stmt in stmts.iter() {
        match stmt {
            Statement::Label(name) => {
                logger.log(3, format!("[symbols] Found label '{}' with offset: {}", name, offset));
                symbols.insert(name.clone(), Symbol::Label(offset));
            }

            Statement::Directive(line, name, args) if name == "const" => {
                if args.len() != 3 {
                    return Err(AssembleError::IncorrectArgumentCount(*line, "const".into(), 3, args.len()));
                }
                let OperandValue::TypeDirective(td) = &args[0].value else { return Err(AssembleError::InvalidDirective(*line, "const".into())); };
                let OperandValue::Ident(id) = &args[1].value else { return Err(AssembleError::InvalidDirective(*line, "const".into())); };
                let Some(ty) = get_type_from_key(td) else { return Err(AssembleError::InvalidTypeSpecifier(*line, td.clone())); };
                let mut operand = args[2].clone();
                type_cast(&mut operand, ty, *line)?;
                logger.log(3, format!("[symbols] Found constant '{}' of type '{:?}' with value: {:?}", name, ty, operand.value));
                symbols.insert(id.clone(), Symbol::Const(operand));
            }

            Statement::Directive(line, name, args) if name == "label" => {
                if args.len() != 1 {
                    return Err(AssembleError::IncorrectArgumentCount(*line, "label".into(), 1, args.len()));
                }
                let OperandValue::Ident(id) = &args[0].value else { return Err(AssembleError::InvalidDirective(*line, "const".into())); };
                logger.log(3, format!("[symbols] Found label '{}' with offset: {:?}", name, offset));
                symbols.insert(id.clone(), Symbol::Label(offset));
            }

            // add more here later

            Statement::Directive(line, name, _) => {
                return Err(AssembleError::InvalidDirective(*line, name.clone()));
            }

            Statement::Instruction(instr) => {
                // NOTE: The Label Reset (lbrt) is removed when the cpu loads the code.
                if let Some(sig) = get_opcode_signature(&instr.name) {
                    if sig.opcode == 0xf0 { continue; }
                }
                offset += 1;
            }
        }
    }

    // Pass 2
    offset = START_OFFSET;
    let mut i = 0;
    while i < stmts.len() {
        let stmt = &mut stmts[i];
        let Statement::Instruction(instr) = stmt else { i += 1; continue; };

        let mut last_td_type = None;
        let mut last_td_ak = None;
        // NOTE: JMP, BTR, BFA and BST use relative jumps
        let needed_ak = if let Some(sig) = get_opcode_signature(&instr.name) {
            match sig.opcode {
                0x3b | 0x5e | 0x3a => Some(AddressKind::Relative),
                0x4c => Some(AddressKind::Absolute),
                _ => None
            }
        } else { None };
        
        // iterate through arguments
        for op in instr.operands.iter_mut() {
            match &op.value {
                OperandValue::TypeDirective(td) => {
                    last_td_type = get_type_from_key(td);
                    last_td_ak = get_address_type_from_key(td);
                    match (last_td_ak, last_td_type) {
                        (None, None) => return Err(AssembleError::InvalidTypeSpecifier(instr.line, td.clone())),
                        _ => {}
                    }
                }
                OperandValue::Ident(name) => {
                    let Some(symbol) = symbols.get(name) else { return Err(AssembleError::UnknownSymbol(instr.line, name.clone())); };
                    // Don't allow type casting on symbols
                    if let Some(ty) = last_td_type {
                        return Err(AssembleError::InvalidTypeCast(instr.line, op.value.clone(), ty));
                    }
                    match symbol {
                        Symbol::Const(val) => {
                            // don't allow addressing specification on constants
                            if let Some(ak) = last_td_ak {
                                return Err(AssembleError::InvalidAddresKind(instr.line, ak));
                            }
                            logger.log(3, format!("[symbols] Resolved Constant '{}'", name));
                            *op = val.clone();
                        }
                        Symbol::Label(pos) => {
                            let addressing_kind = match (last_td_ak, needed_ak) {
                                (Some(AddressKind::Absolute), Some(AddressKind::Relative)) => return Err(AssembleError::InvalidAddresKind(instr.line, AddressKind::Absolute)),
                                (Some(AddressKind::Relative), Some(AddressKind::Absolute)) => return Err(AssembleError::InvalidAddresKind(instr.line, AddressKind::Relative)),
                                (Some(AddressKind::RelativeNext), Some(AddressKind::Absolute)) => return Err(AssembleError::InvalidAddresKind(instr.line, AddressKind::RelativeNext)),
                                (Some(ak), _) => ak,
                                (None, Some(ak)) => ak,
                                (None, None) => return Err(AssembleError::UnableToInferr(instr.line)),
                            };
                            match addressing_kind {
                                AddressKind::Absolute => {
                                    logger.log(3, format!("[symbols] Resolved Label '{}' with absolute position: {}", name, *pos as i32 ));
                                    *op = Operand { value: OperandValue::Int(*pos as i32), resolved_type: Some(ValueType::Int32) }
                                }
                                AddressKind::Relative => {
                                    logger.log(3, format!("[symbols] Resolved Label '{}', with relative position: {:+}", name, *pos as i32 - offset as i32));
                                    *op = Operand {
                                        value: OperandValue::Int(*pos as i32 - offset as i32),
                                        resolved_type: Some(ValueType::Int32)
                                    };
                                }
                                AddressKind::RelativeNext => {
                                    logger.log(3, format!("[symbols] Resolved Label '{}', with relative position: {:+}", name, *pos as i32 - offset as i32 - 1));
                                    *op = Operand {
                                        value: OperandValue::Int(*pos as i32 - offset as i32 - 1),
                                        resolved_type: Some(ValueType::Int32)
                                    };
                                }
                            }
                        }
                    }
                    last_td_type = None;
                    last_td_ak = None;
                }
                _ => {
                    last_td_type = None;
                    last_td_ak = None;
                }
            }
        }
        // NOTE: The Label Reset (lbrt) is removed when the cpu loads the code.
        if let Some(sig) = get_opcode_signature(&instr.name) {
            if sig.opcode == 0xf0 { continue; }
        }
        offset += 1;
        i += 1;
    }

    logger.log(2, format!("[symbols] Found {} Symbols", symbols.len()));

    Ok(())
}

// Type checking

fn type_check(stmts: &mut [Statement], logger: &Logger) -> Result<(), AssembleError> {
    for stmt in stmts {
        // only on instructions
        if let Statement::Instruction(instr) = stmt {
            // get the signature
            if let Some(sig) = get_opcode_signature(&instr.name) {
                // remove type annotations
                let mut filtered_operands = Vec::new();

                let mut actuals = instr.operands.drain(..);
                for (i, expected) in sig.operand_types.iter().enumerate() {
                    let Some(mut actual) = actuals.next() else {
                        return Err(AssembleError::IncorrectArgumentCount(instr.line, instr.name.clone(), sig.operand_types.len(), i))
                    };

                    // handle type annotations
                    if let OperandValue::TypeDirective(type_key) = &actual.value {
                        let Some(declared_type) = get_type_from_key(type_key) else {
                            return Err(AssembleError::InvalidTypeSpecifier(instr.line, type_key.clone()))
                        };
                        
                        match actuals.next() {
                            Some(next) => actual = next,
                            None => return Err(AssembleError::IncorrectArgumentCount(instr.line, instr.name.clone(), sig.operand_types.len(), i)),
                        };

                        type_cast(&mut actual, declared_type, instr.line)?;
                    }

                    // validate / infer type
                    match actual.resolved_type {
                        Some(ty) => {
                            if expected.allows(ty) {
                                logger.log(3, format!("[typecheck] Validated type for operand {} of {}", i, instr.name));
                            } else {
                                return Err(AssembleError::ArgumentTypeMismatch(instr.line, ty, expected.0));
                            }
                        }
                        None => {
                            let actual_ty = infer_best_type(&actual.value, expected, instr.line)?;
        
                            logger.log(3, format!("[typecheck] Inferred {:?} for operand {} of {}", actual_ty, i + 1, instr.name));
        
                            actual.resolved_type = Some(actual_ty);
                        }
                    }
                    filtered_operands.push(actual);
                }
                // make sure to remove borrow
                drop(actuals);
                instr.operands = filtered_operands;
            } else {
                return Err(AssembleError::UnknownInstruction(instr.clone()));
            }
        }
    }

    Ok(())
}

fn infer_best_type(op: &OperandValue, allowed: &OperandTypeSet, line: usize) -> Result<ValueType, AssembleError> {
    use ValueType::*;

    let mut matches = Vec::new();

    for &candidate in allowed.0 {
        match (op, candidate) {
            // Integer literal
            (OperandValue::Int(v), Int16) if *v >= i16::MIN as i32 && *v <= i16::MAX as i32 => {
                matches.push(Int16);
            }
            (OperandValue::Int(_), Int32) => matches.push(Int32),
            (OperandValue::Int(_), ScalarIntValue) => matches.push(ScalarIntValue),

            // float literal
            (OperandValue::Float(_), Double) => matches.push(Double),
            (OperandValue::Float(_), ScalarDoubleValue) => matches.push(ScalarDoubleValue),

            // bool literal
            (OperandValue::Bool(_), Bool) => matches.push(Bool),
            (OperandValue::Bool(_), BooleanValue) => matches.push(BooleanValue),

            // String literal
            (OperandValue::Str(_), String) => matches.push(String),
            (OperandValue::Str(_), StringValue) => matches.push(StringValue),

            // Null
            (OperandValue::Null, Null) => matches.push(Null),
            (OperandValue::Ident(name), Null) if name.eq_ignore_ascii_case("null") => matches.push(Null),

            // ArgMarker
            (OperandValue::ArgMarker, ArgMarker) => matches.push(ArgMarker),
            (OperandValue::Ident(name), ArgMarker) if name.eq_ignore_ascii_case("argmarker") => matches.push(ArgMarker),

            _ => {}
        }
    }

    if matches.is_empty() {
        // default types for error message
        Err(AssembleError::ArgumentTypeMismatch(line, match op {
            OperandValue::ArgMarker => ValueType::ArgMarker,
            OperandValue::Null => ValueType::Null,
            OperandValue::Str(_) => ValueType::String,
            OperandValue::Bool(_) => ValueType::Bool,
            OperandValue::Float(_) => ValueType::Double,
            OperandValue::Int(_) => ValueType::Int32,
            // These shouldn't exist anymore.
            OperandValue::Ident(_) => ValueType::Null,
            OperandValue::TypeDirective(_) => ValueType::Null,
            OperandValue::MacroParam(_) => ValueType::Null,
        }, allowed.0))
    } else {
        let preceedence = [
            Int16,
            Int32,
            ScalarIntValue,
            Double,
            ScalarDoubleValue,
            Bool,
            BooleanValue,
            String,
            StringValue,
            Null,
            ArgMarker,
        ];

        let selected = preceedence.iter()
            .find(|t| matches.contains(t))
            .cloned()
            .unwrap_or(matches[0]);

        Ok(selected)
    }
}

fn type_cast(op: &mut Operand, cast_as: ValueType, line: usize) -> Result<(), AssembleError> {
    match (&op.value, cast_as) {
        (OperandValue::Bool(_), ValueType::Bool | ValueType::BooleanValue) => op.resolved_type = Some(cast_as),
        (OperandValue::Int(_), ValueType::Int32 | ValueType::ScalarIntValue) => op.resolved_type = Some(cast_as),
        (OperandValue::Float(_), ValueType::Double | ValueType::ScalarDoubleValue) => op.resolved_type = Some(cast_as),
        (OperandValue::Str(_), ValueType::String | ValueType::StringValue) => op.resolved_type = Some(cast_as),
        // bounds check for i32 -> i16 cast
        (OperandValue::Int(v), ValueType::Int16) if *v >= i16::MIN as i32 && *v <= i16::MAX as i32 => op.resolved_type = Some(cast_as),
        // allow casting an int as a float
        (OperandValue::Int(v), ValueType::Double | ValueType::ScalarDoubleValue) => {
            op.resolved_type = Some(cast_as);
            op.value = OperandValue::Float(*v as f64);
        }
        (OperandValue::Ident(name), _) => return Err(AssembleError::UnresolvedSymbol(line, name.clone())),
        _ => return Err(AssembleError::InvalidTypeCast(line, op.value.clone(), cast_as))
    }
    Ok(())
}
