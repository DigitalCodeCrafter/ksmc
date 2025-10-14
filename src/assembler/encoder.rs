use std::collections::HashMap;

use super::{get_opcode_signature, parser::{Instruction, OperandValue}, semantics::ValueType, AssembleError};
use crate::Logger;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ArgKey {
    Int16(i16),
    Int32(i32),
    Double(u64),
    Bool(bool),
    String(String),
    Null,
    ArgMarker,
    IntValue(i32),
    DoubleValue(u64),
    BoolValue(bool),
    StringValue(String),
}

fn write_argument(buf: &mut Vec<u8>, ty: ValueType, value: &OperandValue, line: usize) -> Result<(), AssembleError> {
    buf.push(ty as u8);

    match (ty, value) {
        (ValueType::Int16,                                 OperandValue::Int(v))    => buf.extend(&(v.to_le_bytes()[..2])),
        (ValueType::Int32  | ValueType::ScalarIntValue,    OperandValue::Int(v))    => buf.extend(&v.to_le_bytes()),
        (ValueType::Double | ValueType::ScalarDoubleValue, OperandValue::Float(f))  => buf.extend(&f.to_le_bytes()),
        (ValueType::Bool   | ValueType::BooleanValue,      OperandValue::Bool(b))  => buf.push(if *b { 1 } else { 0 }),
        (ValueType::String | ValueType::StringValue,       OperandValue::Str(s)) => {
            let bytes = s.as_bytes();
            buf.push(bytes.len() as u8);
            buf.extend(bytes);
        }
        (ValueType::Null, _) => {}
        (ValueType::ArgMarker, _) => {}
        _ => return Err(AssembleError::TypeMismatch(line, value.clone(), ty)),
    }
    Ok(())
}

fn encode_arguments(instructions: &[Instruction], logger: &Logger) -> Result<(Vec<u8>, Vec<Vec<u32>>, u8), AssembleError> {
    let mut arg_buf = Vec::new();
    let mut arg_offsets = Vec::new();

    let mut arg_dedup: HashMap<ArgKey, u32> = HashMap::new();

    // argument section header
    arg_buf.extend_from_slice(b"%A");
    let mut address_width: u8 = 0;
    arg_buf.push(0x00); // placeholder for the final width

    // arguments
    for instr in instructions {
        let mut this_instr_offsets = Vec::new();

        for op in &instr.operands {
            let ty = op.resolved_type.unwrap_or(ValueType::Null);
            let key = match (&op.value, ty) {
                (OperandValue::Int(v),    ValueType::Int16)  => ArgKey::Int16(*v as i16),
                (OperandValue::Int(v),    ValueType::Int32)  => ArgKey::Int32(*v),
                (OperandValue::Int(v),    ValueType::ScalarIntValue)    => ArgKey::IntValue(*v),
                (OperandValue::Float(f),  ValueType::Double) => ArgKey::Double(f.to_bits()),
                (OperandValue::Float(f),  ValueType::ScalarDoubleValue) => ArgKey::DoubleValue(f.to_bits()),
                (OperandValue::Bool(b),  ValueType::Bool)   => ArgKey::Bool(*b),
                (OperandValue::Bool(b),  ValueType::BooleanValue)      => ArgKey::BoolValue(*b),
                (OperandValue::Str(s), ValueType::String) => ArgKey::String(s.clone()),
                (OperandValue::Str(s), ValueType::StringValue)       => ArgKey::StringValue(s.clone()),
                (_, ValueType::Null) => ArgKey::Null,
                (_, ValueType::ArgMarker) => ArgKey::ArgMarker,
                _ => return Err(AssembleError::TypeMismatch(instr.line, op.value.clone(), ty))
            };

            let offset = if let Some(&existing_offset) = arg_dedup.get(&key) {
                existing_offset
            } else {
                let offset = arg_buf.len() as u32;
                address_width = address_width.max((offset.ilog2() + 1).div_ceil(8) as u8);
                write_argument(&mut arg_buf, ty, &op.value, instr.line)?;
                arg_dedup.insert(key, offset);
                logger.log(3, format!("[encode] Added arg {:?} at offset {} (type: {:?})", op.value, offset, ty));
                offset
            };

            this_instr_offsets.push(offset);
        }

        arg_offsets.push(this_instr_offsets);
    }

    arg_buf[2] = address_width;

    Ok((arg_buf, arg_offsets, address_width))
}

fn encode_code(instructions: &[Instruction], arg_offsets: &[Vec<u32>], address_width: u8) -> Result<Vec<u8>, AssembleError> {
    let mut code_buf = Vec::new();

    // code headers
    code_buf.extend_from_slice(b"%F%I%M");  // TODO: maybe leverage the other sections

    // main section
    for (instr, args) in instructions.iter().zip(arg_offsets) {
        if let Some(sig) = get_opcode_signature(&instr.name) {
            code_buf.push(sig.opcode);

            for &arg_offset in args {
                code_buf.extend_from_slice(&arg_offset.to_be_bytes()[4 - address_width as usize..]);
            }
        } else {
            return Err(AssembleError::UnknownInstruction(instr.clone()));
        }
    }

    Ok(code_buf)
}

pub fn encode(instructions: &[Instruction], logger: &Logger) -> Result<Vec<u8>, AssembleError> {
    let (arg_buf, arg_offsets, address_width) = encode_arguments(instructions, logger)?;
    let code_buf = encode_code(instructions, &arg_offsets, address_width)?;

    logger.log(2, format!("[encode] Code: {} bytes, Args: {} bytes, {} byte width addresses", code_buf.len(), arg_buf.len(), address_width));

    let mut file = Vec::new();

    file.extend_from_slice(b"k\x03XE");

    file.extend(&arg_buf);
    file.extend(&code_buf);

    Ok(file)
}
