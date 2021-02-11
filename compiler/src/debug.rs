use std::convert::TryFrom;
use std::io::Write;

use crate::{chunk::Chunk, ops::OpCode, Value};

type IoResult<T = ()> = Result<T, std::io::Error>;

pub fn disassemble_chunk<W: Write>(chunk: &Chunk, name: &str, w: &mut W) -> IoResult {
    write!(w, "== {} ==\n", name)?;

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instr(chunk, offset, w)?;
    }

    Ok(())
}

pub fn disassemble_instr<W: Write>(chunk: &Chunk, offset: usize, w: &mut W) -> IoResult<usize> {
    write!(w, "{:04} ", offset)?;

    let line = chunk.lines[offset];
    if offset > 0 && line == chunk.lines[offset - 1] {
        write!(w, "   | ")?;
    } else {
        write!(w, "{:04} ", line)?;
    }

    match OpCode::try_from(chunk.code[offset]) {
        Ok(OpCode::OP_RETURN) => simple_instruction("OP_RETURN", offset, w),
        Ok(OpCode::OP_CONSTANT) => constant_instruction(chunk, "OP_CONSTANT", offset, w),
        Ok(OpCode::OP_NEGATE) => simple_instruction("OP_NEGATE", offset, w),
        Ok(OpCode::OP_ADD) => simple_instruction("OP_ADD", offset, w),
        Ok(OpCode::OP_SUBTRACT) => simple_instruction("OP_SUBTRACT", offset, w),
        Ok(OpCode::OP_MULTIPLY) => simple_instruction("OP_MULTIPLY", offset, w),
        Ok(OpCode::OP_DIVIDE) => simple_instruction("OP_DIVIDE", offset, w),
        Ok(OpCode::OP_PRINT) => simple_instruction("OP_PRINT", offset, w),
        Ok(OpCode::OP_POP) => simple_instruction("OP_POP", offset, w),
        Ok(OpCode::OP_NIL) => simple_instruction("OP_NIL", offset, w),
        Ok(OpCode::OP_TRUE) => simple_instruction("OP_TRUE", offset, w),
        Ok(OpCode::OP_FALSE) => simple_instruction("OP_FALSE", offset, w),
        Ok(OpCode::OP_NOT) => simple_instruction("OP_NOT", offset, w),
        Ok(OpCode::OP_GEQ) => simple_instruction("OP_GEQ", offset, w),
        Ok(OpCode::OP_GT) => simple_instruction("OP_GT", offset, w),
        Ok(OpCode::OP_EQ) => simple_instruction("OP_EQ", offset, w),
        Ok(OpCode::OP_NEQ) => simple_instruction("OP_NEQ", offset, w),
        Ok(OpCode::OP_LEQ) => simple_instruction("OP_LEQ", offset, w),
        Ok(OpCode::OP_LT) => simple_instruction("OP_LT", offset, w),
        Err(unknown_code) => {
            write!(w, "Unknown opcode {}\n", unknown_code)?;
            Ok(offset + 1)
        }
    }
}

fn simple_instruction<W: Write>(name: &str, offset: usize, w: &mut W) -> IoResult<usize> {
    write!(w, "{}\n", name)?;
    Ok(offset + 1)
}

fn constant_instruction<W: Write>(
    chunk: &Chunk,
    name: &str,
    offset: usize,
    w: &mut W,
) -> IoResult<usize> {
    let const_idx: u8 = chunk.code[offset + 1];
    write!(w, "{} {:04} '", name, const_idx)?;
    print_value(w, chunk.constants[const_idx as usize])?;
    write!(w, "'\n")?;
    Ok(offset + 2)
}

fn print_value<W: Write>(w: &mut W, val: Value) -> IoResult {
    write!(w, "{}", val)?;
    Ok(())
}
