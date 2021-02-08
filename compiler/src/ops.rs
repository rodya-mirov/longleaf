use std::convert::TryFrom;

#[repr(u8)]
// SCREAMING_SNAKE_CASE is just for consistency with sample code; not doing this everywhere.
// These do feel like constants though
#[allow(non_camel_case_types)]
pub enum OpCode {
    OP_RETURN = 0,
    OP_CONSTANT = 1,
    // numerical negation
    OP_NEGATE = 2,
    OP_ADD = 3,
    OP_SUBTRACT = 4,
    OP_MULTIPLY = 5,
    OP_DIVIDE = 6,
    OP_PRINT = 7,
    // just pop something off the stack, whatever
    OP_POP = 8,
}

impl TryFrom<u8> for OpCode {
    type Error = u8;

    #[inline(always)]
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::OP_RETURN),
            1 => Ok(OpCode::OP_CONSTANT),
            2 => Ok(OpCode::OP_NEGATE),
            3 => Ok(OpCode::OP_ADD),
            4 => Ok(OpCode::OP_SUBTRACT),
            5 => Ok(OpCode::OP_MULTIPLY),
            6 => Ok(OpCode::OP_DIVIDE),
            7 => Ok(OpCode::OP_PRINT),
            8 => Ok(OpCode::OP_POP),
            _ => Err(value),
        }
    }
}

impl From<OpCode> for u8 {
    fn from(o: OpCode) -> Self {
        o as u8
    }
}
