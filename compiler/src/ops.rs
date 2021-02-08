use std::convert::TryFrom;

#[repr(u8)]
// SCREAMING_SNAKE_CASE is just for consistency with sample code; not doing this everywhere.
// These do feel like constants though
#[allow(non_camel_case_types)]
pub enum OpCode {
    OP_RETURN = 0,
    OP_CONSTANT = 1,
}

impl TryFrom<u8> for OpCode {
    type Error = u8;

    #[inline(always)]
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::OP_RETURN),
            1 => Ok(OpCode::OP_CONSTANT),
            _ => Err(value),
        }
    }
}

impl From<OpCode> for u8 {
    fn from(o: OpCode) -> Self {
        o as u8
    }
}
