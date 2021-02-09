use std::fmt::{Debug, Display, Formatter};

pub mod chunk;
pub mod compile;
pub mod debug;
pub mod ops;

// TODO: this probably shouldn't live here but we'll leave it alone for now
// Enumeration of all possible values of a longleaf object
#[derive(Debug, Copy, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", *b),
            Value::Number(n) => write!(f, "{:.03}", *n),
        }
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Number(f)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Nil
    }
}
