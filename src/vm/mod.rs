use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::parser::*;

#[macro_use]
mod macros;

pub type VmResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub enum EvalError {
    UnknownVariable(String),
    DimensionMismatch(String),
}

pub type Value = Rc<PrimitiveValue>;

#[derive(Clone, PartialEq, Debug)]
pub enum PrimitiveValue {
    Float(f64),
    FloatList(Vec<f64>),
}

pub struct VM {
    stored_values: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stored_values: HashMap::new(),
        }
    }

    pub fn define_variable(&mut self, name: &str, value: Value) {
        let name = name.to_string();

        self.stored_values.insert(name, value.clone());
    }

    pub fn evaluate_expr(&self, expr: ExprNode) -> VmResult<Value> {
        let out: Value = match expr {
            ExprNode::Float(f) => Rc::new(PrimitiveValue::Float(f)),
            ExprNode::FloatList(vals) => Rc::new(PrimitiveValue::FloatList(vals)),
            ExprNode::UnaryExpr(op, val) => self.eval_unary_expr(op, *val)?,
            ExprNode::BinaryExpr(op, a, b) => self.eval_binary_expr(op, *a, *b)?,
            ExprNode::VariableRef(id) => {
                let stored = self.stored_values.get(&id);
                match stored {
                    None => {
                        return Err(EvalError::UnknownVariable(format!(
                            "No value saved for variable {}",
                            id
                        )));
                    }
                    Some(val) => val.clone(),
                }
            }
        };
        Ok(out)
    }

    fn eval_unary_expr(&self, op: UnaryOp, expr: ExprNode) -> VmResult<Value> {
        use PrimitiveValue::*;
        use UnaryOp::*;

        let inner: Value = self.evaluate_expr(expr)?;

        let out: PrimitiveValue = match op {
            Negate => unary_switcher!(-, inner),
        };

        Ok(Rc::new(out))
    }

    fn eval_binary_expr(&self, op: BinaryOp, a: ExprNode, b: ExprNode) -> VmResult<Value> {
        use BinaryOp::*;
        use PrimitiveValue::*;

        let a: Value = self.evaluate_expr(a)?;
        let b: Value = self.evaluate_expr(b)?;

        let out: PrimitiveValue = match op {
            Plus => binary_switcher!(+, a, b),
            Minus => binary_switcher!(-, a, b),
            Times => binary_switcher!(*, a, b),
            Divide => binary_switcher!(/, a, b),
        };

        Ok(Rc::new(out))
    }
}

impl fmt::Display for PrimitiveValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            PrimitiveValue::Float(val) => write!(f, "{}", val),
            PrimitiveValue::FloatList(vals) => {
                write!(f, "[")?;
                if vals.is_empty() {
                    write!(f, "]")?;
                    return Ok(());
                }
                let mut iter = vals.into_iter();
                write!(f, "{}", iter.next().unwrap())?;

                while let Some(next) = iter.next() {
                    write!(f, ", {}", next)?;
                }
                write!(f, "]")
            }
        }
    }
}

#[cfg(test)]
mod tests;
