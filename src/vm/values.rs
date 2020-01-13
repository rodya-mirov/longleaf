use std::ops::Deref;
use std::rc::Rc;

use crate::parser::StatementNode;

#[derive(Debug, Clone, PartialEq)]
pub struct Args {
    pub names: Vec<String>,
}

/// The result of evaluating an expression
#[derive(Debug)]
pub enum EvalValue {
    Float(f64),
    FloatList(Box<dyn FloatListValue>),
    FunctionDefinition(Rc<Args>, Rc<Vec<StatementNode>>),
}

impl PartialEq for EvalValue {
    fn eq(&self, other: &EvalValue) -> bool {
        use EvalValue::*;

        match self {
            Float(a) => match other {
                Float(b) => a == b,
                _ => false,
            },
            FloatList(a) => match other {
                FloatList(b) => a == b,
                _ => false,
            },
            FunctionDefinition(a_args, a_expr) => match other {
                FunctionDefinition(b_args, b_expr) => (a_args == b_args && a_expr == b_expr),
                _ => false,
            },
        }
    }
}

impl From<EvalValue> for PrimitiveValue {
    fn from(ev: EvalValue) -> PrimitiveValue {
        match ev {
            EvalValue::Float(f) => PrimitiveValue::Float(f),
            EvalValue::FloatList(vals) => {
                let fl = vals.to_float_list();
                PrimitiveValue::FloatList(fl)
            }
            EvalValue::FunctionDefinition(args, expr) => {
                PrimitiveValue::FunctionDefinition(args, expr)
            }
        }
    }
}

impl From<f64> for EvalValue {
    fn from(f: f64) -> EvalValue {
        EvalValue::Float(f)
    }
}

impl From<Vec<f64>> for EvalValue {
    fn from(vals: Vec<f64>) -> EvalValue {
        EvalValue::FloatList(Box::new(Box::new(vals)))
    }
}

/// Trait describing actions which a FloatListValue must satisfy.
/// The return type EvalValue only guarantees that (float lists) must satisfy
/// this behavior. In particular it does not guarantee any kind of mutability,
/// because this may be a view into a saved vector.
// TODO: make the Vec<f64> be a slice??? Better for parallelization
pub trait FloatListValue: Deref<Target = Vec<f64>> + std::fmt::Debug {
    fn to_float_list(self: Box<Self>) -> Rc<Vec<f64>>;
}

impl PartialEq for dyn FloatListValue {
    fn eq(&self, other: &dyn FloatListValue) -> bool {
        let a_vec: &Vec<f64> = self; // deref-magic
        let b_vec: &Vec<f64> = other;

        a_vec == b_vec
    }
}

// TODO: do something about this double indirection? We end up with a Box<Box<Vec>>
impl FloatListValue for Box<Vec<f64>> {
    fn to_float_list(self: Box<Self>) -> Rc<Vec<f64>> {
        Rc::new(**self)
    }
}

// TODO: do something about this double indirection? We end up with a Box<Rc<Vec>>
impl FloatListValue for Rc<Vec<f64>> {
    fn to_float_list(self: Box<Self>) -> Rc<Vec<f64>> {
        *self
    }
}

// TODO: why do we have PrimitiveValue and EvalValue???
#[derive(Clone, PartialEq, Debug)]
pub enum PrimitiveValue {
    Float(f64),
    FloatList(Rc<Vec<f64>>),
    FunctionDefinition(Rc<Args>, Rc<Vec<StatementNode>>),
}

impl PrimitiveValue {
    pub fn to_eval_value(&self) -> EvalValue {
        match self {
            PrimitiveValue::Float(f) => EvalValue::Float(*f),
            PrimitiveValue::FloatList(vals) => EvalValue::FloatList(Box::new(vals.clone())),
            PrimitiveValue::FunctionDefinition(args, expr) => {
                EvalValue::FunctionDefinition(args.clone(), expr.clone())
            }
        }
    }
}
