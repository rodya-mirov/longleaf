use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use crate::parser::*;

#[macro_use]
mod macros;

mod store;
use store::VectorStore;

// This is the whole point of once_cell; it's been vetted, it works
lazy_static! {
    static ref RESERVED_WORDS: HashSet<&'static str> =
        vec!["sin", "cos", "tan", "range"].into_iter().collect();
}

pub type VmResult<T> = Result<T, EvalError>;

#[derive(Debug)]
pub enum EvalError {
    UnknownVariable(String),
    DimensionMismatch(String),
    RedefineReservedWord(String),
    WrongNumArgs(String),
    TypeMismatch(String),
    IllegalArgument(String),
}

/// The result of evaluating an expression
#[derive(Debug)]
pub enum EvalValue {
    Float(f64),
    FloatList(Box<dyn FloatListValue>),
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

#[derive(Clone, PartialEq, Debug)]
pub enum PrimitiveValue {
    Float(f64),
    FloatList(Rc<Vec<f64>>),
}

impl PrimitiveValue {
    fn to_eval_value(&self) -> EvalValue {
        match self {
            PrimitiveValue::Float(f) => EvalValue::Float(*f),
            PrimitiveValue::FloatList(vals) => EvalValue::FloatList(Box::new(vals.clone())),
        }
    }
}

pub struct VM {
    variable_definitions: HashMap<String, PrimitiveValue>,
    arena: VectorStore,
}

type VmFunction<'a> = Box<dyn Fn(Vec<EvalValue>, &'a VectorStore) -> VmResult<EvalValue>>;

impl VM {
    pub fn new() -> Self {
        VM {
            variable_definitions: HashMap::new(),
            arena: VectorStore::new(),
        }
    }

    pub fn define_variable<T>(&mut self, name: &str, value: T) -> VmResult<()>
    where
        T: Into<PrimitiveValue>,
    {
        if RESERVED_WORDS.contains(name) {
            return Err(EvalError::RedefineReservedWord(format!(
                "Cannot associate a value to name {}, because it is reserved",
                name
            )));
        }
        let name = name.to_string();

        self.variable_definitions.insert(name, value.into());
        Ok(())
    }

    pub fn evaluate_expr(&self, expr: ExprNode) -> VmResult<EvalValue> {
        let out: EvalValue = match expr {
            ExprNode::Float(f) => EvalValue::Float(f),
            // TODO: This double-indirection is embarassing
            ExprNode::FloatList(vals) => EvalValue::FloatList(Box::new(Box::new(vals))),
            ExprNode::FunctionCall(name, args) => self.eval_function_call(name, args)?,
            ExprNode::UnaryExpr(op, val) => self.eval_unary_expr(op, *val)?,
            ExprNode::BinaryExpr(op, a, b) => self.eval_binary_expr(op, *a, *b)?,
            ExprNode::VariableRef(id) => {
                let stored = self.variable_definitions.get(&id);
                match stored {
                    None => {
                        return Err(EvalError::UnknownVariable(format!(
                            "No value saved for variable {}",
                            id
                        )));
                    }
                    Some(val) => val.to_eval_value(),
                }
            }
        };
        Ok(out)
    }

    fn eval_function_call(&self, name: String, args: Vec<ExprNode>) -> VmResult<EvalValue> {
        let name_str: &str = &name;
        // TODO: this doesn't _feel_ like it's saving a lot of code, it expanded to be enormous
        let (num_args, procedure): (usize, VmFunction<'_>) = match name_str {
            "sin" => (
                1,
                Box::new(|mut results, arena| {
                    Ok(unary_fn_switcher!(
                        <f64>::sin,
                        results.pop().unwrap(),
                        arena
                    ))
                }),
            ),
            "cos" => (
                1,
                Box::new(|mut results, arena| {
                    Ok(unary_fn_switcher!(
                        <f64>::cos,
                        results.pop().unwrap(),
                        arena
                    ))
                }),
            ),
            "tan" => (
                1,
                Box::new(|mut results, arena| {
                    Ok(unary_fn_switcher!(
                        <f64>::tan,
                        results.pop().unwrap(),
                        arena
                    ))
                }),
            ),
            "range" => (
                3,
                Box::new(|mut args, arena| {
                    let step = args.pop().unwrap();
                    let end = args.pop().unwrap();
                    let start = args.pop().unwrap();
                    make_range(start, end, step, arena)
                }),
            ),
            _ => return Err(EvalError::UnknownVariable(name_str.to_string())),
        };

        if args.len() != num_args {
            return Err(EvalError::WrongNumArgs(format!(
                "For function {}, expected {} args, but got {}",
                name_str,
                num_args,
                args.len()
            )));
        }

        let mut results = Vec::with_capacity(num_args);

        for arg in args {
            results.push(self.evaluate_expr(arg)?);
        }

        procedure(results, &self.arena)
    }

    fn eval_unary_expr(&self, op: UnaryOp, expr: ExprNode) -> VmResult<EvalValue> {
        use UnaryOp::*;

        let inner: EvalValue = self.evaluate_expr(expr)?;

        let out: EvalValue = match op {
            Negate => unary_switcher!(-, inner, &self.arena),
        };

        Ok(out)
    }

    #[allow(clippy::cognitive_complexity)] // False positive from macro expansions
    fn eval_binary_expr(&self, op: BinaryOp, a: ExprNode, b: ExprNode) -> VmResult<EvalValue> {
        use BinaryOp::*;

        let a: EvalValue = self.evaluate_expr(a)?;
        let b: EvalValue = self.evaluate_expr(b)?;

        let out: EvalValue = match op {
            Plus => binary_switcher!(+, a, b, &self.arena),
            Minus => binary_switcher!(-, a, b, &self.arena),
            Times => binary_switcher!(*, a, b, &self.arena),
            Divide => binary_switcher!(/, a, b, &self.arena),
        };

        Ok(out)
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
                let mut iter = vals.iter();
                write!(f, "{}", iter.next().unwrap())?;

                for next in iter {
                    write!(f, ", {}", next)?;
                }
                write!(f, "]")
            }
        }
    }
}

fn make_range(
    start: EvalValue,
    end: EvalValue,
    step: EvalValue,
    arena: &VectorStore,
) -> VmResult<EvalValue> {
    let start = get_float_helper(start, "0 (start)")?;
    let end = get_float_helper(end, "1 (end)")?;
    let step = get_float_helper(step, "2 (step)")?;

    if start < end && step <= 0. {
        return Err(EvalError::IllegalArgument(
            "Since start<end, expected argument 2 (step) to be a positive number".to_string(),
        ));
    } else if start > end && step >= 0. {
        return Err(EvalError::IllegalArgument(
            "Since start>end, expected argument 2 (step) to be a negative number".to_string(),
        ));
    }

    let len = { ((end - start) / step).ceil() as usize };

    if len > 100_000_000 {
        return Err(EvalError::IllegalArgument(format!(
            "Start/end/step=({}/{}/{}) yields a range of length {} which is not OK",
            start, end, step, len
        )));
    }

    let mut running = start;
    let mut out = arena.get_vector(len);

    for i in 0..len {
        out[i] = running;
        running += step;
    }

    Ok(out.into())
}

fn get_float_helper(f: EvalValue, arg_name: &str) -> VmResult<f64> {
    match f {
        EvalValue::Float(f) => Ok(f),
        EvalValue::FloatList(_) => Err(EvalError::TypeMismatch(format!(
            "Argument '{}' needed to be a float, but got a float list",
            arg_name
        ))),
    }
}

#[cfg(test)]
mod tests;
