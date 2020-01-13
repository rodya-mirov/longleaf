use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use crate::parser::*;

#[macro_use]
mod macros;

mod store;
use store::VectorStore;

mod namespace;
use namespace::Namespace;

mod values;
use values::{EvalValue, FloatListValue, PrimitiveValue};

const PAR_CHUNK_LEN: usize = 128; // TODO: find the right chunk length

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

pub struct VM {
    variable_definitions: Namespace,
    arena: VectorStore,
}

impl VM {
    pub fn new() -> Self {
        VM {
            variable_definitions: Namespace::new(),
            arena: VectorStore::new(),
        }
    }

    fn define_variable<T>(&mut self, name: &str, value: T) -> VmResult<()>
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

        self.variable_definitions
            .define_variable(name, value.into());
        Ok(())
    }

    pub fn evaluate_statement(&mut self, stmt: StatementNode) -> VmResult<()> {
        match stmt {
            StatementNode::VarDefn(name, expr) => {
                let val = self.evaluate_expr(expr)?;
                self.define_variable(&name, val)
            }
        }
    }

    pub fn evaluate_expr(&mut self, expr: ExprNode) -> VmResult<EvalValue> {
        let out: EvalValue = match expr {
            ExprNode::Float(f) => EvalValue::Float(f),
            // TODO: This double-indirection is embarassing
            ExprNode::FloatList(vals) => EvalValue::FloatList(Box::new(Box::new(vals))),
            ExprNode::FunctionCall(name, args) => self.eval_function_call(name, args)?,
            ExprNode::UnaryExpr(op, val) => self.eval_unary_expr(op, *val)?,
            ExprNode::BinaryExpr(op, a, b) => self.eval_binary_expr(op, *a, *b)?,
            ExprNode::FunctionDefn(args, body) => EvalValue::FunctionDefinition(
                Rc::new(values::Args { names: args.0 }),
                Rc::new(body),
            ),
            ExprNode::VariableRef(id) => {
                let stored = self.variable_definitions.lookup_variable(&id);
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

    fn eval_function_call(&mut self, name: String, args: Vec<ExprNode>) -> VmResult<EvalValue> {
        let name_str: &str = &name;

        let num_args: usize = match name_str {
            "sin" => 1,
            "cos" => 1,
            "tan" => 1,
            "range" => 3,
            other => match self.variable_definitions.lookup_variable(other) {
                None => {
                    return Err(EvalError::UnknownVariable(other.to_string()));
                }
                Some(PrimitiveValue::FunctionDefinition(fn_args, _expr)) => fn_args.names.len(),
                Some(PrimitiveValue::Float(_)) => {
                    return Err(EvalError::TypeMismatch(format!(
                        "Name {} is associated to a float, but needed a function",
                        name_str
                    )));
                }
                Some(PrimitiveValue::FloatList(_)) => {
                    return Err(EvalError::TypeMismatch(format!(
                        "Name {} is associated to a float list, but needed a function",
                        name_str
                    )));
                }
            },
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

        match name_str {
            "sin" => unary_fn_switcher!(<f64>::sin, results.pop().unwrap(), &self.arena),
            "cos" => unary_fn_switcher!(<f64>::cos, results.pop().unwrap(), &self.arena),
            "tan" => unary_fn_switcher!(<f64>::tan, results.pop().unwrap(), &self.arena),
            "range" => {
                let step = results.pop().unwrap();
                let end = results.pop().unwrap();
                let start = results.pop().unwrap();
                make_range(start, end, step, &self.arena)
            }
            other => {
                let (fn_args, fn_expr) = match self.variable_definitions.lookup_variable(other) {
                    Some(PrimitiveValue::FunctionDefinition(fn_args, fn_expr)) => {
                        (fn_args.clone(), fn_expr.clone())
                    }
                    _ => unreachable!(),
                };

                self.variable_definitions.start_call();

                for (arg_name, arg_val) in fn_args.names.iter().zip(results.into_iter()) {
                    self.variable_definitions
                        .define_variable(arg_name.to_string(), arg_val.into());
                }

                // TODO: make this easier to clone?
                let res = self.evaluate_expr(fn_expr.as_ref().clone());

                self.variable_definitions.end_call();

                res
            }
        }
    }

    fn eval_unary_expr(&mut self, op: UnaryOp, expr: ExprNode) -> VmResult<EvalValue> {
        use UnaryOp::*;

        let inner: EvalValue = self.evaluate_expr(expr)?;

        match op {
            Negate => unary_switcher!(-, inner, &self.arena),
        }
    }

    #[allow(clippy::cognitive_complexity)] // False positive from macro expansions
    fn eval_binary_expr(&mut self, op: BinaryOp, a: ExprNode, b: ExprNode) -> VmResult<EvalValue> {
        use BinaryOp::*;

        let a: EvalValue = self.evaluate_expr(a)?;
        let b: EvalValue = self.evaluate_expr(b)?;

        match op {
            Plus => binary_switcher!(+, a, b, &self.arena),
            Minus => binary_switcher!(-, a, b, &self.arena),
            Times => binary_switcher!(*, a, b, &self.arena),
            Divide => binary_switcher!(/, a, b, &self.arena),
        }
    }
}

impl fmt::Display for PrimitiveValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            PrimitiveValue::Float(val) => write!(f, "{}", val),
            PrimitiveValue::FunctionDefinition(args, _expr) => {
                write!(f, "Function of {} arguments", args.names.len())
            }
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
        EvalValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
            "Argument '{}' needed to be a float, but got a function",
            arg_name
        ))),
    }
}

#[cfg(test)]
mod tests;
