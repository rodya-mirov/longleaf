use std::convert::TryFrom;
use std::ops::{Add as _, Div as _, Mul as _, Neg as _, Sub as _};

use crate::parser::{BinaryOp, UnaryOp};

use super::{EvalError, LongleafValue, VectorStore, VmResult};

#[macro_use]
mod builtins_macros;

mod list_builtins;
mod range;

pub trait Operation {
    fn num_args(&self) -> usize;
    fn name(&self) -> &'static str;
    fn process(&self, args: Vec<LongleafValue>, store: &mut VectorStore)
        -> VmResult<LongleafValue>;
}

impl_float_unary!(Sin, "sin", <f64>::sin);
impl_float_unary!(Cos, "cos", <f64>::cos);
impl_float_unary!(Tan, "tan", <f64>::tan);
impl_float_unary!(Neg, "-", <f64>::neg);
impl_float_unary!(Exp, "exp", <f64>::exp);
impl_float_unary!(Ln, "ln", <f64>::ln);

impl_float_binary!(Plus, "+", <f64>::add);
impl_float_binary!(Minus, "-", <f64>::sub);
impl_float_binary!(Times, "*", <f64>::mul);
impl_float_binary!(Divide, "/", <f64>::div);

fn get_only_arg(name: &str, mut args: Vec<LongleafValue>) -> VmResult<LongleafValue> {
    if args.len() != 1 {
        return Err(EvalError::WrongNumArgs(format!(
            "Function {} requires 1 argument, but got {}",
            name,
            args.len()
        )));
    }

    Ok(args.pop().unwrap())
}

fn get_two_args(
    name: &str,
    mut args: Vec<LongleafValue>,
) -> VmResult<(LongleafValue, LongleafValue)> {
    if args.len() != 2 {
        return Err(EvalError::WrongNumArgs(format!(
            "Function {} requires 2 arguments, but got {}",
            name,
            args.len()
        )));
    }

    let b = args.pop().unwrap();
    let a = args.pop().unwrap();

    Ok((a, b))
}

fn get_three_args(
    name: &str,
    mut args: Vec<LongleafValue>,
) -> VmResult<(LongleafValue, LongleafValue, LongleafValue)> {
    if args.len() != 3 {
        return Err(EvalError::WrongNumArgs(format!(
            "Function {} requires 3 arguments, but got {}",
            name,
            args.len()
        )));
    }

    let c = args.pop().unwrap();
    let b = args.pop().unwrap();
    let a = args.pop().unwrap();

    Ok((a, b, c))
}

type DynOp = Box<dyn Operation>;
type DynOpRes = Result<Box<dyn Operation>, ()>;

impl<'a> TryFrom<&'a str> for DynOp {
    type Error = ();

    fn try_from(name: &'a str) -> DynOpRes {
        match name {
            "sin" => Ok(Box::new(Sin)),
            "cos" => Ok(Box::new(Cos)),
            "tan" => Ok(Box::new(Tan)),
            "exp" => Ok(Box::new(Exp)),
            "ln" => Ok(Box::new(Ln)),

            "range" => Ok(Box::new(range::Range)),

            "length" => Ok(Box::new(list_builtins::Length)),
            "sum" => Ok(Box::new(list_builtins::Sum)),

            _ => Err(()),
        }
    }
}

impl From<UnaryOp> for DynOp {
    fn from(op: UnaryOp) -> DynOp {
        match op {
            UnaryOp::Negate => Box::new(Neg),
        }
    }
}

impl From<BinaryOp> for DynOp {
    fn from(op: BinaryOp) -> DynOp {
        match op {
            BinaryOp::Plus => Box::new(Plus),
            BinaryOp::Minus => Box::new(Minus),
            BinaryOp::Divide => Box::new(Divide),
            BinaryOp::Times => Box::new(Times),
        }
    }
}
