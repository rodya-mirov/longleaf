use std::convert::TryFrom;
use std::ops::{Add as _, Div as _, Mul as _, Neg as _, Sub as _};

use crate::parser::{BinaryOp, UnaryOp};
use crate::vm::PAR_CHUNK_LEN;

use super::{EvalError, LongleafValue, VectorStore, VmResult};

#[macro_use]
mod builtins_macros;

mod range;

pub trait Operation {
    fn num_args(&self) -> usize;
    fn name(&self) -> &'static str;
    fn process(&self, args: Vec<LongleafValue>, store: &VectorStore) -> VmResult<LongleafValue>;
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Sin;

impl_float_unary!(Sin, "sin", <f64>::sin);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Cos;

impl_float_unary!(Cos, "cos", <f64>::cos);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Tan;

impl_float_unary!(Tan, "tan", <f64>::tan);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Neg;

impl_float_unary!(Neg, "-", <f64>::neg);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Plus;

impl_float_binary!(Plus, "+", <f64>::add);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Minus;

impl_float_binary!(Minus, "-", <f64>::sub);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Times;

impl_float_binary!(Times, "*", <f64>::mul);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Divide;

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
            "range" => Ok(Box::new(range::Range)),
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
