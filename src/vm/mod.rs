use std::collections::HashSet;
use std::rc::Rc;

use crate::parser::{BinaryOp, BlockStmt, ExprNode, StatementNode, UnaryOp};

#[macro_use]
mod eval_macros;

use crate::vector_store::{AllocationError, MemoryUsageReport, TrackedVector, VectorStore};

mod namespace;
use namespace::Namespace;

use crate::values::{Args, LongleafValue};

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
    NoReturn(String),
    OutOfMemory(String),
}

impl From<AllocationError> for EvalError {
    fn from(e: AllocationError) -> EvalError {
        EvalError::OutOfMemory(format!(
            "Insufficient available memory to allocate vector of size {}",
            e.length
        ))
    }
}

pub enum StatementOutput {
    NoOutput,
    Returned(LongleafValue),
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

    pub fn get_memory_usage(&self) -> MemoryUsageReport {
        self.arena.get_memory_usage()
    }

    pub fn garbage_collect(&self) {
        self.arena.garbage_collect();
    }

    pub fn evaluate_statement(&mut self, stmt: StatementNode) -> VmResult<StatementOutput> {
        match stmt {
            StatementNode::VarDefn(name, expr) => {
                let val = self.evaluate_expr(expr)?;
                self.define_variable(&name, val)?;
                Ok(StatementOutput::NoOutput)
            }
            StatementNode::ReturnStmt(expr) => {
                let val = self.evaluate_expr(expr)?;
                Ok(StatementOutput::Returned(val))
            }
            StatementNode::BlockStmt(block) => self.evaluate_block_stmt(block),
        }
    }

    fn evaluate_block_stmt(&mut self, block: BlockStmt) -> VmResult<StatementOutput> {
        for stmt in block.stmts {
            match self.evaluate_statement(stmt.clone())? {
                StatementOutput::NoOutput => {}
                StatementOutput::Returned(val) => {
                    self.variable_definitions.end_call();
                    return Ok(StatementOutput::Returned(val));
                }
            }
        }

        self.variable_definitions.end_call();
        Ok(StatementOutput::NoOutput)
    }

    pub fn evaluate_expr(&mut self, expr: ExprNode) -> VmResult<LongleafValue> {
        let out: LongleafValue = match expr {
            ExprNode::Float(f) => LongleafValue::Float(f),
            ExprNode::FloatList(vals) => {
                LongleafValue::FloatList(Rc::new(self.arena.track_vector(vals)))
            }
            ExprNode::FunctionCall(name, args) => self.eval_function_call(name, args)?,
            ExprNode::UnaryExpr(op, val) => self.eval_unary_expr(op, *val)?,
            ExprNode::BinaryExpr(op, a, b) => self.eval_binary_expr(op, *a, *b)?,
            ExprNode::FunctionDefn(args, body) => {
                LongleafValue::FunctionDefinition(Rc::new(Args { names: args.0 }), Rc::new(body))
            }
            ExprNode::VariableRef(id) => {
                let stored = self.variable_definitions.lookup_variable(&id);
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

    fn define_variable<T>(&mut self, name: &str, value: T) -> VmResult<()>
    where
        T: Into<LongleafValue>,
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

    fn eval_function_call(&mut self, name: String, args: Vec<ExprNode>) -> VmResult<LongleafValue> {
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
                Some(LongleafValue::FunctionDefinition(fn_args, _expr)) => fn_args.names.len(),
                Some(LongleafValue::Float(_)) => {
                    return Err(EvalError::TypeMismatch(format!(
                        "Name {} is associated to a float, but needed a function",
                        name_str
                    )));
                }
                Some(LongleafValue::FloatList(_)) => {
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
                let (fn_args, fn_body) = match self.variable_definitions.lookup_variable(other) {
                    Some(LongleafValue::FunctionDefinition(fn_args, fn_body)) => {
                        (fn_args.clone(), fn_body.clone())
                    }
                    _ => unreachable!(),
                };

                self.eval_user_function_call(fn_args, fn_body, results)
            }
        }
    }

    fn eval_user_function_call(
        &mut self,
        fn_args: Rc<Args>,
        body: Rc<Vec<StatementNode>>,
        arg_vals: Vec<LongleafValue>,
    ) -> VmResult<LongleafValue> {
        self.variable_definitions.start_call();

        for (arg_name, arg_val) in fn_args.names.iter().zip(arg_vals.into_iter()) {
            self.variable_definitions
                .define_variable(arg_name.to_string(), arg_val);
        }

        for stmt in body.iter() {
            match self.evaluate_statement(stmt.clone())? {
                StatementOutput::NoOutput => {}
                StatementOutput::Returned(val) => {
                    self.variable_definitions.end_call();
                    return Ok(val);
                }
            }
        }

        self.variable_definitions.end_call();

        Err(EvalError::NoReturn(
            "Function had no return statement, nothing is returned.".to_string(),
        ))
    }

    fn eval_unary_expr(&mut self, op: UnaryOp, expr: ExprNode) -> VmResult<LongleafValue> {
        use UnaryOp::*;

        let inner: LongleafValue = self.evaluate_expr(expr)?;

        match op {
            Negate => unary_switcher!(-, inner, &self.arena),
        }
    }

    #[allow(clippy::cognitive_complexity)] // False positive from macro expansions
    fn eval_binary_expr(
        &mut self,
        op: BinaryOp,
        a: ExprNode,
        b: ExprNode,
    ) -> VmResult<LongleafValue> {
        use BinaryOp::*;

        let a: LongleafValue = self.evaluate_expr(a)?;
        let b: LongleafValue = self.evaluate_expr(b)?;

        match op {
            Plus => binary_switcher!(+, a, b, &self.arena),
            Minus => binary_switcher!(-, a, b, &self.arena),
            Times => binary_switcher!(*, a, b, &self.arena),
            Divide => binary_switcher!(/, a, b, &self.arena),
        }
    }
}

fn make_range(
    start: LongleafValue,
    end: LongleafValue,
    step: LongleafValue,
    arena: &VectorStore,
) -> VmResult<LongleafValue> {
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

    let len: usize = { ((end - start) / step).ceil() as usize };

    let mut running = start;
    let mut out = arena.get_vector(len)?;

    for i in 0..len {
        out[i] = running;
        running += step;
    }

    Ok(out.into())
}

fn get_float_helper(f: LongleafValue, arg_name: &str) -> VmResult<f64> {
    match f {
        LongleafValue::Float(f) => Ok(f),
        LongleafValue::FloatList(_) => Err(EvalError::TypeMismatch(format!(
            "Argument '{}' needed to be a float, but got a float list",
            arg_name
        ))),
        LongleafValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
            "Argument '{}' needed to be a float, but got a function",
            arg_name
        ))),
    }
}

#[cfg(test)]
mod tests;
