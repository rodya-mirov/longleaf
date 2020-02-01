use std::collections::HashSet;
use std::convert::TryInto;
use std::rc::Rc;

use crate::internal_store::{AllocationError, MemoryUsageReport, VectorStore};
use crate::parser::{BinaryOp, BlockStmt, ExprNode, StatementNode, UnaryOp};

mod namespace;
use namespace::Namespace;

mod builtins;
use builtins::Operation;

use crate::values::{Args, LongleafValue};
use crate::MEMORY_CAPACITY;

// TODO: this is not a great way to manage this; if we forget to add stuff to this
// set, then you can have weird things (e.g. setting "true" to a value, which is
// then completely inaccessible but takes up memory)
lazy_static! {
    static ref RESERVED_WORDS: HashSet<&'static str> =
        vec!["true", "false", "sin", "cos", "tan", "exp", "ln", "range", "length", "sum", "dot"]
            .into_iter()
            .collect();
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
            "Insufficient available memory to allocate vector of size {}, requiring {} bytes",
            e.num_elements, e.mem_required
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
            arena: VectorStore::new(MEMORY_CAPACITY),
        }
    }

    pub fn get_memory_usage(&self) -> MemoryUsageReport {
        self.arena.get_memory_usage()
    }

    pub fn garbage_collect(&mut self) {
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
            ExprNode::Bool(b) => LongleafValue::Bool(b),
            ExprNode::Float(f) => LongleafValue::Float(f),
            ExprNode::BoolList(vals) => {
                LongleafValue::BoolList(Rc::new(self.arena.track_vector(vals)))
            }
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

        if let Ok(op) = (&name as &str).try_into() {
            let op: Box<dyn Operation> = op;
            if args.len() != op.num_args() {
                return Err(EvalError::WrongNumArgs(format!(
                    "For function {}, expected {} args, but got {}",
                    name_str,
                    op.num_args(),
                    args.len()
                )));
            }

            let mut results = Vec::with_capacity(op.num_args());

            for arg in args {
                results.push(self.evaluate_expr(arg)?);
            }

            return op.process(results, &mut self.arena);
        }

        let num_args: usize = match self.variable_definitions.lookup_variable(name_str) {
            None => {
                return Err(EvalError::UnknownVariable(name_str.to_string()));
            }
            Some(LongleafValue::FunctionDefinition(fn_args, _expr)) => fn_args.names.len(),
            Some(other) => {
                let type_name = other.type_name();
                return Err(EvalError::TypeMismatch(format!(
                    "Name {} is associated to a {}, but needed a function",
                    name_str, type_name
                )));
            }
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
        let inner: LongleafValue = self.evaluate_expr(expr)?;

        let op: Box<dyn Operation> = op.into();
        // TODO: this extra vec allocation is gross but fixing it
        // is such a non-issue that I can't afford the extra LoC for it
        // right now
        op.process(vec![inner], &mut self.arena)
    }

    #[allow(clippy::cognitive_complexity)] // False positive from macro expansions
    fn eval_binary_expr(
        &mut self,
        op: BinaryOp,
        a: ExprNode,
        b: ExprNode,
    ) -> VmResult<LongleafValue> {
        let a: LongleafValue = self.evaluate_expr(a)?;
        let b: LongleafValue = self.evaluate_expr(b)?;

        let op: Box<dyn Operation> = op.into();
        op.process(vec![a, b], &mut self.arena)
    }
}

#[cfg(test)]
mod tests;
