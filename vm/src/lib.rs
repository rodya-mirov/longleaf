use std::{convert::TryFrom, io::Write};

use compiler::{chunk::Chunk, ops::OpCode, Value};

pub struct VM {
    chunk: Chunk,
    // pointer to an instruction in chunk.code
    ip: usize,
    stack: Stack,
}

mod stack {
    use super::{CompilerErrorKind, InterpretError, InterpretResult, RuntimeErrorKind};
    use compiler::Value;

    const STACK_SIZE: usize = 10_000;

    pub struct Stack {
        stack: Box<[Value; STACK_SIZE]>,
        // Next unused stack value
        stack_p: usize,
    }

    impl Stack {
        #[inline]
        pub fn new() -> Self {
            let stack = Box::new([Value::Nil; STACK_SIZE]);
            Self { stack, stack_p: 0 }
        }

        #[inline(always)]
        pub fn push<T: Into<Value>>(&mut self, next: T) -> InterpretResult {
            if self.stack_p >= STACK_SIZE {
                return Err(InterpretError::RuntimeError(
                    RuntimeErrorKind::StackOverflow,
                ));
            }

            self.stack[self.stack_p] = next.into();
            self.stack_p += 1;
            Ok(())
        }

        #[inline(always)]
        pub fn pop(&mut self) -> InterpretResult<Value> {
            if self.stack_p > 0 {
                self.stack_p -= 1;
                Ok(self.stack[self.stack_p])
            } else {
                Err(InterpretError::CompileError(
                    CompilerErrorKind::StackUnderflow,
                ))
            }
        }

        #[inline(always)]
        pub fn peek(&self, dist: usize) -> InterpretResult<Value> {
            if dist > self.stack_p {
                Err(InterpretError::CompileError(
                    CompilerErrorKind::StackUnderflow,
                ))
            } else {
                Ok(self.stack[self.stack_p - dist])
            }
        }
    }

    pub struct ValueIter<'a> {
        stack: &'a Stack,
        ptr: usize,
    }

    impl<'a> IntoIterator for &'a Stack {
        type Item = Value;
        type IntoIter = ValueIter<'a>;

        fn into_iter(self) -> Self::IntoIter {
            ValueIter {
                ptr: 0,
                stack: self,
            }
        }
    }

    impl<'a> Iterator for ValueIter<'a> {
        type Item = Value;

        fn next(&mut self) -> Option<Self::Item> {
            if self.ptr >= self.stack.stack_p {
                None
            } else {
                let out = self.stack.stack[self.ptr];
                self.ptr += 1;
                Some(out)
            }
        }
    }
}

use stack::Stack;

#[derive(Debug)]
pub enum InterpretError {
    // Something bad happened to the compiled code; this is a longleaf bug
    CompileError(CompilerErrorKind),
    // Something bad happened in the user code; this is a user bug
    RuntimeError(RuntimeErrorKind),
}

// These are all indicative of compiler bugs
#[derive(Debug)]
pub enum CompilerErrorKind {
    StackUnderflow,
}

// These are all possible from user code bugs or other issues at runtime
#[derive(Debug)]
pub enum RuntimeErrorKind {
    StackOverflow,
    // TODO: probably more debug output would be helpful, this happens a lot
    WrongType,
    ArithmeticException(ArithmeticException),
    IoError(std::io::Error),
}

#[derive(Debug)]
pub enum ArithmeticException {
    DivByZero,
}

impl From<std::io::Error> for InterpretError {
    fn from(e: std::io::Error) -> Self {
        InterpretError::RuntimeError(RuntimeErrorKind::IoError(e))
    }
}

impl From<ArithmeticException> for InterpretError {
    fn from(e: ArithmeticException) -> Self {
        InterpretError::RuntimeError(RuntimeErrorKind::ArithmeticException(e))
    }
}

impl From<RuntimeErrorKind> for InterpretError {
    fn from(e: RuntimeErrorKind) -> Self {
        InterpretError::RuntimeError(e)
    }
}

impl From<CompilerErrorKind> for InterpretError {
    fn from(e: CompilerErrorKind) -> Self {
        InterpretError::CompileError(e)
    }
}

pub type InterpretResult<T = ()> = Result<T, InterpretError>;

impl VM {
    pub fn interpret<W: Write>(chunk: Chunk, w: &mut W) -> InterpretResult {
        let mut vm = VM {
            chunk,
            ip: 0,
            stack: Stack::new(),
        };
        vm.run(w)
    }

    fn run<W: Write>(&mut self, w: &mut W) -> InterpretResult {
        loop {
            let instr: u8 = self.chunk.read_byte(self.ip);
            self.ip += 1;

            #[cfg(any(test, feature = "verbose"))]
            {
                // Dump the stack ...
                write!(w, "          ")?;
                for val in &self.stack {
                    write!(w, "[ {:.03} ]", val)?;
                }
                write!(w, "\n")?;
                // ... then disassemble the current instruction
                compiler::debug::disassemble_instr(&self.chunk, self.ip, w)?;
            }

            // TODO: jump table? super fast byte math? idk but this is the perf-intensive part
            let instr: OpCode = OpCode::try_from(instr).unwrap();
            match instr {
                OpCode::OP_RETURN => {
                    // TODO: probably not what we want
                    return Ok(());
                }
                OpCode::OP_PRINT => {
                    let val = self.stack.pop()?;
                    write!(w, "{:.03}\n", val)?;
                }
                OpCode::OP_POP => {
                    self.stack.pop()?;
                }
                OpCode::OP_CONSTANT => {
                    let val = self.read_constant()?;
                    self.stack.push(val)?;
                }
                OpCode::OP_NIL => {
                    self.stack.push(())?;
                }
                OpCode::OP_FALSE => {
                    self.stack.push(false)?;
                }
                OpCode::OP_TRUE => {
                    self.stack.push(true)?;
                }
                OpCode::OP_NEGATE => {
                    let val = self.stack.pop()?;
                    let val = -as_number(val)?;
                    self.stack.push(val)?;
                }
                OpCode::OP_NOT => {
                    let val = self.stack.pop()?;
                    let val = !as_bool(val)?;
                    self.stack.push(val)?;
                }
                OpCode::OP_ADD => {
                    // TODO: here and throughout; when we do GC, it may be important to keep these on the stack while the operation is going (???)
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    let val = a + b;
                    self.stack.push(val)?;
                }
                OpCode::OP_SUBTRACT => {
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    let val = a - b;
                    self.stack.push(val)?;
                }
                OpCode::OP_MULTIPLY => {
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    let val = a * b;
                    self.stack.push(val)?;
                }
                OpCode::OP_DIVIDE => {
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    if b == 0.0 {
                        return Err(ArithmeticException::DivByZero)?;
                    }
                    let val = a / b;
                    self.stack.push(val)?;
                }
                OpCode::OP_GEQ => {
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    let val = a >= b;
                    self.stack.push(val)?;
                }
                OpCode::OP_GT => {
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    let val = a > b;
                    self.stack.push(val)?;
                }
                OpCode::OP_EQ => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    let val = check_eq(a, b)?;
                    self.stack.push(val)?;
                }
                OpCode::OP_NEQ => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    let val = check_neq(a, b)?;
                    self.stack.push(val)?;
                }
                OpCode::OP_LT => {
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    let val = a < b;
                    self.stack.push(val)?;
                }
                OpCode::OP_LEQ => {
                    let b = as_number(self.stack.pop()?)?;
                    let a = as_number(self.stack.pop()?)?;
                    let val = a <= b;
                    self.stack.push(val)?;
                }
            }
        }
    }

    #[inline(always)]
    fn read_constant(&mut self) -> InterpretResult<Value> {
        // TODO: if this is out of range, throw a CompileError? idk if I want safety checks
        let ind: u8 = self.chunk.read_byte(self.ip);
        self.ip += 1;
        // TODO: if this is out of range, throw a CompileError? idk if I want safety checks
        let val: Value = self.chunk.get_constant(ind as usize);
        Ok(val)
    }
}

fn check_eq(a: Value, b: Value) -> InterpretResult<bool> {
    match a {
        Value::Number(a) => match b {
            Value::Number(b) => Ok(a == b),
            _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
        },
        Value::Bool(a) => match b {
            Value::Bool(b) => Ok(a == b),
            _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
        },
        Value::Nil => match b {
            Value::Nil => Ok(true),
            _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
        },
    }
}

fn check_neq(a: Value, b: Value) -> InterpretResult<bool> {
    match a {
        Value::Number(a) => match b {
            Value::Number(b) => Ok(a != b),
            _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
        },
        Value::Bool(a) => match b {
            Value::Bool(b) => Ok(a != b),
            _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
        },
        Value::Nil => match b {
            Value::Nil => Ok(false),
            _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
        },
    }
}

#[inline(always)]
fn as_number(v: Value) -> InterpretResult<f64> {
    match v {
        Value::Number(f) => Ok(f),
        _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
    }
}

#[inline(always)]
fn as_bool(v: Value) -> InterpretResult<bool> {
    match v {
        Value::Bool(b) => Ok(b),
        _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
    }
}
