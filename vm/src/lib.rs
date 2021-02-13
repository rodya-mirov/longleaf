use std::{collections::HashMap, convert::TryFrom, io::Write};

use compiler::{chunk::Chunk, ops::OpCode, Obj, ObjString, Value};

pub trait VMIO {
    type PrintWriter: Write;
    type ErrWriter: Write;
    type DebugWriter: Write;

    fn print_writer(&mut self) -> &mut Self::PrintWriter;
    fn err_writer(&mut self) -> &mut Self::ErrWriter;
    fn debug_writer(&mut self) -> &mut Self::DebugWriter;
}

pub struct VM {
    chunk: Chunk,
    // pointer to an instruction in chunk.code
    ip: usize,
    stack: Stack,
    // TODO http://www.craftinginterpreters.com/global-variables.html reuses string pointers
    // I'm not sure if it's possible, given rust's move semantics, to avoid this clone
    // I'm also not sure I care; the number of globals should be smallish
    globals: HashMap<String, Value>,
    objects: Heap,
}

mod heap {
    use compiler::Obj;

    pub struct Heap {
        owned: Vec<*mut Obj>,
    }

    impl Heap {
        pub fn new() -> Self {
            Self { owned: Vec::new() }
        }

        pub fn track(&mut self, obj: *mut Obj) {
            self.owned.push(obj);
        }
    }

    impl Drop for Heap {
        fn drop(&mut self) {
            while let Some(obj) = self.owned.pop() {
                unsafe {
                    #[cfg(any(test, feature = "verbose"))]
                    {
                        let o = obj.as_ref().unwrap();
                        match o {
                            Obj::ObjString(s) => println!("Dropping string '{}'", s.val),
                        }
                    }
                    std::ptr::drop_in_place(obj)
                }
            }
        }
    }
}

use heap::Heap;

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
        pub fn set(&mut self, idx: usize, val: Value) -> InterpretResult {
            if idx >= self.stack_p {
                return Err(InterpretError::RuntimeError(
                    RuntimeErrorKind::OutOfRangeLocalIdx,
                ));
            }
            self.stack[idx] = val;
            Ok(())
        }

        #[inline(always)]
        pub fn get(&mut self, idx: usize) -> InterpretResult<Value> {
            if idx >= self.stack_p {
                return Err(InterpretError::RuntimeError(
                    RuntimeErrorKind::OutOfRangeLocalIdx,
                ));
            }
            Ok(self.stack[idx])
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
            if self.stack_p == 0 || dist > self.stack_p - 1 {
                Err(InterpretError::CompileError(
                    CompilerErrorKind::StackUnderflow,
                ))
            } else {
                Ok(self.stack[self.stack_p - 1 - dist])
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
    OutOfRangeLocalIdx,
    StackOverflow,
    UndefinedGlobal,
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
    pub fn interpret<W: VMIO>(chunk: Chunk, w: &mut W) -> InterpretResult {
        VM::new_with(chunk).run(w)
    }

    pub fn new_with(chunk: Chunk) -> Self {
        VM {
            chunk,
            ip: 0,
            globals: HashMap::new(),
            stack: Stack::new(),
            objects: Heap::new(),
        }
    }

    pub fn new() -> Self {
        Self::new_with(Chunk::default())
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    pub fn run<W: VMIO>(&mut self, w: &mut W) -> InterpretResult {
        loop {
            if self.ip >= self.chunk.len() {
                break Ok(());
            }

            let instr: u8 = self.chunk.read_byte(self.ip);

            #[cfg(any(test, feature = "verbose"))]
            {
                // Dump the stack ...
                write!(w.debug_writer(), "    [STACK STATE]: ")?;
                for val in &self.stack {
                    write!(w.debug_writer(), "[ {:.03} ]", val)?;
                }
                write!(w.debug_writer(), "\n")?;
                // ... then disassemble the current instruction
                write!(w.debug_writer(), "Current instruction:\n  ")?;
                compiler::debug::disassemble_instr(&self.chunk, self.ip, w.debug_writer())?;
            }

            self.ip += 1;

            // TODO: jump table? super fast byte math? idk but this is the perf-intensive part
            let instr: OpCode = OpCode::try_from(instr).unwrap();
            match instr {
                OpCode::OP_RETURN => {
                    // TODO: probably not what we want
                    return Ok(());
                }
                OpCode::OP_PRINT => {
                    let val = self.stack.pop()?;
                    write!(w.print_writer(), "{:.03}\n", val)?;
                }
                OpCode::OP_POP => {
                    self.stack.pop()?;
                }
                OpCode::OP_POP_SWAP => {
                    let keep = self.stack.pop()?;
                    self.stack.pop()?;
                    self.stack.push(keep)?;
                }
                OpCode::OP_CONSTANT => {
                    let val = self.read_constant()?;
                    self.stack.push(val)?;
                }
                OpCode::OP_SET_LOCAL => {
                    // This is leftover; if we at some point allow mutable locals, which we will,
                    // then this should allow them to work correctly
                    #[allow(unreachable_code)]
                    {
                        unimplemented!("Local mutation is not correctly supported");
                        let stack_idx = self.chunk.read_byte(self.ip);
                        self.ip += 1;
                        let val = self.stack.peek(0)?;
                        self.stack.set(stack_idx as usize, val)?;
                    }
                }
                OpCode::OP_GET_LOCAL => {
                    let stack_idx = self.chunk.read_byte(self.ip);
                    self.ip += 1;
                    let val = self.stack.get(stack_idx as usize)?;
                    self.stack.push(val)?;
                }
                OpCode::OP_DEFINE_GLOBAL => {
                    let val = self.read_constant()?;
                    let name: &ObjString = as_string(val)?;
                    self.globals.insert(name.val.clone(), self.stack.peek(0)?);
                    self.stack.pop()?;
                }
                OpCode::OP_GET_GLOBAL => {
                    let val = self.read_constant()?;
                    let name: &ObjString = as_string(val)?;
                    match self.globals.get(&name.val) {
                        Some(val) => self.stack.push(*val)?,
                        None => {
                            return Err(InterpretError::RuntimeError(
                                RuntimeErrorKind::UndefinedGlobal,
                            ))
                        }
                    }
                }
                OpCode::OP_JUMP_IF_FALSE => {
                    let peeked = self.stack.peek(0)?;
                    if is_bool(peeked) {
                        let offset: u16 = self.read_u16()?;
                        if !as_bool(peeked)? {
                            self.ip += offset as usize;
                        }
                    } else {
                        write!(w.err_writer(), "Cannot jump on a non-bool\n")?;
                        return Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType));
                    }
                }
                OpCode::OP_JUMP => {
                    let offset: u16 = self.read_u16()?;
                    self.ip += offset as usize;
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
                    let (a, b) = (self.stack.peek(1)?, self.stack.peek(0)?);
                    if is_string(a) && is_string(b) {
                        self.stack.pop()?;
                        self.stack.pop()?;
                        let b = as_string(b)?;
                        let a = as_string(a)?;
                        let mut new_str: String = a.val.clone();
                        new_str.push_str(b.val.as_str());
                        let val = Obj::ObjString(ObjString { val: new_str });
                        let val_ptr = Box::into_raw(Box::new(val));
                        self.objects.track(val_ptr);
                        let val = Value::Object(val_ptr);
                        self.stack.push(val)?;
                    } else if is_number(a) && is_number(b) {
                        self.stack.pop()?;
                        self.stack.pop()?;
                        let val = as_number(a)? + as_number(b)?;
                        self.stack.push(val)?;
                    } else {
                        write!(w.err_writer(), "Cannot add values {:?} and {:?}", a, b)?;
                        return Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType));
                    }
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

    #[inline(always)]
    fn read_u16(&mut self) -> InterpretResult<u16> {
        let offset: u16 = ((self.chunk.read_byte(self.ip) as u16) << 8)
            + self.chunk.read_byte(self.ip + 1) as u16;
        self.ip += 2;
        Ok(offset)
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
        Value::Object(a) => {
            match b {
                Value::Object(b) => {
                    // Quick optimization; in the (common?) case of two references pointing
                    // to exactly the same object, we don't need to deref or anything
                    if std::ptr::eq(a, b) {
                        return Ok(true);
                    }

                    match unsafe { a.as_ref().unwrap() } {
                        Obj::ObjString(a) => match unsafe { b.as_ref().unwrap() } {
                            Obj::ObjString(b) => {
                                println!("{} and {}", a.val, b.val);
                                Ok(&a.val == &b.val)
                            }
                        },
                    }
                }
                _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
            }
        }
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
        Value::Object(a) => {
            match b {
                Value::Object(b) => {
                    // Quick optimization; in the (common?) case of two references pointing
                    // to exactly the same object, we don't need to deref or anything
                    if std::ptr::eq(a, b) {
                        return Ok(false);
                    }

                    match unsafe { a.as_ref().unwrap() } {
                        Obj::ObjString(a) => match unsafe { b.as_ref().unwrap() } {
                            Obj::ObjString(b) => {
                                println!("{} and {}", a.val, b.val);
                                Ok(&a.val != &b.val)
                            }
                        },
                    }
                }
                _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
            }
        }
    }
}

#[inline(always)]
fn is_string(v: Value) -> bool {
    match v {
        Value::Object(o) => match unsafe { o.as_ref().expect("GC Pointer should be valid") } {
            Obj::ObjString(_) => true,
        },
        _ => false,
    }
}

#[inline(always)]
fn is_number(v: Value) -> bool {
    match v {
        Value::Number(_) => true,
        _ => false,
    }
}

#[inline(always)]
fn is_bool(v: Value) -> bool {
    match v {
        Value::Bool(_) => true,
        _ => false,
    }
}

// TODO: probably refactor this to take a closure
#[inline(always)]
fn as_string<'a>(v: Value) -> InterpretResult<&'a ObjString> {
    match v {
        Value::Object(o) => match unsafe { o.as_ref().expect("GC Pointer should be valid") } {
            Obj::ObjString(s) => Ok(s),
        },
        _ => Err(InterpretError::RuntimeError(RuntimeErrorKind::WrongType)),
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
