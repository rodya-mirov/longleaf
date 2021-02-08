use std::{convert::TryFrom, io::Write};

use compiler::{chunk::Chunk, ops::OpCode, Value};

pub struct VM {
    chunk: Chunk,
    // pointer to an instruction in chunk.code
    ip: usize,
}

#[derive(Debug)]
pub enum InterpretError {
    // Something bad happened to the compiled code; this is a longleaf bug
    CompileError,
    // Something bad happened in the user code; this is a user bug
    RuntimeError,
    // IO shit
    IoError(std::io::Error),
}

impl From<std::io::Error> for InterpretError {
    fn from(e: std::io::Error) -> Self {
        InterpretError::IoError(e)
    }
}

pub type InterpretResult<T = ()> = Result<T, InterpretError>;

impl VM {
    pub fn interpret<W: Write>(chunk: Chunk, w: &mut W) -> InterpretResult {
        let mut vm = VM { chunk, ip: 0 };
        vm.run(w)
    }

    fn run<W: Write>(&mut self, w: &mut W) -> InterpretResult {
        loop {
            let instr: u8 = self.chunk.read_byte(self.ip);
            self.ip += 1;

            // TODO: jump table? super fast byte math? idk
            let instr: OpCode = OpCode::try_from(instr).unwrap();
            match instr {
                OpCode::OP_RETURN => {
                    return Ok(());
                }
                OpCode::OP_CONSTANT => {
                    let val = self.read_constant()?;
                    // TODO: definitely not how we want to actually handle constants
                    write!(w, "{:.03}\n", val)?;
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
