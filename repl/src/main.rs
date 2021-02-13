use std::io::Write;
use vm::VMIO;

fn main() {
    print!("Longleaf REPL, Version 0.0.0\n> ");
    std::io::stdout().flush().unwrap();

    let stdin = std::io::stdin();
    let mut line = String::new();

    let mut vm = vm::VM::new();

    let mut io = StdIO {
        out: std::io::stdout(),
        #[cfg(not(feature = "verbose"))]
        sink: std::io::sink(),
    };

    while let Ok(_) = stdin.read_line(&mut line) {
        match parser::parse_stmt_ast(&line.trim_end()) {
            Ok(node) => {
                let chunk = std::mem::replace(vm.chunk_mut(), Default::default());
                let mut ctx = compiler::compile::CompileContext::new_with(chunk);
                ctx.compile_stmt(node).unwrap();
                let chunk = ctx.into_chunk();

                #[cfg(feature = "verbose")]
                {
                    compiler::debug::disassemble_chunk(
                        &chunk,
                        "User Input",
                        &mut std::io::stdout(),
                    )
                    .unwrap();
                }

                *vm.chunk_mut() = chunk;
                vm.run(&mut io).unwrap();
            }
            Err(e) => {
                println!("Could not parse input as a valid longleaf statement. Please consult your reference manual for more information. Error: {:?}", e);
            }
        }

        print!("> ");
        std::io::stdout().flush().unwrap();
        line.clear();
    }
}

struct StdIO {
    out: std::io::Stdout,

    #[cfg(not(feature = "verbose"))]
    sink: std::io::Sink,
}

impl VMIO for StdIO {
    type PrintWriter = std::io::Stdout;
    type ErrWriter = std::io::Stdout;

    #[cfg(feature = "verbose")]
    type DebugWriter = std::io::Stdout;

    #[cfg(not(feature = "verbose"))]
    type DebugWriter = std::io::Sink;

    fn print_writer(&mut self) -> &mut Self::PrintWriter {
        &mut self.out
    }

    fn err_writer(&mut self) -> &mut Self::ErrWriter {
        &mut self.out
    }

    fn debug_writer(&mut self) -> &mut Self::DebugWriter {
        #[cfg(feature = "verbose")]
        {
            &mut self.out
        }

        #[cfg(not(feature = "verbose"))]
        {
            &mut self.sink
        }
    }
}
