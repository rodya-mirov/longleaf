use std::io::Write;

fn main() {
    print!("Longleaf REPL, Version 0.0.0\n> ");
    std::io::stdout().flush().unwrap();

    let stdin = std::io::stdin();
    let mut line = String::new();

    let mut vm = vm::VM::new();

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
                    );
                }

                *vm.chunk_mut() = chunk;
                vm.run(&mut std::io::stdout()).unwrap();
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
