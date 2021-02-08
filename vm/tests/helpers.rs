use compiler::ops::OpCode;

pub fn run_and_collect_output(text: &str) -> String {
    let mut out_buffer = Vec::new();

    let parsed = parser::parse_stmt_ast(text).unwrap();
    let mut ctx = compiler::compile::CompileContext::new();
    ctx.compile_stmt(parsed).unwrap();

    let mut chunk = ctx.into_chunk();

    // TODO: placeholder for actual script termination?
    chunk.write_chunk(OpCode::OP_RETURN as u8, 0);

    let ir = vm::VM::interpret(chunk, &mut out_buffer);

    let out = String::from_utf8(out_buffer).unwrap();

    match ir {
        Ok(_) => out,
        Err(e) => {
            println!("Script output: {}", out);
            println!("Interpret error: {:?}", e);
            assert!(false);
            unreachable!()
        }
    }
}
