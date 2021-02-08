use compiler::{chunk::Chunk, ops::OpCode};

#[test]
pub fn simple_test() {
    let mut w = Vec::new();

    let mut chunk = Chunk::new();

    let ci = chunk.add_constant(1.2);

    chunk.write_chunk(OpCode::OP_CONSTANT as u8, 0);
    chunk.write_chunk(ci as u8, 0);

    chunk.write_chunk(OpCode::OP_RETURN as u8, 1);

    vm::VM::interpret(chunk, &mut w).unwrap();

    let actual = String::from_utf8(w).unwrap();
    let expected = "1.200\n";

    assert_eq!(&actual, expected);
}
