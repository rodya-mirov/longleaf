use compiler::ops::OpCode::OP_CONSTANT;
use compiler::{chunk::Chunk, debug, ops::OpCode};

#[test]
pub fn simple_it() {
    let mut chunk = Chunk::new();
    chunk.write_chunk(OpCode::OP_RETURN as u8, 10);

    chunk.add_constant(1.25);
    chunk.write_chunk(OP_CONSTANT as u8, 12);
    chunk.write_chunk(0, 12);

    chunk.add_constant(1.5);
    chunk.write_chunk(OP_CONSTANT as u8, 12);
    chunk.write_chunk(1, 12);

    let mut writer = Vec::new();
    debug::disassemble_chunk(&chunk, "main", &mut writer).unwrap();

    let expected = "== main ==\n\
                    0000 0010 OP_RETURN\n\
                    0001 0012 OP_CONSTANT 0000 '1.250'\n\
                    0003    | OP_CONSTANT 0001 '1.500'\n";

    let actual = String::from_utf8(writer).unwrap();

    assert_eq!(&actual, expected);
}
