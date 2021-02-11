use compiler::{chunk::Chunk, ops::OpCode};

mod helpers;

#[test]
pub fn simple_direct_test() {
    let mut w = Vec::new();

    let mut chunk = Chunk::new();

    let ci0 = chunk.add_constant(1.2);
    let ci1 = chunk.add_constant(2.4);
    let ci2 = chunk.add_constant(5.0);

    // return -(1.2 / 2.4) + 5.0;
    chunk.write_chunk(OpCode::OP_CONSTANT as u8, 0);
    chunk.write_chunk(ci0 as u8, 0);

    chunk.write_chunk(OpCode::OP_CONSTANT as u8, 0);
    chunk.write_chunk(ci1 as u8, 0);

    chunk.write_chunk(OpCode::OP_DIVIDE as u8, 0);

    chunk.write_chunk(OpCode::OP_NEGATE as u8, 0);

    chunk.write_chunk(OpCode::OP_CONSTANT as u8, 0);
    chunk.write_chunk(ci2 as u8, 0);

    chunk.write_chunk(OpCode::OP_ADD as u8, 0);

    chunk.write_chunk(OpCode::OP_PRINT as u8, 0);
    chunk.write_chunk(OpCode::OP_RETURN as u8, 1);

    vm::VM::interpret(chunk, &mut w).unwrap();

    let actual = String::from_utf8(w).unwrap();
    let expected = "4.500\n";

    assert_eq!(&actual, expected);
}

#[test]
pub fn simple_interp_test() {
    let text = "print 12 / (1.2 + -3);";
    let expected = "-6.667\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn simple_interp_test_2() {
    let text = "print nil;";
    let expected = "nil\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn simple_interp_test_3() {
    let text = "print false;";
    let expected = "false\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn simple_interp_test_4() {
    let text = "print true;";
    let expected = "true\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn simple_interp_test_5() {
    let text = "print !true;";
    let expected = "false\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn simple_interp_test_6() {
    let text = "print (1.2 > 1.3);";
    let expected = "false\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn string_test_0() {
    let text = "print \"ugh whatever\";";
    let expected = "ugh whatever\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn string_test_1() {
    let text = r#"print ("a" + "foo" + "bar");"#;
    let expected = "afoobar\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn string_test_2() {
    let text = r#"print ("a"+"b"+"cd");"#;
    let expected = "abcd\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn string_test_3() {
    let text = r#"print ("abc" + "d");"#;
    let expected = "abcd\n";
    let actual = helpers::run_and_collect_output(text);

    assert_eq!(expected, actual);
}

#[test]
pub fn string_eq_test_0() {
    let text = r#"print ("a"+"b"+"cd") == ("abc" + "d");"#;
    let expected = "true\n";
    let actual = helpers::run_and_collect_output(text);

    if expected != actual {
        println!("Expected:\n{}\n", expected);
        println!("Actual:\n{}\n", actual);
    }

    assert_eq!(expected, actual);
}
