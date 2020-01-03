use super::*;

use super::BinaryOp::*;
use super::ExprNode::*;
use super::UnaryOp::*;

type ParseResult<T> = Result<T, ()>;

fn to_expr(s: &str) -> ParseResult<ExprNode> {
    let mut parsed = PestParser::parse(Rule::only_expr, s).map_err(|e| {
        eprintln!("Error parsing input {}: {}", s, e);
        ()
    })?;

    let mut pair = parsed.next().unwrap().into_inner();

    assert!(parsed.next().is_none());

    let actual = pair.next().unwrap();

    assert_eq!(pair.next().unwrap().as_rule(), Rule::EOI);
    assert!(pair.next().is_none());

    Ok(compile_expr_node(actual))
}

#[test]
fn to_expr_tests() {
    fn do_test(input: &str, expected: ExprNode) {
        let actual = to_expr(input).expect("Parse should be successful");

        assert_eq!(actual, expected);
    }

    do_test(
        "12 * 1 + 15",
        BinaryExpr(
            Plus,
            Box::new(BinaryExpr(
                Times,
                Box::new(Float(12.0)),
                Box::new(Float(1.0)),
            )),
            Box::new(Float(15.0)),
        ),
    );

    do_test(
        "12 * -1.12 + (.15 * --10.7)",
        BinaryExpr(
            Plus,
            Box::new(BinaryExpr(
                Times,
                Box::new(Float(12.0)),
                Box::new(Float(-1.12)),
            )),
            Box::new(BinaryExpr(
                Times,
                Box::new(Float(0.15)),
                // TODO: this should probably be -(-10.7) instead of -(-(10.7))
                // but I'm not super worried about it right now and I don't want
                // to rejigger the grammar yet
                Box::new(UnaryExpr(Negate, Box::new(Float(-10.7)))),
            )),
        ),
    );
}

fn to_float_list(s: &str) -> ParseResult<Vec<f64>> {
    let mut parsed = PestParser::parse(Rule::only_float_list, s).map_err(|e| {
        eprintln!("Error parsing input {}: {}", s, e);
        ()
    })?;

    let mut pair = parsed.next().unwrap().into_inner();

    assert!(parsed.next().is_none());

    let float_list = pair.next().unwrap();

    assert_eq!(pair.next().unwrap().as_rule(), Rule::EOI);
    assert!(pair.next().is_none());

    Ok(compile_float_list(float_list))
}

#[test]
fn float_vec_tests() {
    fn do_test(input: &str, expected: &[f64]) {
        let actual = to_float_list(input).expect("Parse should be successful");

        let ar: &[f64] = &actual;

        assert_eq!(ar, expected);
    }

    do_test("[]", &[]);
    do_test("[,]", &[]);

    do_test("[1.0 ]", &[1.0]);
    do_test("[1.]", &[1.0]);

    do_test("[1123]", &[1123.0]);
    do_test("[0.0132,]", &[0.0132]);
    do_test("[-.0132]", &[-0.0132]);

    do_test("[0.0132,-123.1]", &[0.0132, -123.1]);
    do_test("[0.0132,-123.1,]", &[0.0132, -123.1]);

    do_test("[0.0132, -123.1]", &[0.0132, -123.1]);
    do_test("[0.0132, -123.1,]", &[0.0132, -123.1]);
}

fn to_float(s: &str) -> ParseResult<f64> {
    let mut parsed = PestParser::parse(Rule::only_float, s).map_err(|e| {
        eprintln!("Error parsing input {}: {}", s, e);
        ()
    })?;

    let mut pair = parsed.next().unwrap().into_inner();

    assert!(parsed.next().is_none());

    let float = pair.next().unwrap();

    assert_eq!(pair.next().unwrap().as_rule(), Rule::EOI);
    assert!(pair.next().is_none());

    Ok(compile_float(float))
}

#[test]
fn float_tests() {
    fn do_test(input: &str, expected: f64) {
        let actual = to_float(input).expect("Parse should be successful");

        assert_eq!(actual, expected);
    }

    do_test(" 1.0", 1.0);
    do_test("1.0", 1.0);
    do_test("1.", 1.0);

    do_test("1123", 1123.0);
    do_test("0.0132", 0.0132);
    do_test(".0132", 0.0132);

    do_test("91.753", 91.753);

    do_test("-1.0", -1.0);
    do_test("-1.", -1.0);

    do_test("-1123", -1123.0);
    do_test("-0.0132", -0.0132);
    do_test("-.0132", -0.0132);

    do_test("-91.753", -91.753);
}
