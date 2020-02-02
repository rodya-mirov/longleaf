use super::*;

use super::BinaryOp::*;
use super::ExprNode::*;
use super::StatementNode::*;
use super::UnaryOp::*;

fn do_full_test(s: &str, expected: ReplInput) {
    let actual = parse_repl_input(s).expect("Should parse");

    assert_eq!(actual, expected);
}

fn test_parse_fail(s: &str) {
    let actual = parse_repl_input(s);
    assert!(actual.is_err());
}

fn parse_fail<T: std::fmt::Debug>(other: T) -> ! {
    // TODO: fail macro?
    eprintln!("When parsing, got {:?}, which was unexpected", other);
    assert!(false);
    unreachable!()
}

fn to_expr(s: &str) -> ExprNode {
    let parsed = parse_repl_input(s).expect("Should parse");

    match parsed {
        ReplInput::Expr(out) => out,
        other => parse_fail(other),
    }
}

fn expr_test(s: &str, expected: ExprNode) {
    let actual = to_expr(s);

    assert_eq!(actual, expected);
}

fn to_statement(s: &str) -> StatementNode {
    let parsed = parse_repl_input(s).expect("Should parse");

    match parsed {
        ReplInput::Statement(out) => out,
        other => parse_fail(other),
    }
}

fn statement_test(s: &str, expected: StatementNode) {
    let actual = to_statement(s);
    assert_eq!(actual, expected);
}

#[test]
fn no_empty_list() {
    test_parse_fail("[]");
    test_parse_fail("x = [];");
    test_parse_fail("sum([])");
}

#[test]
fn bool_tests() {
    do_full_test("true", ReplInput::Expr(ExprNode::Bool(true)));
    do_full_test("false", ReplInput::Expr(ExprNode::Bool(false)));
}

#[test]
fn bool_vec_tests() {
    do_full_test(
        "[true, false, true]",
        ReplInput::Expr(ExprNode::BoolList(vec![true, false, true])),
    );
    do_full_test(
        "[false, false, true, false]",
        ReplInput::Expr(ExprNode::BoolList(vec![false, false, true, false])),
    );
    do_full_test("[true]", ReplInput::Expr(ExprNode::BoolList(vec![true])));
    do_full_test("[false]", ReplInput::Expr(ExprNode::BoolList(vec![false])));
}

#[test]
fn comp_tests() {
    expr_test(
        "1 + 2 >= 3 + 4",
        ExprNode::BinaryExpr(
            BinaryOp::Geq,
            Box::new(ExprNode::BinaryExpr(
                BinaryOp::Plus,
                Box::new(ExprNode::Float(1.)),
                Box::new(ExprNode::Float(2.)),
            )),
            Box::new(ExprNode::BinaryExpr(
                BinaryOp::Plus,
                Box::new(ExprNode::Float(3.)),
                Box::new(ExprNode::Float(4.)),
            )),
        ),
    );
    expr_test(
        "1 + 2 > 3 + 4",
        ExprNode::BinaryExpr(
            BinaryOp::Gt,
            Box::new(ExprNode::BinaryExpr(
                BinaryOp::Plus,
                Box::new(ExprNode::Float(1.)),
                Box::new(ExprNode::Float(2.)),
            )),
            Box::new(ExprNode::BinaryExpr(
                BinaryOp::Plus,
                Box::new(ExprNode::Float(3.)),
                Box::new(ExprNode::Float(4.)),
            )),
        ),
    );
    expr_test(
        "1 + 2 < 3 + 4",
        ExprNode::BinaryExpr(
            BinaryOp::Lt,
            Box::new(ExprNode::BinaryExpr(
                BinaryOp::Plus,
                Box::new(ExprNode::Float(1.)),
                Box::new(ExprNode::Float(2.)),
            )),
            Box::new(ExprNode::BinaryExpr(
                BinaryOp::Plus,
                Box::new(ExprNode::Float(3.)),
                Box::new(ExprNode::Float(4.)),
            )),
        ),
    );

    expr_test(
        "[1, 2, 3] != 2 + [5, 4]",
        BinaryExpr(
            NotEquals,
            Box::new(FloatList(vec![1., 2., 3.])),
            Box::new(BinaryExpr(
                Plus,
                Box::new(Float(2.)),
                Box::new(FloatList(vec![5., 4.])),
            )),
        ),
    );

    statement_test(
        "isSame = 12 == 13;",
        StatementNode::VarDefn(
            "isSame".to_string(),
            BinaryExpr(Equals, Box::new(Float(12.)), Box::new(Float(13.))),
        ),
    );
}

#[test]
fn to_expr_tests() {
    expr_test(
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

    expr_test(
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

    expr_test(
        "f(1, 2, 3+4)",
        FunctionCall(
            "f".to_string(),
            vec![
                Float(1.),
                Float(2.),
                BinaryExpr(Plus, Box::new(Float(3.)), Box::new(Float(4.))),
            ],
        ),
    );

    expr_test(
        "sin(x)",
        FunctionCall("sin".to_string(), vec![VariableRef("x".to_string())]),
    );

    expr_test(
        "-cos(12+f(x)-1)",
        UnaryExpr(
            Negate,
            Box::new(FunctionCall(
                "cos".to_string(),
                vec![BinaryExpr(
                    Plus,
                    Box::new(Float(12.)),
                    Box::new(BinaryExpr(
                        Minus,
                        Box::new(FunctionCall(
                            "f".to_string(),
                            vec![VariableRef("x".to_string())],
                        )),
                        Box::new(Float(1.)),
                    )),
                )],
            )),
        ),
    );

    expr_test(
        "12 + \\x => x + 12",
        BinaryExpr(
            Plus,
            Box::new(Float(12.)),
            Box::new(FunctionDefn(
                args(&["x"]),
                vec![ReturnStmt(BinaryExpr(
                    Plus,
                    var_ref("x"),
                    Box::new(Float(12.)),
                ))],
            )),
        ),
    );

    expr_test(
        "12 + \\x => \\y => x + y + 12",
        BinaryExpr(
            Plus,
            Box::new(Float(12.)),
            Box::new(FunctionDefn(
                args(&["x"]),
                vec![ReturnStmt(FunctionDefn(
                    args(&["y"]),
                    vec![ReturnStmt(BinaryExpr(
                        Plus,
                        var_ref("x"),
                        Box::new(BinaryExpr(Plus, var_ref("y"), Box::new(Float(12.)))),
                    ))],
                ))],
            )),
        ),
    );

    expr_test(
        "12 + \\x => { f = \\y => x + y + 12; return f(x); }",
        BinaryExpr(
            Plus,
            Box::new(Float(12.)),
            Box::new(FunctionDefn(
                args(&["x"]),
                vec![
                    VarDefn(
                        "f".to_string(),
                        FunctionDefn(
                            args(&["y"]),
                            vec![ReturnStmt(BinaryExpr(
                                Plus,
                                var_ref("x"),
                                Box::new(BinaryExpr(Plus, var_ref("y"), Box::new(Float(12.)))),
                            ))],
                        ),
                    ),
                    ReturnStmt(FunctionCall("f".to_string(), vec![*var_ref("x")])),
                ],
            )),
        ),
    );
}

fn var_ref(name: &str) -> Box<ExprNode> {
    Box::new(VariableRef(name.to_string()))
}

fn args(args: &[&str]) -> Args {
    Args(args.iter().map(|s| s.to_string()).collect())
}

fn to_float_list(s: &str) -> Vec<f64> {
    let parsed = parse_repl_input(s).expect("Should parse");

    match parsed {
        ReplInput::Expr(ExprNode::FloatList(out)) => out,
        other => parse_fail(other),
    }
}

#[test]
fn float_vec_tests() {
    fn do_test(input: &str, expected: &[f64]) {
        let actual = to_float_list(input);

        let ar: &[f64] = &actual;

        assert_eq!(ar, expected);
    }

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

fn to_float(s: &str) -> f64 {
    let parsed = parse_repl_input(s).expect("Should parse");

    match parsed {
        ReplInput::Expr(ExprNode::Float(f)) => f,
        other => parse_fail(other),
    }
}

#[test]
fn float_tests() {
    fn do_test(input: &str, expected: f64) {
        let actual = to_float(input);

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
