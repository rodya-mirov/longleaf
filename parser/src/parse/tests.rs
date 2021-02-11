use lang::ast;

use super::*;

// Helper so you can use ? which helps the parsers work correctly
// Just make your test be `res_test(|| { your actual code with ? and stuff; ok() })`
fn res_test<'a, T: FnOnce() -> IResult<Span<'a>, ()>>(f: T) {
    let res = f();
    match res {
        Ok(_) => {}
        Err(e) => {
            println!("Error parsing node: {:?}", e);
            assert!(false);
            unreachable!();
        }
    }
}

fn ok<'a>() -> IResult<Span<'a>, ()> {
    Ok((Span::new(""), ()))
}

/// Make a default span for the purpose of a test fixture.
/// Note that if the arguments are bad, USING the span may result in UB. This is obviously
/// bad, so this is only used for unit tests, and only for making "expected" fixtures.
/// Don't use this span for anything!
fn def_span<'a>(offset: usize, line: u32) -> Span<'a> {
    unsafe { Span::new_from_raw_offset(offset, line, "", ()) }
}

#[inline]
fn ast_num(num: f64) -> Box<ast::ExprNode> {
    Box::new(ast::ExprNode::Number(ast::NumberNode { val: num }))
}

#[inline]
fn neg(node: Box<ast::ExprNode>) -> Box<ast::ExprNode> {
    Box::new(ast::ExprNode::Unary(ast::UnaryExprNode {
        op: ast::UnaryOp::Neg,
        arg: node,
    }))
}

#[test]
fn parse_expr_tests_num_0() {
    res_test(|| {
        let s = Span::new("1");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Number(ast::NumberNode { val: 1. });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_num_1() {
    res_test(|| {
        let s = Span::new("1.34");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Number(ast::NumberNode { val: 1.34 });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_num_2() {
    res_test(|| {
        let s = Span::new("1e7");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Number(ast::NumberNode { val: 1e7 });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_neg() {
    res_test(|| {
        let s = Span::new("---3");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Unary(ast::UnaryExprNode {
            op: ast::UnaryOp::Neg,
            arg: Box::new(ast::ExprNode::Unary(ast::UnaryExprNode {
                op: ast::UnaryOp::Neg,
                arg: Box::new(ast::ExprNode::Unary(ast::UnaryExprNode {
                    op: ast::UnaryOp::Neg,
                    arg: ast_num(3.),
                })),
            })),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_bin_1() {
    res_test(|| {
        let s = Span::new("1+2");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Plus,
            left: ast_num(1.),
            right: ast_num(2.),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_bin_2() {
    res_test(|| {
        let s = Span::new("1-2");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Minus,
            left: ast_num(1.),
            right: ast_num(2.),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_bin_3() {
    res_test(|| {
        let s = Span::new("1*2");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Times,
            left: ast_num(1.),
            right: ast_num(2.),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_bin_4() {
    res_test(|| {
        let s = Span::new("1/2");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Divide,
            left: ast_num(1.),
            right: ast_num(2.),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_bin_5() {
    res_test(|| {
        let s = Span::new("1 / 2");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Divide,
            left: ast_num(1.),
            right: ast_num(2.),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_bin_6() {
    res_test(|| {
        let s = Span::new("1/ 2");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Divide,
            left: ast_num(1.),
            right: ast_num(2.),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_bin_7() {
    res_test(|| {
        let s = Span::new("1 /2");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Divide,
            left: ast_num(1.),
            right: ast_num(2.),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_paren() {
    res_test(|| {
        let s = Span::new("1/(2+-3)");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Divide,
            left: ast_num(1.),
            right: Box::new(ast::ExprNode::Binary(ast::BinaryExprNode {
                op: ast::BinaryOp::Plus,
                left: ast_num(2.),
                right: neg(ast_num(3.)),
            })),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_expr_tests_paren_2() {
    res_test(|| {
        let s = Span::new("12/(1.2 + -3)");

        let (_s, actual) = parse_expr(s)?;

        let expected = ast::ExprNode::Binary(ast::BinaryExprNode {
            op: ast::BinaryOp::Divide,
            left: ast_num(12.),
            right: Box::new(ast::ExprNode::Binary(ast::BinaryExprNode {
                op: ast::BinaryOp::Plus,
                left: ast_num(1.2),
                right: neg(ast_num(3.)),
            })),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_str_tests() {
    res_test(|| {
        let s = Span::new("\"whatever\"");
        let (_s, actual) = parse_expr(s)?;
        let expected = ast::ExprNode::String(ast::StringNode {
            val: "whatever".to_string(),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_str_tests_2() {
    res_test(|| {
        let s = Span::new("\"whatever\nnewline\"");
        let (_s, actual) = parse_expr(s)?;
        let expected = ast::ExprNode::String(ast::StringNode {
            val: "whatever\nnewline".to_string(),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_str_tests_escaped_quote() {
    res_test(|| {
        let s = Span::new("\"whatever\\\"stuff\n\\newline\"");
        let (_s, actual) = parse_expr(s)?;
        let expected = ast::ExprNode::String(ast::StringNode {
            val: "whatever\"\\\nnewline".to_string(),
        });
        let actual: ast::ExprNode = actual.into();

        assert_eq!(actual, expected);
        ok()
    })
}

#[test]
fn parse_id_tests() {
    res_test(|| {
        let s = Span::new("fhdjfhjdks fdjksfn \n dsfjnkds 1");

        let (s, actual_1) = parse_id(s)?;
        let (s, _): (_, Span) = multispace1(s)?;
        let (s, actual_2) = parse_id(s)?;
        let (s, _): (_, Span) = multispace1(s)?;
        let (s, actual_3) = parse_id(s)?;

        assert_eq!(s.fragment(), &" 1");

        assert_eq!(
            actual_1,
            cst::IdRefNode {
                position: def_span(0, 1),
                name: "fhdjfhjdks"
            }
        );
        assert_eq!(actual_1.position.get_column(), 1);

        assert_eq!(
            actual_2,
            cst::IdRefNode {
                position: def_span(11, 1),
                name: "fdjksfn"
            }
        );
        assert_eq!(actual_2.position.get_column(), 12);

        assert_eq!(
            actual_3,
            cst::IdRefNode {
                position: def_span(21, 2),
                name: "dsfjnkds"
            }
        );
        assert_eq!(actual_3.position.get_column(), 2);

        ok()
    });
}
