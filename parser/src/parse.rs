use std::collections::VecDeque;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1},
    combinator::recognize,
    multi::many0,
    number::complete::recognize_float,
    sequence::pair,
    IResult, Parser,
};

use nom_locate::position;

use crate::cst::UnaryOpNode;
use crate::{cst, Span};
use nom::sequence::tuple;

#[cfg(test)]
mod tests;

#[allow(unused)]
pub(crate) fn parse_statement(s: Span) -> IResult<Span, cst::StmtNode> {
    alt((
        parse_assign_stmt.map(|v| cst::StmtNode::Assign(v)),
        parse_expr_stmt.map(|v| cst::StmtNode::Expr(v)),
    ))(s)
}

// Parser rules to apply throughout -- to the greatest extent possible,
// parsers should drop (optional!) leading whitespace -- let (s, _) = multispace0(s)?)
// and not do anything about trailing whitespace. This is because a calling parser may
// need to verify the existence of whitespace (multispace1(s)?) to make sure tokens are
// separated.

fn parse_expr_stmt(s: Span) -> IResult<Span, cst::ExprStmt> {
    let (s, pos) = position(s)?;
    let (s, _) = multispace0(s)?;
    let (s, expr) = parse_expr(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = parse_semi(s)?;

    Ok((
        s,
        cst::ExprStmt {
            position: pos,
            expr,
        },
    ))
}

fn parse_assign_stmt(s: Span) -> IResult<Span, cst::AssignStmt> {
    // this "skip optional whitespace" thing is super tedious
    let (s, pos) = position(s)?;

    let (s, _) = multispace0(s)?;
    let (s, _) = parse_let(s)?;
    let (s, _) = multispace1(s)?;
    let (s, id) = parse_id(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = parse_assign(s)?;
    let (s, _) = multispace0(s)?;
    let (s, expr) = parse_expr(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = parse_semi(s)?;

    Ok((
        s,
        cst::AssignStmt {
            position: pos,
            lhs: id,
            rhs: expr,
        },
    ))
}

fn parse_let(s: Span) -> IResult<Span, Span> {
    tag("let")(s)
}

fn parse_id(s: Span) -> IResult<Span, cst::IdRef> {
    let (s, pos) = position(s)?;

    let (s, slice) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(s)?;

    Ok((
        s,
        cst::IdRef {
            position: pos,
            name: slice.fragment(),
        },
    ))
}

fn parse_assign(s: Span) -> IResult<Span, Span> {
    tag("=")(s)
}

fn parse_semi(s: Span) -> IResult<Span, Span> {
    tag(";")(s)
}

// Top-level expressions. Anything which takes "any arbitrary expression" should call this.
// Just a pass-through for whatever top-level expression there is.
fn parse_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    parse_bool_expr(s)
}

// Helper function for the quite common case of parsing a chain of same-precedence binary
// infix expressions, with left-to-right associativity. That is, a+b+c should be (a+b)+c.
fn parse_binary_expr_left_to_right<'a, OpParser, ChildParser>(
    s: Span<'a>,
    mut child_parser: ChildParser,
    mut op_parser: OpParser,
    space_separated: bool,
) -> IResult<Span<'a>, cst::ExprNode<'a>>
where
    OpParser: FnMut(Span<'a>) -> IResult<Span<'a>, cst::BinaryOpNode>,
    ChildParser: FnMut(Span<'a>) -> IResult<Span<'a>, cst::ExprNode<'a>>,
{
    let (s, pos) = position(s)?;
    let (mut s, first) = child_parser(s)?;

    let mut ops = VecDeque::new();
    let mut exprs = VecDeque::new();
    exprs.push_back(first);

    let mut spaced_op_parser: Box<dyn FnMut(Span<'a>) -> IResult<Span<'a>, cst::BinaryOpNode>> =
        if space_separated {
            Box::new(|span| {
                let (span, (_, op, _)) = tuple((multispace1, &mut op_parser, multispace1))(span)?;
                Ok((span, op))
            })
        } else {
            Box::new(|span| op_parser(span))
        };

    while let Ok((next_s, op)) = spaced_op_parser(s) {
        ops.push_back(op);

        let (next_s, expr) = child_parser(next_s)?;

        exprs.push_back(expr);

        s = next_s;
    }

    // code should guarantee this, so it's fine to panic on failure
    assert_eq!(ops.len() + 1, exprs.len());

    // note left-to-right associativity
    let mut out: cst::ExprNode = exprs.pop_front().unwrap();

    while let Some(next_expr) = exprs.pop_front() {
        let next_op = ops.pop_front().unwrap();
        out = cst::ExprNode::Binary(cst::BinaryExprNode {
            position: pos,
            op: next_op,
            left: Box::new(out),
            right: Box::new(next_expr),
        });
    }

    Ok((s, out))
}

// a OR b AND c, etc.
fn parse_bool_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    parse_binary_expr_left_to_right(s, &mut parse_add_expr, &mut parse_bool_op, true)
}

fn parse_bool_op(s: Span) -> IResult<Span, cst::BinaryOpNode> {
    let (s, pos) = position(s)?;
    let (s, op) = alt((
        tag("and").map(|_| cst::BinaryOp::And),
        tag("or").map(|_| cst::BinaryOp::Or),
    ))(s)?;

    Ok((s, cst::BinaryOpNode { position: pos, op }))
}

fn parse_add_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    parse_binary_expr_left_to_right(s, &mut parse_mul_expr, &mut parse_add_op, false)
}

fn parse_add_op(s: Span) -> IResult<Span, cst::BinaryOpNode> {
    let (s, pos) = position(s)?;
    let (s, op) = alt((
        tag("+").map(|_| cst::BinaryOp::Plus),
        tag("-").map(|_| cst::BinaryOp::Minus),
    ))(s)?;

    Ok((s, cst::BinaryOpNode { position: pos, op }))
}

fn parse_mul_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    parse_binary_expr_left_to_right(s, &mut parse_unary_expr, &mut parse_mul_op, false)
}

fn parse_mul_op(s: Span) -> IResult<Span, cst::BinaryOpNode> {
    let (s, pos) = position(s)?;
    let (s, op) = alt((
        tag("*").map(|_| cst::BinaryOp::Times),
        tag("/").map(|_| cst::BinaryOp::Divide),
    ))(s)?;

    Ok((s, cst::BinaryOpNode { position: pos, op }))
}

fn parse_unary_expr<'a>(mut s: Span<'a>) -> IResult<Span<'a>, cst::ExprNode> {
    let mut ops = Vec::new();

    let tagged_op = |span: Span<'a>| -> IResult<Span<'a>, UnaryOpNode> {
        let (span, (_, op)) = pair(multispace0, parse_unary_op)(span)?;
        Ok((span, op))
    };

    while let Ok((next_s, op)) = tagged_op(s) {
        ops.push(op);
        s = next_s;
    }

    let (s, mut expr) = parse_base_expr(s)?;

    while let Some(op) = ops.pop() {
        expr = cst::ExprNode::Unary(cst::UnaryExprNode {
            position: op.position.clone(),
            op,
            arg: Box::new(expr),
        });
    }

    Ok((s, expr))
}

fn parse_unary_op(s: Span) -> IResult<Span, cst::UnaryOpNode> {
    let (s, pos) = position(s)?;
    let (s, op) = alt((
        tag("-").map(|_| cst::UnaryOp::Neg),
        tag("!").map(|_| cst::UnaryOp::Not),
    ))(s)?;

    Ok((s, cst::UnaryOpNode { position: pos, op }))
}

fn parse_base_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    alt((
        parse_paren_expr,
        parse_block_expr,
        parse_fn_call_expr,
        parse_fn_defn_expr,
        parse_if_expr,
        parse_number_expr,
        parse_id_expr,
    ))(s)
}

fn parse_paren_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    let (s, _) = multispace0(s)?;

    let (s, pos) = position(s)?;

    let (s, _) = tag("(")(s)?;
    let (s, _) = multispace0(s)?;
    let (s, expr) = parse_expr(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = tag(")")(s)?;

    Ok((
        s,
        cst::ExprNode::Paren(cst::ParenNode {
            position: pos,
            child: Box::new(expr),
        }),
    ))
}

fn parse_block_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    let (s, _) = multispace0(s)?;

    let (s, pos) = position(s)?;

    let (mut s, _) = tag("{")(s)?;

    let mut stmts = Vec::new();

    // Note this drops parser errors on the assumption that the final error is an expression
    while let Ok((next_s, stmt)) = parse_statement(s) {
        stmts.push(stmt);
        s = next_s;
    }

    let (s, expr) = parse_expr(s)?;

    let (s, _) = multispace0(s)?;
    let (s, _) = tag("}")(s)?;

    Ok((
        s,
        cst::ExprNode::Block(cst::BlockExprNode {
            position: pos,
            statements: stmts,
            ret: Box::new(expr),
        }),
    ))
}

fn parse_comma_list<
    'a,
    EltType,
    CloseEltType,
    EltParser: Parser<Span<'a>, EltType, nom::error::Error<Span<'a>>>,
    CloseParser: Parser<Span<'a>, CloseEltType, nom::error::Error<Span<'a>>>,
>(
    mut s: Span<'a>,
    mut elt_parser: EltParser,
    mut close_parser: CloseParser,
) -> IResult<Span<'a>, Vec<EltType>> {
    let mut elts = Vec::new();

    loop {
        let (next_s, arg) = elt_parser.parse(s)?;

        elts.push(arg);

        let (next_s, _) = multispace0(next_s)?;

        // we might immediately see a close tag
        if let Ok((ns, _)) = close_parser.parse(s) {
            return Ok((ns, elts));
        }

        // otherwise we definitely need a comma
        let (next_s, _) = tag(",")(next_s)?;
        let (next_s, _) = multispace0(next_s)?;

        // again, we might immediately see a close tag
        if let Ok((ns, _)) = close_parser.parse(s) {
            return Ok((ns, elts));
        }

        // If we didn't see a close tag then we need to immediately see another expression
        // so we just go to the top of the loop
        s = next_s;
    }
}

fn parse_fn_defn_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    let (s, _) = multispace0(s)?;

    let (s, pos) = position(s)?;

    let (s, _) = tag("(")(s)?;
    let (s, _) = multispace0(s)?;

    let (s, args) = parse_comma_list(s, parse_id, tag(")"))?;

    let (s, _) = multispace0(s)?;
    let (s, _) = tag("->")(s)?;
    let (s, _) = multispace0(s)?;

    let (s, body) = parse_block_expr(s)?;

    Ok((
        s,
        cst::ExprNode::FnDef(cst::FnDefNode {
            position: pos,
            arg_names: args,
            body: Box::new(body),
        }),
    ))
}

fn parse_fn_call_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    let (s, _) = multispace0(s)?;

    let (s, pos) = position(s)?;

    let (s, func_ref) = alt((parse_id_expr, parse_paren_expr))(s)?;

    let (s, _) = multispace0(s)?;
    let (s, _) = tag("(")(s)?;

    let (s, args) = parse_comma_list(s, parse_expr, tag(")"))?;

    return Ok((
        s,
        cst::ExprNode::FnCall(cst::FnCallNode {
            position: pos,
            function: Box::new(func_ref),
            args,
        }),
    ));
}

fn parse_if_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    let (s, _) = multispace0(s)?;

    let (s, pos) = position(s)?;

    let (s, _) = tag("if")(s)?;
    let (s, cond) = parse_paren_expr(s)?;

    let (s, _) = multispace0(s)?;
    let (s, on_true) = parse_block_expr(s)?;

    let (s, _) = multispace0(s)?;
    let (s, _) = tag("else")(s)?;
    let (s, _) = multispace0(s)?;

    let (s, on_false) = parse_block_expr(s)?;

    Ok((
        s,
        cst::ExprNode::If(cst::IfExprNode {
            position: pos,
            cond: Box::new(cond),
            on_true: Box::new(on_true),
            on_false: Box::new(on_false),
        }),
    ))
}

fn parse_number_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    use std::str::FromStr;

    let (s, _) = multispace0(s)?;

    let (s, pos) = position(s)?;
    let (s, num_text) = recognize_float(s)?;

    let num_text = num_text.fragment();
    let val = f64::from_str(num_text).unwrap();

    Ok((
        s,
        cst::ExprNode::Number(cst::NumberNode {
            position: pos,
            num_text,
            val,
        }),
    ))
}

fn parse_id_expr(s: Span) -> IResult<Span, cst::ExprNode> {
    let (s, _) = multispace0(s)?;

    let (s, pos) = position(s)?;
    let (s, id) = parse_id(s)?;

    Ok((
        s,
        cst::ExprNode::Id(cst::IdNode {
            position: pos,
            id_text: id.name,
        }),
    ))
}
