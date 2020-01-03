use pest::{
    iterators::{Pair, Pairs},
    Parser,
};

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
struct PestParser;

pub type ParseResult<T> = Result<T, String>;

#[derive(PartialEq, Debug, Clone)]
pub enum ReplInput {
    Expr(ExprNode),
    VarDefn(String, ExprNode),
    Exit,
    Help,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExprNode {
    VariableRef(String),
    Float(f64),
    FloatList(Vec<f64>),
    UnaryExpr(UnaryOp, Box<ExprNode>),
    BinaryExpr(BinaryOp, Box<ExprNode>, Box<ExprNode>),
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
}

pub fn parse_repl_input(input: &str) -> ParseResult<ReplInput> {
    let parsed = PestParser::parse(Rule::repl_input_line, input);

    if parsed.is_err() {
        // TODO: better error handling
        return Err(parsed.unwrap_err().to_string());
    }

    let mut parsed: Pairs<'_, Rule> = parsed.unwrap().next().unwrap().into_inner();

    let actual_input = parsed.next().unwrap();

    assert_eq!(parsed.next().unwrap().as_rule(), Rule::EOI);
    assert!(parsed.next().is_none());

    assert_eq!(actual_input.as_rule(), Rule::repl_input);
    let actual_input = only_child(actual_input);

    match actual_input.as_rule() {
        Rule::expr => Ok(ReplInput::Expr(compile_expr_node(actual_input))),
        Rule::statement => {
            let statement_node = only_child(actual_input);
            match statement_node.as_rule() {
                Rule::var_defn_stmt => {
                    let mut pairs = statement_node.into_inner();
                    let id = pairs.next().unwrap().as_str().trim().to_string();
                    let expr = compile_expr_node(pairs.next().unwrap());
                    assert!(pairs.next().is_none());

                    Ok(ReplInput::VarDefn(id, expr))
                }
                other => panic!(
                    "Unexpected rule match {:?} when parsing REPL input line",
                    other
                ),
            }
        }
        Rule::quit => Ok(ReplInput::Exit),
        Rule::help => Ok(ReplInput::Help),
        other => panic!(
            "Unexpected rule match {:?} when parsing REPL input line",
            other
        ),
    }
}

fn only_child<'a>(pair: Pair<'a, Rule>) -> Pair<'a, Rule> {
    let mut pairs = pair.into_inner();

    let next = pairs.next();

    if next.is_none() {
        panic!("Received pair which has no child term");
    }

    if pairs.next().is_some() {
        panic!("Received pair which has multiple child terms");
    }

    next.unwrap()
}

fn compile_expr_node(parsed: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(parsed.as_rule(), Rule::expr);

    compile_add_expr_node(only_child(parsed))
}

fn compile_add_expr_node(parsed: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(parsed.as_rule(), Rule::add_expr);

    let mut pairs = parsed.into_inner();

    let mut mul_exprs: Vec<ExprNode> = Vec::new();
    let mut add_ops: Vec<BinaryOp> = Vec::new();

    mul_exprs.push(compile_mul_expr_node(pairs.next().unwrap()));

    while let Some(next_op) = pairs.next() {
        let op = compile_add_op(next_op);
        add_ops.push(op);

        mul_exprs.push(compile_mul_expr_node(pairs.next().unwrap()));
    }

    let mut out = mul_exprs.pop().unwrap();

    while let Some(op) = add_ops.pop() {
        let a = mul_exprs.pop().unwrap();
        out = ExprNode::BinaryExpr(op, Box::new(a), Box::new(out));
    }

    out
}

fn compile_mul_expr_node(parsed: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(parsed.as_rule(), Rule::mul_expr);

    let mut pairs = parsed.into_inner();

    let mut unary_exprs: Vec<ExprNode> = Vec::new();
    let mut mul_ops: Vec<BinaryOp> = Vec::new();

    unary_exprs.push(compile_unary_expr_node(pairs.next().unwrap()));

    while let Some(next_op) = pairs.next() {
        let op = compile_mul_op(next_op);
        mul_ops.push(op);

        unary_exprs.push(compile_unary_expr_node(pairs.next().unwrap()));
    }

    let mut out = unary_exprs.pop().unwrap();

    while let Some(op) = mul_ops.pop() {
        let a = unary_exprs.pop().unwrap();
        out = ExprNode::BinaryExpr(op, Box::new(a), Box::new(out));
    }

    out
}

fn compile_unary_expr_node(parsed: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(parsed.as_rule(), Rule::unary_expr);

    let mut pairs = parsed.into_inner();

    let first = pairs.next().unwrap();

    match first.as_rule() {
        Rule::base_expr =>  {
            assert!(pairs.next().is_none());
            compile_base_expr_node(first)
        }
        Rule::unary_op => {
            let op = compile_unary_op(first);
            let next = compile_unary_expr_node(pairs.next().unwrap());

            assert!(pairs.next().is_none());

            ExprNode::UnaryExpr(op, Box::new(next))
        }
        other => panic!("Unexpected rule {:?} when parsing unary expression", other),
    }
}

fn compile_base_expr_node(pair: Pair<'_, Rule>) -> ExprNode {
    let pair = only_child(pair);

    match pair.as_rule() {
        Rule::float => ExprNode::Float(compile_float(pair)),
        Rule::float_list => ExprNode::FloatList(compile_float_list(pair)),
        Rule::paren_expr => compile_expr_node(only_child(pair)),
        Rule::id => ExprNode::VariableRef(compile_id(pair)),
        other => panic!("Unexpected rule {:?} as child of base_expr", other),
    }
}

fn compile_float(pair: Pair<'_, Rule>) -> f64 {
    assert_eq!(pair.as_rule(), Rule::float);

    pair.as_str().trim().parse().unwrap()
}

fn compile_float_list(pair: Pair<'_, Rule>) -> Vec<f64> {
    assert_eq!(pair.as_rule(), Rule::float_list);

    let mut out = Vec::new();

    for child in pair.into_inner() {
        out.push(compile_float(child));
    }

    out
}

fn compile_id(pair: Pair<'_, Rule>) -> String {
    assert_eq!(pair.as_rule(), Rule::id);

    pair.as_str().trim().to_string()
}

fn compile_add_op(pair: Pair<'_, Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::add_op);

    let inner = only_child(pair);

    match inner.as_rule() {
        Rule::PLUS => BinaryOp::Plus,
        Rule::MINUS => BinaryOp::Minus,
        _ => panic!("Not a recognized additive operation: {:?}", inner),
    }
}

fn compile_mul_op(pair: Pair<'_, Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::mul_op);

    let inner = only_child(pair);

    match inner.as_rule() {
        Rule::TIMES => BinaryOp::Times,
        Rule::DIVIDE => BinaryOp::Divide,
        _ => panic!("Not a recognized multiplicative operation: {:?}", inner),
    }
}

fn compile_unary_op(pair: Pair<'_, Rule>) -> UnaryOp {
    assert_eq!(pair.as_rule(), Rule::unary_op);

    let inner = only_child(pair);

    match inner.as_rule() {
        Rule::NEG => UnaryOp::Negate,
        _ => panic!("Not a recognized unary operation: {:?}", inner),
    }
}

#[cfg(test)]
mod tests;
