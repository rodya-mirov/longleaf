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
    Statement(StatementNode),
    Exit,
    Help,
    MemoryUsage,
    GarbageCollect,
}

#[derive(PartialEq, Debug, Clone)]
pub enum StatementNode {
    VarDefn(String, ExprNode),
    ReturnStmt(ExprNode),
    BlockStmt(BlockStmt),
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<StatementNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Args(pub Vec<String>);

#[derive(PartialEq, Debug, Clone)]
pub enum ExprNode {
    // Note this isn't Args and BlockStmt, because Block handles its own namespacing and flow control,
    // while function evaluation needs to be more particular. So it's better to think that both functions
    // and blocks "contain a list of statements" instead of thinking "functions contain a block", even
    // though the grammar says functions contain a block. This is just for parsing convenience.
    FunctionDefn(Args, Vec<StatementNode>),
    VariableRef(String),
    Float(f64),
    FloatList(Vec<f64>),
    Bool(bool),          // TODO: unit test parser producing these
    BoolList(Vec<bool>), // TODO: Unit test parser producing these
    FunctionCall(String, Vec<ExprNode>),
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
    // TODO: abbreviations too enigmatic?
    Geq,
    Gt,
    Leq,
    Lt,
    Equals,
    NotEquals,
}

pub fn parse_repl_input(input: &str) -> ParseResult<ReplInput> {
    let parsed = PestParser::parse(Rule::repl_input_line, input);

    // TODO: better error handling?
    let mut parsed = parsed.map_err(|e| e.to_string())?;

    let mut parsed: Pairs<'_, Rule> = parsed.next().unwrap().into_inner();

    let actual_input = parsed.next().unwrap();

    assert_eq!(parsed.next().unwrap().as_rule(), Rule::EOI);
    assert!(parsed.next().is_none());

    assert_eq!(actual_input.as_rule(), Rule::repl_input);
    let actual_input = only_child(actual_input);

    match actual_input.as_rule() {
        Rule::expr => Ok(ReplInput::Expr(compile_expr_node(actual_input))),
        Rule::statement => Ok(ReplInput::Statement(compile_statement_node(actual_input))),
        Rule::quit => Ok(ReplInput::Exit),
        Rule::help => Ok(ReplInput::Help),
        Rule::mem => Ok(ReplInput::MemoryUsage),
        Rule::gc => Ok(ReplInput::GarbageCollect),
        other => panic!(
            "Unexpected rule match {:?} when parsing REPL input line",
            other
        ),
    }
}

fn only_child(pair: Pair<'_, Rule>) -> Pair<'_, Rule> {
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

fn compile_statement_node(parsed: Pair<'_, Rule>) -> StatementNode {
    assert_eq!(parsed.as_rule(), Rule::statement);

    let statement_node = only_child(parsed);

    match statement_node.as_rule() {
        Rule::var_defn_stmt => {
            let mut pairs = statement_node.into_inner();
            let id = pairs.next().unwrap().as_str().trim().to_string();
            let expr = compile_expr_node(pairs.next().unwrap());
            assert!(pairs.next().is_none());

            StatementNode::VarDefn(id, expr)
        }
        Rule::block_stmt => StatementNode::BlockStmt(compile_block_stmt(statement_node)),
        Rule::return_stmt => {
            let inner_expr = only_child(statement_node);
            let expr = compile_expr_node(inner_expr);

            StatementNode::ReturnStmt(expr)
        }
        other => panic!(
            "Unexpected rule match {:?} when parsing REPL input line",
            other
        ),
    }
}

fn compile_block_stmt(pair: Pair<'_, Rule>) -> BlockStmt {
    assert_eq!(pair.as_rule(), Rule::block_stmt);

    let mut out: Vec<StatementNode> = Vec::new();

    for child in pair.into_inner() {
        out.push(compile_statement_node(child));
    }

    BlockStmt { stmts: out }
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
        Rule::comp_expr => {
            assert!(pairs.next().is_none());
            compile_comp_expr_node(first)
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

fn compile_comp_expr_node(parsed: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(parsed.as_rule(), Rule::comp_expr);

    let mut pairs = parsed.into_inner();

    let first = pairs.next().unwrap();

    assert_eq!(first.as_rule(), Rule::base_expr);

    let left = compile_base_expr_node(first);

    if let Some(op) = pairs.next() {
        assert_eq!(op.as_rule(), Rule::comp_op);

        let right = pairs.next().unwrap();
        assert_eq!(right.as_rule(), Rule::base_expr);

        let right = compile_base_expr_node(right);

        let bin_op = compile_comp_op(op);

        ExprNode::BinaryExpr(bin_op, Box::new(left), Box::new(right))
    } else {
        left
    }
}

fn compile_comp_op(pair: Pair<'_, Rule>) -> BinaryOp {
    assert_eq!(pair.as_rule(), Rule::comp_op);

    let child = only_child(pair);

    match child.as_rule() {
        Rule::GEQ => BinaryOp::Geq,
        Rule::GT => BinaryOp::Gt,
        Rule::LEQ => BinaryOp::Leq,
        Rule::LT => BinaryOp::Lt,
        Rule::EQ => BinaryOp::Equals,
        Rule::NEQ => BinaryOp::NotEquals,
        other => panic!("Unexpected rule {:?} as comp_op", other),
    }
}

fn compile_base_expr_node(pair: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(pair.as_rule(), Rule::base_expr);

    let pair = only_child(pair);

    match pair.as_rule() {
        Rule::bool => ExprNode::Bool(compile_bool(pair)),
        Rule::float => ExprNode::Float(compile_float(pair)),
        Rule::bool_list => ExprNode::BoolList(compile_bool_list(pair)),
        Rule::float_list => ExprNode::FloatList(compile_float_list(pair)),
        Rule::paren_expr => compile_expr_node(only_child(pair)),
        Rule::id => ExprNode::VariableRef(compile_id(pair)),
        Rule::function_call => compile_function_call(pair),
        Rule::function_definition => compile_function_defn_expr(pair),
        other => panic!("Unexpected rule {:?} as child of base_expr", other),
    }
}

fn compile_function_defn_expr(pair: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(pair.as_rule(), Rule::function_definition);

    let mut args: Vec<String> = Vec::new();
    let mut body: Option<Vec<StatementNode>> = None;

    for child in pair.into_inner() {
        match child.as_rule() {
            Rule::escaped_id => {
                assert!(body.is_none());

                let id = compile_escaped_id(child);
                args.push(id);
            }
            Rule::function_body => {
                let body_node = only_child(child);
                match body_node.as_rule() {
                    Rule::expr => {
                        assert!(body.is_none());

                        let expr_body = compile_expr_node(body_node);
                        body = Some(vec![StatementNode::ReturnStmt(expr_body)]);
                    }
                    Rule::block_stmt => {
                        assert!(body.is_none());

                        let block_body: BlockStmt = compile_block_stmt(body_node);
                        body = Some(block_body.stmts);
                    }
                    other => {
                        panic!("Received rule {:?} when parsing function definition", other);
                    }
                }
            }
            other => {
                panic!("Received rule {:?} when parsing function definition", other);
            }
        }
    }

    assert!(body.is_some());

    let args = Args(args);
    ExprNode::FunctionDefn(args, body.unwrap())
}

fn compile_escaped_id(pair: Pair<'_, Rule>) -> String {
    assert_eq!(pair.as_rule(), Rule::escaped_id);

    pair.as_str().chars().skip(1).collect()
}

fn compile_function_call(pair: Pair<'_, Rule>) -> ExprNode {
    assert_eq!(pair.as_rule(), Rule::function_call);

    let mut pairs = pair.into_inner();

    let id = compile_id(pairs.next().unwrap());

    let mut args = Vec::new();

    for pair in pairs {
        args.push(compile_expr_node(pair));
    }

    ExprNode::FunctionCall(id, args)
}

fn compile_bool(pair: Pair<'_, Rule>) -> bool {
    assert_eq!(pair.as_rule(), Rule::bool);

    pair.as_str().trim().parse().unwrap()
}

fn compile_float(pair: Pair<'_, Rule>) -> f64 {
    assert_eq!(pair.as_rule(), Rule::float);

    pair.as_str().trim().parse().unwrap()
}

fn compile_bool_list(pair: Pair<'_, Rule>) -> Vec<bool> {
    assert_eq!(pair.as_rule(), Rule::bool_list);

    let mut out = Vec::new();

    for child in pair.into_inner() {
        out.push(compile_bool(child));
    }

    out.shrink_to_fit();

    out
}

fn compile_float_list(pair: Pair<'_, Rule>) -> Vec<f64> {
    assert_eq!(pair.as_rule(), Rule::float_list);

    let mut out = Vec::new();

    for child in pair.into_inner() {
        out.push(compile_float(child));
    }

    out.shrink_to_fit();

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
