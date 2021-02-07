use crate::Span;

#[derive(PartialEq, Debug)]
pub enum StmtNode<'a> {
    Assign(AssignStmt<'a>),
    Expr(ExprStmt<'a>),
}

#[derive(PartialEq, Debug)]
pub struct AssignStmt<'a> {
    pub position: Span<'a>,
    pub lhs: IdRef<'a>,
    pub rhs: ExprNode<'a>,
}

#[derive(PartialEq, Debug)]
pub struct ExprStmt<'a> {
    pub position: Span<'a>,
    pub expr: ExprNode<'a>,
}

#[derive(PartialEq, Debug)]
pub enum ExprNode<'a> {
    Unary(UnaryExprNode<'a>),
    Binary(BinaryExprNode<'a>),
    Block(BlockExprNode<'a>),
    If(IfExprNode<'a>),
    FnCall(FnCallNode<'a>),
    FnDef(FnDefNode<'a>),
    Paren(ParenNode<'a>),
    Number(NumberNode<'a>),
    Id(IdNode<'a>),
}

#[derive(PartialEq, Debug)]
pub struct IdRef<'a> {
    pub position: Span<'a>,
    pub name: &'a str,
}

#[derive(PartialEq, Debug)]
pub struct UnaryExprNode<'a> {
    pub position: Span<'a>,
    pub op: UnaryOpNode<'a>,
    pub arg: Box<ExprNode<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct UnaryOpNode<'a> {
    pub position: Span<'a>,
    pub op: UnaryOp,
}

#[derive(PartialEq, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(PartialEq, Debug)]
pub struct BinaryExprNode<'a> {
    pub position: Span<'a>,
    pub op: BinaryOpNode<'a>,
    pub left: Box<ExprNode<'a>>,
    pub right: Box<ExprNode<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct BinaryOpNode<'a> {
    pub position: Span<'a>,
    pub op: BinaryOp,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    And,
    Or,
}

#[derive(PartialEq, Debug)]
pub struct ParenNode<'a> {
    pub position: Span<'a>,
    pub child: Box<ExprNode<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct NumberNode<'a> {
    pub position: Span<'a>,
    pub num_text: &'a str,
    pub val: f64,
}

#[derive(PartialEq, Debug)]
pub struct IdNode<'a> {
    pub position: Span<'a>,
    pub id_text: &'a str,
}

#[derive(PartialEq, Debug)]
pub struct IfExprNode<'a> {
    pub position: Span<'a>,
    pub cond: Box<ExprNode<'a>>,
    pub on_true: Box<ExprNode<'a>>,
    pub on_false: Box<ExprNode<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct BlockExprNode<'a> {
    pub position: Span<'a>,
    pub statements: Vec<StmtNode<'a>>,
    pub ret: Box<ExprNode<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct FnCallNode<'a> {
    pub position: Span<'a>,
    pub function: Box<ExprNode<'a>>,
    pub args: Vec<ExprNode<'a>>,
}

#[derive(PartialEq, Debug)]
pub struct FnDefNode<'a> {
    pub position: Span<'a>,
    pub arg_names: Vec<IdRef<'a>>,
    pub body: Box<ExprNode<'a>>,
}
