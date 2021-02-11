#[derive(PartialEq, Debug)]
pub enum StmtNode {
    Assign(AssignStmt),
    Expr(ExprStmt),
    Print(PrintStmt),
}

#[derive(PartialEq, Debug)]
pub struct AssignStmt {
    pub lhs: IdRefNode,
    pub rhs: ExprNode,
}

#[derive(PartialEq, Debug)]
pub struct ExprStmt {
    pub expr: ExprNode,
}

#[derive(PartialEq, Debug)]
pub struct PrintStmt {
    pub expr: ExprNode,
}

#[derive(PartialEq, Debug)]
pub enum ExprNode {
    Unary(UnaryExprNode),
    Binary(BinaryExprNode),
    Block(BlockExprNode),
    If(IfExprNode),
    FnCall(FnCallNode),
    FnDef(FnDefNode),
    Nil(NilNode),
    BoolConst(BoolConstNode),
    Number(NumberNode),
    String(StringNode),
    Id(IdRefNode),
}

#[derive(PartialEq, Debug)]
pub struct IdRefNode {
    pub name: String,
}

#[derive(PartialEq, Debug)]
pub struct UnaryExprNode {
    pub op: UnaryOp,
    pub arg: Box<ExprNode>,
}

#[derive(PartialEq, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(PartialEq, Debug)]
pub struct BinaryExprNode {
    pub op: BinaryOp,
    pub left: Box<ExprNode>,
    pub right: Box<ExprNode>,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    And,
    Or,
    Gt,
    Geq,
    Eq,
    Neq,
    Lt,
    Leq,
}

#[derive(PartialEq, Debug)]
pub struct NumberNode {
    pub val: f64,
}

#[derive(PartialEq, Debug)]
pub struct StringNode {
    pub val: String,
}

#[derive(PartialEq, Debug)]
pub struct BoolConstNode {
    pub val: bool,
}

#[derive(PartialEq, Debug)]
pub struct NilNode {}

#[derive(PartialEq, Debug)]
pub struct IfExprNode {
    pub cond: Box<ExprNode>,
    pub on_true: Box<ExprNode>,
    pub on_false: Box<ExprNode>,
}

#[derive(PartialEq, Debug)]
pub struct BlockExprNode {
    pub statements: Vec<StmtNode>,
    pub ret: Box<ExprNode>,
}

#[derive(PartialEq, Debug)]
pub struct FnCallNode {
    pub function: Box<ExprNode>,
    pub args: Vec<ExprNode>,
}

#[derive(PartialEq, Debug)]
pub struct FnDefNode {
    pub arg_names: Vec<IdRefNode>,
    pub body: Box<ExprNode>,
}
