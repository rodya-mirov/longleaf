#[derive(PartialEq, Debug)]
pub enum StmtNode {
    Assign(AssignStmt),
    Expr(ExprStmt),
}

#[derive(PartialEq, Debug)]
pub struct AssignStmt {
    pub lhs: IdRef,
    pub rhs: ExprNode,
}

#[derive(PartialEq, Debug)]
pub struct ExprStmt {
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
    Number(NumberNode),
    Id(IdNode),
}

#[derive(PartialEq, Debug)]
pub struct IdRef {
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
}

#[derive(PartialEq, Debug)]
pub struct NumberNode {
    pub val: f64,
}

#[derive(PartialEq, Debug)]
pub struct IdNode {
    pub id_text: String,
}

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
    pub arg_names: Vec<IdRef>,
    pub body: Box<ExprNode>,
}
