use lang::ast;

use crate::cst;

impl<'a> From<cst::StmtNode<'a>> for ast::StmtNode {
    fn from(stmt: cst::StmtNode) -> Self {
        match stmt {
            cst::StmtNode::Assign(assign) => ast::StmtNode::Assign(ast::AssignStmt::from(assign)),
            cst::StmtNode::Expr(expr) => ast::StmtNode::Expr(ast::ExprStmt::from(expr)),
        }
    }
}

impl<'a> From<cst::AssignStmt<'a>> for ast::AssignStmt {
    fn from(assign: cst::AssignStmt) -> Self {
        ast::AssignStmt {
            lhs: ast::IdRef::from(assign.lhs),
            rhs: ast::ExprNode::from(assign.rhs),
        }
    }
}

impl<'a> From<cst::ExprStmt<'a>> for ast::ExprStmt {
    fn from(expr_stmt: cst::ExprStmt) -> Self {
        ast::ExprStmt {
            expr: ast::ExprNode::from(expr_stmt.expr),
        }
    }
}

impl<'a> From<cst::IdRef<'a>> for ast::IdRef {
    fn from(id_ref: cst::IdRef<'a>) -> Self {
        ast::IdRef {
            name: id_ref.name.to_string(),
        }
    }
}

impl<'a> From<cst::ExprNode<'a>> for ast::ExprNode {
    fn from(expr_node: cst::ExprNode<'a>) -> Self {
        match expr_node {
            cst::ExprNode::Unary(u) => ast::ExprNode::Unary(ast::UnaryExprNode::from(u)),
            cst::ExprNode::Binary(b) => ast::ExprNode::Binary(ast::BinaryExprNode::from(b)),
            cst::ExprNode::Block(b) => ast::ExprNode::Block(ast::BlockExprNode::from(b)),
            cst::ExprNode::If(i) => ast::ExprNode::If(ast::IfExprNode::from(i)),
            cst::ExprNode::FnCall(f) => ast::ExprNode::FnCall(ast::FnCallNode::from(f)),
            cst::ExprNode::FnDef(f) => ast::ExprNode::FnDef(ast::FnDefNode::from(f)),
            cst::ExprNode::Paren(p) => ast::ExprNode::from(*p.child),
            cst::ExprNode::Number(n) => ast::ExprNode::Number(ast::NumberNode::from(n)),
            cst::ExprNode::Id(i) => ast::ExprNode::Id(ast::IdNode::from(i)),
        }
    }
}

impl<'a> From<cst::UnaryExprNode<'a>> for ast::UnaryExprNode {
    fn from(u: cst::UnaryExprNode<'a>) -> Self {
        ast::UnaryExprNode {
            op: u.op.op.into(),
            arg: Box::new(ast::ExprNode::from(*u.arg)),
        }
    }
}

impl From<cst::UnaryOp> for ast::UnaryOp {
    fn from(u: cst::UnaryOp) -> Self {
        match u {
            cst::UnaryOp::Neg => ast::UnaryOp::Neg,
            cst::UnaryOp::Not => ast::UnaryOp::Not,
        }
    }
}

impl<'a> From<cst::BinaryExprNode<'a>> for ast::BinaryExprNode {
    fn from(b: cst::BinaryExprNode<'a>) -> Self {
        ast::BinaryExprNode {
            op: b.op.op.into(),
            left: Box::new(ast::ExprNode::from(*b.left)),
            right: Box::new(ast::ExprNode::from(*b.right)),
        }
    }
}

impl From<cst::BinaryOp> for ast::BinaryOp {
    fn from(b: cst::BinaryOp) -> Self {
        match b {
            cst::BinaryOp::Divide => ast::BinaryOp::Divide,
            cst::BinaryOp::Plus => ast::BinaryOp::Plus,
            cst::BinaryOp::Minus => ast::BinaryOp::Minus,
            cst::BinaryOp::Times => ast::BinaryOp::Times,
            cst::BinaryOp::And => ast::BinaryOp::And,
            cst::BinaryOp::Or => ast::BinaryOp::Or,
        }
    }
}

impl<'a> From<cst::BlockExprNode<'a>> for ast::BlockExprNode {
    fn from(b: cst::BlockExprNode<'a>) -> Self {
        ast::BlockExprNode {
            statements: b.statements.into_iter().map(|s| s.into()).collect(),
            ret: Box::new(ast::ExprNode::from(*b.ret)),
        }
    }
}

impl<'a> From<cst::FnCallNode<'a>> for ast::FnCallNode {
    fn from(f: cst::FnCallNode<'a>) -> Self {
        ast::FnCallNode {
            function: Box::new(ast::ExprNode::from(*f.function)),
            args: f
                .args
                .into_iter()
                .map(|arg| ast::ExprNode::from(arg))
                .collect(),
        }
    }
}

impl<'a> From<cst::FnDefNode<'a>> for ast::FnDefNode {
    fn from(f: cst::FnDefNode<'a>) -> Self {
        ast::FnDefNode {
            arg_names: f
                .arg_names
                .into_iter()
                .map(|a| ast::IdRef::from(a))
                .collect(),
            body: Box::new(ast::ExprNode::from(*f.body)),
        }
    }
}

impl<'a> From<cst::IfExprNode<'a>> for ast::IfExprNode {
    fn from(i: cst::IfExprNode<'a>) -> Self {
        ast::IfExprNode {
            cond: Box::new(ast::ExprNode::from(*i.cond)),
            on_true: Box::new(ast::ExprNode::from(*i.on_true)),
            on_false: Box::new(ast::ExprNode::from(*i.on_false)),
        }
    }
}

impl<'a> From<cst::NumberNode<'a>> for ast::NumberNode {
    fn from(n: cst::NumberNode<'a>) -> Self {
        ast::NumberNode { val: n.val }
    }
}

impl<'a> From<cst::IdNode<'a>> for ast::IdNode {
    fn from(i: cst::IdNode<'a>) -> Self {
        ast::IdNode {
            id_text: i.id_text.to_string(),
        }
    }
}