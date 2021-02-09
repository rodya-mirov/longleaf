use lang::ast;

use crate::{chunk::Chunk, ops::OpCode, Value};
use lang::ast::BoolConstNode;

pub struct CompileContext {
    pub(crate) current_chunk: Chunk,
}

pub type CompileResult<T = ()> = Result<T, CompileError>;

#[derive(Debug, Clone)]
pub enum CompileError {
    TooManyConstants,
}

// TODO: line numbers throughout
impl CompileContext {
    pub fn new() -> Self {
        Self {
            current_chunk: Chunk::new(),
        }
    }

    pub fn into_chunk(self) -> Chunk {
        self.current_chunk
    }

    pub fn compile_stmt(&mut self, stmt: ast::StmtNode) -> CompileResult {
        match stmt {
            ast::StmtNode::Print(stmt) => self.compile_print_stmt(stmt),
            ast::StmtNode::Assign(_) => unimplemented!(),
            ast::StmtNode::Expr(stmt) => self.compile_expr_stmt(stmt),
        }
    }

    fn compile_print_stmt(&mut self, stmt: ast::PrintStmt) -> CompileResult {
        self.compile_expr(stmt.expr)?;
        self.current_chunk.write_chunk(OpCode::OP_PRINT as u8, 0);
        Ok(())
    }

    fn compile_expr_stmt(&mut self, stmt: ast::ExprStmt) -> CompileResult {
        self.compile_expr(stmt.expr)?;
        self.current_chunk.write_chunk(OpCode::OP_POP as u8, 0);
        Ok(())
    }

    fn compile_expr(&mut self, stmt: ast::ExprNode) -> CompileResult {
        match stmt {
            ast::ExprNode::Unary(u) => self.compile_unary(u),
            ast::ExprNode::Binary(b) => self.compile_binary(b),
            ast::ExprNode::Block(_) => unimplemented!(),
            ast::ExprNode::If(_) => unimplemented!(),
            ast::ExprNode::FnCall(_) => unimplemented!(),
            ast::ExprNode::FnDef(_) => unimplemented!(),
            ast::ExprNode::Nil(_) => self.compile_nil(),
            ast::ExprNode::BoolConst(b) => self.compile_bool_const(b),
            ast::ExprNode::Number(n) => self.compile_number(n),
            ast::ExprNode::Id(_) => unimplemented!(),
        }
    }

    fn compile_nil(&mut self) -> CompileResult {
        self.current_chunk.write_chunk(OpCode::OP_NIL as u8, 0);
        Ok(())
    }

    fn compile_bool_const(&mut self, b: BoolConstNode) -> CompileResult {
        let instr = match b.val {
            true => OpCode::OP_TRUE,
            false => OpCode::OP_FALSE,
        };

        self.current_chunk.write_chunk(instr as u8, 0);
        Ok(())
    }

    fn compile_number(&mut self, n: ast::NumberNode) -> CompileResult {
        let ci = self.current_chunk.add_constant(Value::Number(n.val));
        if ci > (u8::MAX as usize) {
            return Err(CompileError::TooManyConstants);
        }

        self.current_chunk.write_chunk(OpCode::OP_CONSTANT as u8, 0);
        self.current_chunk.write_chunk(ci as u8, 0);

        Ok(())
    }

    fn compile_unary(&mut self, n: ast::UnaryExprNode) -> CompileResult {
        self.compile_expr(*n.arg)?;
        match n.op {
            ast::UnaryOp::Neg => self.current_chunk.write_chunk(OpCode::OP_NEGATE as u8, 0),
            ast::UnaryOp::Not => self.current_chunk.write_chunk(OpCode::OP_NOT as u8, 0),
        }

        Ok(())
    }

    fn compile_binary(&mut self, b: ast::BinaryExprNode) -> CompileResult {
        self.compile_expr(*b.left)?;
        self.compile_expr(*b.right)?;

        match b.op {
            ast::BinaryOp::Plus => self.current_chunk.write_chunk(OpCode::OP_ADD as u8, 0),
            ast::BinaryOp::Minus => self.current_chunk.write_chunk(OpCode::OP_SUBTRACT as u8, 0),
            ast::BinaryOp::Times => self.current_chunk.write_chunk(OpCode::OP_MULTIPLY as u8, 0),
            ast::BinaryOp::Divide => self.current_chunk.write_chunk(OpCode::OP_DIVIDE as u8, 0),
            ast::BinaryOp::Geq => self.current_chunk.write_chunk(OpCode::OP_GEQ as u8, 0),
            ast::BinaryOp::Gt => self.current_chunk.write_chunk(OpCode::OP_GT as u8, 0),
            ast::BinaryOp::Eq => self.current_chunk.write_chunk(OpCode::OP_EQ as u8, 0),
            ast::BinaryOp::Neq => self.current_chunk.write_chunk(OpCode::OP_NEQ as u8, 0),
            ast::BinaryOp::Leq => self.current_chunk.write_chunk(OpCode::OP_LEQ as u8, 0),
            ast::BinaryOp::Lt => self.current_chunk.write_chunk(OpCode::OP_LT as u8, 0),
            ast::BinaryOp::And => unimplemented!(),
            ast::BinaryOp::Or => unimplemented!(),
        }

        Ok(())
    }
}
