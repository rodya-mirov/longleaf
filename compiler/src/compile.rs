use lang::ast;

use crate::{chunk::Chunk, ops::OpCode, Obj, ObjString, Value};

pub struct CompileContext {
    pub(crate) current_chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
}

trait VecExt {
    type Item;

    fn peek(&self) -> Option<&Self::Item>;
}

impl<T> VecExt for Vec<T> {
    type Item = T;

    fn peek(&self) -> Option<&Self::Item> {
        if self.is_empty() {
            None
        } else {
            Some(&self[self.len() - 1])
        }
    }
}

#[derive(Debug, Clone)]
struct Local {
    name: String,
    // this will always be >=1 because depth 0 is globals,
    // and globals are handled differently
    scope_depth: usize,
}

pub type CompileResult<T = ()> = Result<T, CompileError>;

#[derive(Debug, Clone)]
pub enum CompileError {
    TooManyConstants,
    TooManyLocals,
    JumpTooFar,
}

// TODO: line numbers throughout
impl CompileContext {
    pub fn new() -> Self {
        Self {
            current_chunk: Chunk::new(),
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    // Used by the REPL for continuing compilation to an existing chunk
    // TODO: Once we get into multiline statements (blocks) in the REPL we will probably
    // need to rethink this.
    pub fn new_with(chunk: Chunk) -> Self {
        Self {
            current_chunk: chunk,
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub fn into_chunk(self) -> Chunk {
        self.current_chunk
    }

    pub fn compile_stmt(&mut self, stmt: ast::StmtNode) -> CompileResult {
        match stmt {
            ast::StmtNode::Print(stmt) => self.compile_print_stmt(stmt),
            ast::StmtNode::Assign(stmt) => self.compile_assign_stmt(stmt),
            ast::StmtNode::Expr(stmt) => self.compile_expr_stmt(stmt),
        }
    }

    fn compile_print_stmt(&mut self, stmt: ast::PrintStmt) -> CompileResult {
        self.compile_expr(stmt.expr)?;
        self.current_chunk.write_chunk(OpCode::OP_PRINT as u8, 0);
        Ok(())
    }

    // note this is the "let x = whatever;" format; it creates a new variable every time,
    // potentially shadowing
    fn compile_assign_stmt(&mut self, stmt: ast::AssignStmt) -> CompileResult {
        self.compile_expr(stmt.rhs)?;

        if self.scope_depth == 0 {
            // TODO: don't make a new ci every time?
            let ci = self.register_string_value(stmt.lhs.name);
            if ci > u8::MAX as usize {
                return Err(CompileError::TooManyConstants);
            }

            self.current_chunk
                .write_chunk(OpCode::OP_DEFINE_GLOBAL as u8, 0);
            self.current_chunk.write_chunk(ci as u8, 0);
        } else {
            let local_idx = self.locals.len();
            if local_idx >= u8::MAX as usize {
                return Err(CompileError::TooManyLocals);
            }
            let local = Local {
                name: stmt.lhs.name,
                scope_depth: self.scope_depth,
            };
            self.locals.push(local);
            // Note we don't actually have to emit any code (!!)
            // The value is at the correct point
            // When we go to access it, that'll matter more
            // NOTE: we do allow shadowing and it's ... fine? We could detect it and pop the old one but not sure I care.
        }

        Ok(())
    }

    fn compile_if_expr(&mut self, node: ast::IfExprNode) -> CompileResult {
        self.compile_expr(*node.cond)?;

        let start_pos = self.current_chunk.code.len();
        self.emit_placeholder_jump(OpCode::OP_JUMP_IF_FALSE)?;

        // if true, pop the condition value ...
        self.current_chunk.write_chunk(OpCode::OP_POP as u8, 0);

        self.compile_expr(*node.on_true)?;

        // the "if-false" is now complete; jump to the end (we'll rewrite the jump in a moment)
        let else_start_pos = self.current_chunk.code.len();
        self.emit_placeholder_jump(OpCode::OP_JUMP)?;

        // otherwise if the original position was false, we've now jumped to here
        self.back_patch_jump(start_pos)?;

        // and now we can do the if-false
        self.compile_expr(*node.on_false)?;

        self.back_patch_jump(else_start_pos)?;

        Ok(())
    }

    /// Emit a "jump instruction" with two placeholder (zero) byte arguments.
    #[inline]
    fn emit_placeholder_jump(&mut self, jump_instr: OpCode) -> CompileResult {
        self.current_chunk.write_chunk(jump_instr as u8, 0);
        self.current_chunk.write_chunk(0, 0);
        self.current_chunk.write_chunk(0, 0);
        Ok(())
    }

    /// We emit forward jumps (OP_JUMP, OP_JUMP_IF_FALSE) with a 2-byte argument placeholder.
    /// This should be called immediately after the "jump over" has been compiled,
    /// and with the argument start_pos to be the index of the jump to be replaced.
    ///
    /// Returns error if it was more than u16::MAX ops to skip.
    #[inline]
    fn back_patch_jump(&mut self, start_pos: usize) -> CompileResult {

        let covered_distance = self.current_chunk.code.len() - start_pos - 3;
        if covered_distance > (u16::MAX as usize) {
            return Err(CompileError::JumpTooFar);
        }
        let covered_distance = covered_distance as u16;
        self.current_chunk.code[start_pos + 1] = (covered_distance >> 8) as u8;
        self.current_chunk.code[start_pos + 2] = (covered_distance & (u8::MAX as u16)) as u8;

        Ok(())
    }

    fn compile_block_expr(&mut self, block: ast::BlockExprNode) -> CompileResult {
        self.scope_depth += 1;

        for stmt in block.statements {
            self.compile_stmt(stmt)?;
        }

        self.compile_expr(*block.ret)?;

        while let Some(next) = self.locals.peek() {
            if next.scope_depth == self.scope_depth {
                self.current_chunk.write_chunk(OpCode::OP_POP_SWAP as u8, 0);
                let _ = self.locals.pop();
            } else {
                break;
            }
        }

        self.scope_depth -= 1;

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
            ast::ExprNode::Block(b) => self.compile_block_expr(b),
            ast::ExprNode::If(i) => self.compile_if_expr(i),
            ast::ExprNode::FnCall(_) => {
                unimplemented!("Compiling function calls is not yet supported")
            }
            ast::ExprNode::FnDef(_) => {
                unimplemented!("Compiling function definitions is not yet supported")
            }
            ast::ExprNode::Nil(_) => self.compile_nil(),
            ast::ExprNode::BoolConst(b) => self.compile_bool_const(b),
            ast::ExprNode::Number(n) => self.compile_number(n),
            ast::ExprNode::String(s) => self.compile_str(s),
            ast::ExprNode::Id(i) => self.compile_id_ref(i),
        }
    }

    fn compile_nil(&mut self) -> CompileResult {
        self.current_chunk.write_chunk(OpCode::OP_NIL as u8, 0);
        Ok(())
    }

    fn compile_bool_const(&mut self, b: ast::BoolConstNode) -> CompileResult {
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

    fn register_string_value(&mut self, val: String) -> usize {
        let obj: Obj = Obj::ObjString(ObjString { val });
        let val: *mut Obj = Box::into_raw(Box::new(obj));
        let val: Value = Value::Object(val);

        self.current_chunk.add_constant(val)
    }

    fn compile_str(&mut self, s: ast::StringNode) -> CompileResult {
        let ci = self.register_string_value(s.val);
        if ci > (u8::MAX as usize) {
            return Err(CompileError::TooManyConstants);
        }

        self.current_chunk.write_chunk(OpCode::OP_CONSTANT as u8, 0);
        self.current_chunk.write_chunk(ci as u8, 0);

        Ok(())
    }

    fn compile_id_ref(&mut self, i: ast::IdRefNode) -> CompileResult {
        // TODO: if you do `let x = 12; print x; print x; ...` too many times, this will overflow
        // That's stupid; we should reuse the same constant index for the same string, especially
        // for variable references
        for (stack_idx, local) in self.locals.iter().enumerate().rev() {
            if i.name == local.name {
                self.current_chunk
                    .write_chunk(OpCode::OP_GET_LOCAL as u8, 0);
                self.current_chunk.write_chunk(stack_idx as u8, 0);
                return Ok(());
            }
        }

        // If we got here, it's not a local, so we'll assume it's a (possibly late-bound) global
        let ci = self.register_string_value(i.name);
        if ci > (u8::MAX as usize) {
            return Err(CompileError::TooManyConstants);
        }

        // TODO: once we have local variables we'll need to check this isn't a reserved local name
        self.current_chunk
            .write_chunk(OpCode::OP_GET_GLOBAL as u8, 0);
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
        let left = *b.left;
        let right = *b.right;

        match b.op {
            ast::BinaryOp::Plus => self.normal_binary_op(left, right, OpCode::OP_ADD),
            ast::BinaryOp::Minus => self.normal_binary_op(left, right, OpCode::OP_SUBTRACT),
            ast::BinaryOp::Times => self.normal_binary_op(left, right, OpCode::OP_MULTIPLY),
            ast::BinaryOp::Divide => self.normal_binary_op(left, right, OpCode::OP_DIVIDE),
            ast::BinaryOp::Geq => self.normal_binary_op(left, right, OpCode::OP_GEQ),
            ast::BinaryOp::Gt => self.normal_binary_op(left, right, OpCode::OP_GT),
            ast::BinaryOp::Eq => self.normal_binary_op(left, right, OpCode::OP_EQ),
            ast::BinaryOp::Neq => self.normal_binary_op(left, right, OpCode::OP_NEQ),
            ast::BinaryOp::Leq => self.normal_binary_op(left, right, OpCode::OP_LEQ),
            ast::BinaryOp::Lt => self.normal_binary_op(left, right, OpCode::OP_LT),
            ast::BinaryOp::And => {
                self.compile_expr(left)?;

                let jump_start = self.current_chunk.code.len();
                self.emit_placeholder_jump(OpCode::OP_JUMP_IF_FALSE)?;

                self.current_chunk.write_chunk(OpCode::OP_POP as u8, 0);
                self.compile_expr(right)?;
                self.back_patch_jump(jump_start)?;

                Ok(())
            }
            ast::BinaryOp::Or => {
                self.compile_expr(left)?;

                // TODO perf: this is dumb, make a JUMP_IF_TRUE instruction
                let if_false_start = self.current_chunk.code.len();
                self.emit_placeholder_jump(OpCode::OP_JUMP_IF_FALSE)?;

                let if_true_start = self.current_chunk.code.len();
                self.emit_placeholder_jump(OpCode::OP_JUMP)?;

                self.back_patch_jump(if_false_start)?;

                self.current_chunk.write_chunk(OpCode::OP_POP as u8, 0);

                self.compile_expr(right)?;

                self.back_patch_jump(if_true_start)?;

                Ok(())
            }
        }
    }

    fn normal_binary_op(&mut self, left: ast::ExprNode, right: ast::ExprNode, op: OpCode) -> CompileResult {
        self.compile_expr(left)?;
        self.compile_expr(right)?;
        self.current_chunk.write_chunk(op as u8, 0);
        Ok(())
    }
}
