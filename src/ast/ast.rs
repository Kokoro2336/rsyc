use crate::ast::decl::Decl;
use crate::ast::stmt::{Statement, Stmt};
use crate::config::config::{BType, CONTEXT_STACK};
use crate::koopa_ir::koopa_ir::{BasicBlock, BasicBlockType, Func, Program};

use std::rc::Rc;

/// define the AST structure
#[derive(Debug)]
pub struct CompUnit {
    pub global_decls: Vec<Decl>,
    pub func_defs: Vec<FuncDef>,
}

impl CompUnit {
    pub fn parse(&self) -> Result<Program, Box<dyn std::error::Error>> {
        // construct Program and return
        let mut program = Program::new();
        for func in &self.func_defs {
            program.push_func(Rc::clone(&func.parse()));
        }
        Ok(program)
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: BType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    fn parse(&self) -> Rc<Func> {
        // TODO: processing global value

        // get func type and ident
        let func_type = &self.func_type;
        let func_name = self.ident.clone();

        let func = Rc::new(Func::new(func_name, func_type.clone(), vec![]));

        let basic_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
        CONTEXT_STACK.with(|stack| {
            let mut stack_mut = stack.borrow_mut();
            stack_mut.enter_func(Rc::clone(&func));
            stack_mut.enter_block(Rc::clone(&basic_block));
            stack_mut.get_current_func().push_basic_block(basic_block);
        });

        self.block.parse();

        CONTEXT_STACK.with(|stack| {
            stack.borrow_mut().exit_func();
        });

        func
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

impl Block {
    pub fn parse(&self) {
        for item in &self.block_items {
            item.parse();
        }
    }
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Decl { decl: Decl },
    Stmt { stmt: Stmt },
}

impl BlockItem {
    pub fn parse(&self) {
        match self {
            BlockItem::Decl { decl } => {
                decl.parse();
            }
            BlockItem::Stmt { stmt } => {
                stmt.parse();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub ident: String,
}
