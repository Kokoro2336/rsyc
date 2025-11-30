use crate::global::config::BType;
use crate::global::context::{RETURN_TYPES, SC_CONTEXT_STACK};
use crate::ir::config::{KoopaOpCode, SC_VAR_ID_ALLOCATOR};
use crate::ir::koopa::{insert_ir, BasicBlock, BasicBlockType, Func, IRObj, InstData, Program};
use crate::sc::decl::Decl;
use crate::sc::exp::{Exp, Expression};
use crate::sc::stmt::{Statement, Stmt};

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum GlobalObj {
    GlobalDecl(Decl),
    FuncDef(FuncDef),
}

/// define the AST structure
#[derive(Debug)]
pub struct CompUnit {
    pub global_decls: Vec<Decl>,
    pub func_defs: Vec<FuncDef>,
}

impl CompUnit {
    pub fn parse(&self) -> Program {
        // construct Program and return
        let mut program = Program::new();

        // init all global defs in global table first for later searching
        self.global_decls
            .iter()
            .for_each(|global_decl| match global_decl {
                Decl::ConstDecl { const_decl } => {
                    const_decl.const_defs.iter().for_each(|def| {
                        SC_CONTEXT_STACK.with(|stack| {
                            stack
                                .borrow_mut()
                                .insert_global_sym(def.ident.clone(), IRObj::None)
                        })
                    });
                }
                Decl::VarDecl { var_decl } => var_decl.var_defs.iter().for_each(|def| {
                    SC_CONTEXT_STACK.with(|stack| {
                        stack
                            .borrow_mut()
                            .insert_global_sym(def.ident.clone(), IRObj::None)
                    })
                }),
            });

        program.global_vals = self
            .global_decls
            .iter()
            .fold(vec![], |mut acc, global_decl| {
                let parse_result = global_decl
                    .parse_global()
                    .iter()
                    .filter(|obj| {
                        // filter out global decls that are single const decls
                        !(matches!(global_decl, Decl::ConstDecl { .. })
                            && matches!(obj, IRObj::Const(_)))
                    })
                    .cloned()
                    .collect::<Vec<IRObj>>();

                acc.extend(parse_result);
                acc
            });

        self.func_defs.iter().for_each(|func_def| {
            RETURN_TYPES.with(|ret_types| {
                ret_types.borrow_mut().enter_func();
            });
            func_def.find_return_val();
            *func_def.return_val.borrow_mut() =
                Some(RETURN_TYPES.with(|ret_types| ret_types.borrow_mut().analyze_return_type()));
        });

        println!(
            "{:#?}",
            &self
                .func_defs
                .iter()
                .map(|f| (&f.ident, f.return_val.borrow().clone()))
                .collect::<Vec<(&String, Option<ReturnVal>)>>()
        );

        if !self
            .func_defs
            .iter()
            .any(|f| f.ident == "main" && matches!(f.func_type, BType::Int))
        {
            panic!("No main function with return type int found.");
        }

        for func in &self.func_defs {
            program.push_func(func.parse());
        }

        program
    }

    pub fn push_obj(&mut self, global_obj: GlobalObj) {
        match global_obj {
            GlobalObj::GlobalDecl(global_decl) => self.global_decls.push(global_decl),
            GlobalObj::FuncDef(func_def) => self.func_defs.push(func_def),
        }
    }

    pub fn get_func_def(&self, name: &String) -> Option<&FuncDef> {
        for func in &self.func_defs {
            if func.ident == *name {
                return Some(func);
            }
        }
        panic!("Function {} not found.", name);
    }
}

#[derive(Debug, Clone)]
pub enum ReturnVal {
    AlwaysZero,
    AlwaysNonZero,
    Other,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub func_type: BType,
    pub ident: String,
    pub params: Vec<FuncFParam>,
    pub block: Block,
    // whether the function always has a non-zero return statement
    pub return_val: RefCell<Option<ReturnVal>>,
}

impl FuncDef {
    fn parse(&self) -> Rc<Func> {
        // get func type and ident
        let func_type = &self.func_type;
        let func_name = self.ident.clone();

        let func = Rc::new(Func::new(func_name, func_type.clone(), self.params.clone()));
        SC_CONTEXT_STACK.with(|stack| {
            stack.borrow_mut().enter_func(Rc::clone(&func));
        });

        let basic_block = Rc::new(BasicBlock::new(BasicBlockType::FuncEntry));
        SC_CONTEXT_STACK.with(|stack| {
            let mut stack_mut = stack.borrow_mut();
            stack_mut.enter_block(Rc::clone(&basic_block));
            stack_mut.get_current_func().push_basic_block(basic_block);
        });

        self.init_func_param(); // process params
        self.block.parse();

        SC_CONTEXT_STACK.with(|stack| {
            stack.borrow_mut().exit_func();
        });

        func
    }

    fn find_return_val(&self) {
        self.block.find_return_val();
    }

    fn init_func_param(&self) {
        self.params.iter().enumerate().for_each(|(index, param)| {
            let sc_var_id = SC_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc());
            let sc_var = IRObj::ScVar {
                initialized: true,
                sc_var_id,
            };

            insert_ir(InstData::new(
                BType::Int,
                sc_var.clone(),
                KoopaOpCode::ALLOC,
                vec![IRObj::BType(BType::Int)],
            ));

            insert_ir(InstData::new(
                BType::Void,
                IRObj::None,
                KoopaOpCode::STORE,
                vec![
                    IRObj::Param {
                        param_type: param.param_type.clone(),
                        idx: index as u32,
                        ident: param.ident.clone(),
                    },
                    sc_var.clone(),
                ],
            ));

            // add to global_sc_var_table
            SC_CONTEXT_STACK.with(|stack| {
                stack
                    .borrow_mut()
                    .insert_sc_var(param.ident.clone(), sc_var);
            });
        });
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub struct FuncFParam {
    pub param_type: BType,
    pub ident: String,
    pub const_exps: Vec<Option<Exp>>,
}

impl std::fmt::Display for FuncFParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.param_type, self.ident);
        Ok(())
    }
}

impl Block {
    pub fn parse(&self) {
        for item in &self.block_items {
            item.parse();
        }
        // add return instruction if not present
        let (end_with_return, func_name, func_type) = SC_CONTEXT_STACK.with(|stack| {
            let stack_mut = stack.borrow_mut();
            let current_func = stack_mut.get_current_func();
            (
                current_func.end_with_return(),
                current_func.name.clone(),
                current_func.func_type.clone(),
            )
        });

        if !end_with_return {
            match func_type {
                BType::Void => {
                    insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::RET,
                        vec![],
                    ));
                }
                _ => {
                    // when panic here, the compiler can't gen correct ir, but this won't happen when do nothing
                }
            }
        };
    }

    pub fn find_return_val(&self) {
        for item in &self.block_items {
            item.find_return_val();
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

    pub fn find_return_val(&self) {
        if let BlockItem::Stmt { stmt } = self {
            stmt.find_return_val();
        }
    }
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub ident: String,
    pub exps: Vec<Exp>,
}

impl std::fmt::Display for LVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.exps.len() > 0 {
            write!(
                f,
                "{}{}",
                self.ident,
                self.exps
                    .iter()
                    .map(|exp| format!("[{}]", exp.evaluate().to_string()))
                    .collect::<String>()
            );
        } else {
            write!(f, "{}", self.ident);
        }
        Ok(())
    }
}
