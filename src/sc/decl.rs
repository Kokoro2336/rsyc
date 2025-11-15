use crate::global::config::BType;
use crate::global::context::SC_CONTEXT_STACK;
use crate::ir::config::{KoopaOpCode, PTR_ID_ALLOCATOR};
use crate::ir::koopa::{insert_ir, IRObj, InstData};
use crate::sc::exp::{Exp, Expression};

use std::vec::Vec;

pub trait Declaration {
    fn parse(&self) -> IRObj;
    fn parse_global(&self) -> IRObj {
        IRObj::None
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    ConstDecl { const_decl: ConstDecl },
    VarDecl { var_decl: VarDecl },
}

impl Decl {
    pub fn parse(&self) {
        match self {
            Decl::ConstDecl { const_decl } => {
                const_decl.parse();
            }
            Decl::VarDecl { var_decl } => {
                var_decl.parse();
            }
        }
    }

    pub fn parse_global(&self) -> Vec<IRObj> {
        match self {
            Decl::ConstDecl { const_decl } => const_decl.parse_global(),
            Decl::VarDecl { var_decl } => var_decl.parse_global(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

impl ConstDecl {
    fn parse(&self) {
        for const_def in &self.const_defs {
            let result = const_def.parse();
            SC_CONTEXT_STACK.with(|stack| {
                stack
                    .borrow_mut()
                    .insert_const(const_def.ident.clone(), result)
            });
        }
    }

    fn parse_global(&self) -> Vec<IRObj> {
        self.const_defs
            .iter()
            .map(|const_def| {
                let result = const_def.parse_global();
                SC_CONTEXT_STACK.with(|stack| {
                    stack
                        .borrow_mut()
                        .insert_global_sym(const_def.ident.clone(), result.clone())
                });
                result
            })
            .collect::<Vec<IRObj>>()
    }
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

impl Declaration for ConstDef {
    fn parse(&self) -> IRObj {
        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_sc_var(self.ident.as_str()))
            .is_some()
        {
            panic!(
                "Cannot declare constant {} with the same name as a variable",
                self.ident
            );
        }

        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_const(self.ident.as_str()))
            .is_some()
        {
            panic!("Constant {} already declared", self.ident);
        }

        self.const_init_val.parse()
    }

    fn parse_global(&self) -> IRObj {
        self.const_init_val.parse()
    }
}

#[derive(Debug, Clone)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}

impl Declaration for ConstInitVal {
    fn parse(&self) -> IRObj {
        self.const_exp.parse()
    }
}

#[derive(Debug, Clone)]
pub struct ConstExp {
    pub exp: Box<Exp>,
}

impl Declaration for ConstExp {
    fn parse(&self) -> IRObj {
        self.exp.parse_const_exp()
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub b_type: BType,
    pub var_defs: Vec<VarDef>,
}

impl VarDecl {
    fn parse(&self) {
        for var_def in &self.var_defs {
            let result = var_def.parse();
            // insert sc_var into sc_var table for parsing first.
            SC_CONTEXT_STACK.with(|stack| {
                stack
                    .borrow_mut()
                    .insert_sc_var(var_def.ident.clone(), result)
            });
        }
    }

    fn parse_global(&self) -> Vec<IRObj> {
        self.var_defs
            .iter()
            .map(|var_def| {
                let result = var_def.parse_global();

                // insert sc_var into sc_var table for parsing first.
                SC_CONTEXT_STACK.with(|stack| {
                    stack
                        .borrow_mut()
                        .insert_global_sym(var_def.ident.clone(), result.clone())
                });

                result
            })
            .collect::<Vec<IRObj>>()
    }
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: String,
    pub init_val: Option<InitVal>,
}

impl Declaration for VarDef {
    fn parse(&self) -> IRObj {
        // semantic check
        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_const(self.ident.as_str()))
            .is_some()
        {
            panic!(
                "Cannot declare variable {} with the same name as a constant",
                self.ident
            );
        }

        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_sc_var(self.ident.as_str()))
            .is_some()
        {
            panic!("Variable {} already declared", self.ident);
        }

        let sc_var_id = PTR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc());
        // whatever the init_val is, we need to allocate space for the variable
        insert_ir(InstData::new(
            BType::Int,
            IRObj::ScVar {
                initialized: self.init_val.is_some(),
                sc_var_id, // placeholder, will be replaced
            },
            KoopaOpCode::ALLOC,
            vec![IRObj::BType(BType::Int)],
        ));

        if let Some(init_val) = &self.init_val {
            let parse_result = init_val.parse();
            // we don't need to store temp var to var_table here for it'll be removed soon after STORE
            if let IRObj::Const(_) | IRObj::IRVar(_) = parse_result {
                insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::STORE,
                    vec![
                        match parse_result {
                            IRObj::IRVar(id) => IRObj::IRVar(id),
                            IRObj::Const(c) => IRObj::Const(c),
                            _ => unreachable!(),
                        },
                        // the allocated address
                        IRObj::ScVar {
                            initialized: self.init_val.is_some(),
                            sc_var_id,
                        },
                    ],
                ));
            }
        };

        IRObj::ScVar {
            initialized: self.init_val.is_some(),
            sc_var_id,
        }
    }

    fn parse_global(&self) -> IRObj {
        if SC_CONTEXT_STACK.with(|stack| {
            let global_sym = stack.borrow().get_global_sym(&self.ident);
            global_sym.is_some() && !matches!(Some(IRObj::None), global_sym)
        }) {
            panic!("Global variable {} already declared", self.ident);
        }

        let parse_result = match &self.init_val {
            Some(init_val) => init_val.parse(),
            None => IRObj::Const(0),
        };

        IRObj::GlobalVar {
            initialized: self.init_val.is_some(),
            global_var_id: self.ident.clone(),
            init_val: match parse_result {
                IRObj::Const(c) => c,
                _ => unreachable!(
                    "Global variable {} must be initialized with a constant",
                    &self.ident
                ),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct InitVal {
    pub exp: Box<Exp>,
}

impl Declaration for InitVal {
    fn parse(&self) -> IRObj {
        self.exp.parse_var_exp()
    }
}
