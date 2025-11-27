use crate::global::config::BType;
use crate::sc::ast::{Block, FuncDef, FuncFParam, ReturnVal};
use std::cell::RefCell;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub enum KoopaOpCode {
    NE,
    EQ,
    GT,
    LT,
    GE,
    LE, // comparison
    ADD,
    SUB,
    MUL,
    DIV,
    MOD, // arithmetic
    AND,
    OR,
    XOR, // bitwise
    SHL,
    SHR,
    SAR, // bitwise shift
    STORE,
    LOAD,
    ALLOC, // store, load & ALLOC
    CALL,
    BR,
    JUMP,
    RET,
}

impl std::fmt::Display for KoopaOpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KoopaOpCode::NE => write!(f, "ne"),
            KoopaOpCode::EQ => write!(f, "eq"),
            KoopaOpCode::GT => write!(f, "gt"),
            KoopaOpCode::LT => write!(f, "lt"),
            KoopaOpCode::GE => write!(f, "ge"),
            KoopaOpCode::LE => write!(f, "le"),
            KoopaOpCode::ADD => write!(f, "add"),
            KoopaOpCode::SUB => write!(f, "sub"),
            KoopaOpCode::MUL => write!(f, "mul"),
            KoopaOpCode::DIV => write!(f, "div"),
            KoopaOpCode::MOD => write!(f, "mod"),
            KoopaOpCode::AND => write!(f, "and"),
            KoopaOpCode::OR => write!(f, "or"),
            KoopaOpCode::XOR => write!(f, "xor"),
            KoopaOpCode::SHL => write!(f, "shl"),
            KoopaOpCode::SHR => write!(f, "shr"),
            KoopaOpCode::SAR => write!(f, "sar"),
            KoopaOpCode::STORE => write!(f, "store"),
            KoopaOpCode::LOAD => write!(f, "load"),
            KoopaOpCode::ALLOC => write!(f, "alloc"),
            KoopaOpCode::CALL => write!(f, "call"),
            KoopaOpCode::BR => write!(f, "br"),
            KoopaOpCode::JUMP => write!(f, "jump"),
            KoopaOpCode::RET => write!(f, "ret"),
        }
    }
}

#[derive(Debug)]
pub struct IdAllocator {
    current_id: u32,
}

impl IdAllocator {
    pub fn new() -> Self {
        IdAllocator { current_id: 0 }
    }

    pub fn alloc(&mut self) -> u32 {
        let current_id = self.current_id;
        self.current_id += 1;
        current_id
    }

    pub fn get_next_id(&self) -> u32 {
        self.current_id
    }
}

thread_local! {
    // initialize sc_var id allocator
    pub static PTR_ID_ALLOCATOR: RefCell<IdAllocator> = RefCell::new(IdAllocator::new());
    // initialize koopa ir block id allocator
    pub static BLOCK_ID_ALLOCATOR: RefCell<IdAllocator> = RefCell::new(IdAllocator::new());
    // initialize ir inst id allocator
    pub static IR_VAR_ID_ALLOCATOR: RefCell<IdAllocator> = RefCell::new(IdAllocator::new());
    // sysy standard library function declarations
    pub static SYSY_STD_LIB: Vec<FuncDef> = vec![
        FuncDef {
            func_type: BType::Int,
            ident: "getint".to_string(),
            params: vec![],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
        FuncDef {
            func_type: BType::Int,
            ident: "getch".to_string(),
            params: vec![],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
        FuncDef {
            func_type: BType::Int,
            ident: "getarray".to_string(),
            params: vec![FuncFParam {
                param_type: BType::Pointer(Box::new(BType::Int)),
                ident: "".to_string(),
            }],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
        FuncDef {
            func_type: BType::Void,
            ident: "putint".to_string(),
            params: vec![FuncFParam {
                param_type: BType::Int,
                ident: "".to_string(),
            }],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
        FuncDef {
            func_type: BType::Void,
            ident: "putch".to_string(),
            params: vec![FuncFParam {
                param_type: BType::Int,
                ident: "".to_string(),
            }],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
        FuncDef {
            func_type: BType::Void,
            ident: "putarray".to_string(),
            params: vec![
                FuncFParam {
                    param_type: BType::Int,
                    ident: "".to_string(),
                },
                FuncFParam {
                    param_type: BType::Pointer(Box::new(BType::Int)),
                    ident: "".to_string(),
                },
            ],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
        FuncDef {
            func_type: BType::Void,
            ident: "starttime".to_string(),
            params: vec![],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
        FuncDef {
            func_type: BType::Void,
            ident: "stoptime".to_string(),
            params: vec![],
            block: Block { block_items: vec![] },
            return_val: RefCell::new(Some(ReturnVal::Other)),
        },
    ];
}
