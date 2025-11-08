use std::cell::RefCell;

use crate::ir::koopa::InstId;

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
            KoopaOpCode::BR => write!(f, "br"),
            KoopaOpCode::JUMP => write!(f, "jump"),
            KoopaOpCode::RET => write!(f, "ret"),
        }
    }
}

impl KoopaOpCode {
    pub fn has_return_value(&self) -> bool {
        match self {
            // These opcodes produce a return value
            KoopaOpCode::NE
            | KoopaOpCode::EQ
            | KoopaOpCode::GT
            | KoopaOpCode::LT
            | KoopaOpCode::GE
            | KoopaOpCode::LE
            | KoopaOpCode::ADD
            | KoopaOpCode::SUB
            | KoopaOpCode::MUL
            | KoopaOpCode::DIV
            | KoopaOpCode::MOD
            | KoopaOpCode::AND
            | KoopaOpCode::OR
            | KoopaOpCode::XOR
            | KoopaOpCode::SHL
            | KoopaOpCode::SHR
            | KoopaOpCode::SAR
            | KoopaOpCode::LOAD
            | KoopaOpCode::ALLOC => true,

            // These opcodes do not produce a return value
            KoopaOpCode::STORE | KoopaOpCode::RET | KoopaOpCode::BR | KoopaOpCode::JUMP => false,
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

#[derive(Debug, Clone)]
pub enum IRObj {
    IRVar(u32), // temp variable, display in format "%id"
    Const(i32),     // constant value, display in literal
    Pointer { initialized: bool, pointer_id: u32 }, // pointer to a variable in memory, display in format "@pointer_id"
    None,
}

impl IRObj {
    pub fn get_value(&self) -> i32 {
        match self {
            IRObj::Const(v) => *v,
            _ => panic!("Not a constant value: {:?}", self),
        }
    }

    pub fn get_id(&self) -> InstId {
        match self {
            IRObj::IRVar(id) => *id,
            _ => panic!("Not an instruction ID: {:?}", self),
        }
    }
}

impl ToString for IRObj {
    fn to_string(&self) -> String {
        match self {
            IRObj::IRVar(id) => format!("%{}", id),
            IRObj::Const(c) => format!("{}", c),
            IRObj::Pointer {
                initialized: _,
                pointer_id,
            } => format!("@{}", pointer_id),
            IRObj::None => "".to_string(),
        }
    }
}

thread_local! {
    // initialize pointer id allocator
    pub static PTR_ID_ALLOCATOR: RefCell<IdAllocator> = RefCell::new(IdAllocator::new());
    // initialize koopa ir block id allocator
    pub static BLOCK_ID_ALLOCATOR: RefCell<IdAllocator> = RefCell::new(IdAllocator::new());
    // initialize ir inst id allocator
    pub static IR_VAR_ID_ALLOCATOR: RefCell<IdAllocator> = RefCell::new(IdAllocator::new());
}
