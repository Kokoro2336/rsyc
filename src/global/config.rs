use crate::ir::koopa::IRObj;
use crate::asm::config::RISCV_BITS;

/// type of value
#[derive(Debug, Clone)]
pub enum BType {
    Int,
    Void,
    Array { typ: Box<BType>, len: u32 },
    Pointer { typ: Box<BType> },
    Unknown,
}

impl BType {
    pub fn size_in_bytes(&self) -> u32 {
        match self {
            BType::Int => 4,
            BType::Void => 0,
            BType::Array { typ, len } => typ.size_in_bytes() * len,
            BType::Pointer { typ } => RISCV_BITS / 8,
            BType::Unknown => panic!("Cannot get size of unknown type"),
        }
    }
}

impl std::fmt::Display for BType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BType::Int => write!(f, "i32"),
            BType::Void => write!(f, "void"),
            BType::Array { typ, len } => write!(f, "[{}, {}]", typ, len),
            BType::Pointer { typ } => write!(f, "*{}", typ),
            BType::Unknown => write!(f, "unknown"),
        }
    }
}
