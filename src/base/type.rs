use crate::asm::config::RISCV_BITS;

/// type of value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Void,
    Float,
    Array { base: Box<Type>, dims: Vec<u32> },
    Pointer { base: Box<Type> },
}

impl Type {
    pub fn size_in_bytes(&self) -> u32 {
        match self {
            Type::Int => 4,
            Type::Float => 4,
            Type::Void => 0,
            Type::Array { base, dims } => base.size_in_bytes() * dims.iter().product::<u32>(),
            Type::Pointer { base } => RISCV_BITS / 8,
        }
    }
}
