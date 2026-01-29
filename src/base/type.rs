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

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Void => write!(f, "void"),
            Type::Array { base, dims } => {
                write!(f, "{}", base)?;
                for dim in dims {
                    write!(f, "[{}]", dim)?;
                }
                Ok(())
            }
            Type::Pointer { base } => {
                write!(f, "{}*", base)
            }
        }
    }
}

impl Type {
    pub fn size_in_bytes(&self) -> u32 {
        match self {
            Type::Int => 4,
            Type::Float => 4,
            Type::Void => 0,
            Type::Array { base, dims } => base.size_in_bytes() * dims.iter().product::<u32>(),
            Type::Pointer { .. } => RISCV_BITS / 8,
        }
    }
}
