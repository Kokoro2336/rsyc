/// type of value
#[derive(Debug, Clone)]
pub enum BType {
    Int,
    Void,
    Pointer(Box<BType>),
}

impl BType {
    pub fn size_in_bytes(&self) -> u32 {
        match self {
            BType::Int => 4,
            BType::Void => 0,
            BType::Pointer(_) => unimplemented!(),
        }
    }
}

impl std::fmt::Display for BType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BType::Int => write!(f, "i32"),
            BType::Void => write!(f, "void"),
            BType::Pointer(b_type) => write!(f, "*{b_type}"),
        }
    }
}
