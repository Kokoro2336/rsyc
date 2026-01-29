pub const REG_IDLE: u32 = u32::MAX;
pub const STK_FRM_BASE_LENGTH: u32 = 16; // 16 bytes for minimum
pub const RISCV_BITS: u32 = 32;
pub const REG_PARAMS_MAX_NUM: u32 = 8;

#[derive(Clone, Debug)]
pub enum RVOp {
    BEQZ,
    BNEZ,
    J,
    CALL,
    RET,
    LW,
    SW,
    ADD,
    ADDI,
    SUB,
    SLT,
    SGT,
    SEQZ,
    SNEZ,
    XOR,
    XORI,
    OR,
    ORI,
    AND,
    ANDI,
    SLL,
    SRL,
    SRA,
    MUL,
    DIV,
    REM,
    LI,
    LA,
    MV,
}

impl std::fmt::Display for RVOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RVOp::BEQZ => write!(f, "beqz"),
            RVOp::BNEZ => write!(f, "bnez"),
            RVOp::J => write!(f, "j"),
            RVOp::CALL => write!(f, "call"),
            RVOp::RET => write!(f, "ret"),
            RVOp::LW => write!(f, "lw"),
            RVOp::SW => write!(f, "sw"),
            RVOp::ADD => write!(f, "add"),
            RVOp::ADDI => write!(f, "addi"),
            RVOp::SUB => write!(f, "sub"),
            RVOp::SLT => write!(f, "slt"),
            RVOp::SGT => write!(f, "sgt"),
            RVOp::SEQZ => write!(f, "seqz"),
            RVOp::SNEZ => write!(f, "snez"),
            RVOp::XOR => write!(f, "xor"),
            RVOp::XORI => write!(f, "xori"),
            RVOp::OR => write!(f, "or"),
            RVOp::ORI => write!(f, "ori"),
            RVOp::AND => write!(f, "and"),
            RVOp::ANDI => write!(f, "andi"),
            RVOp::SLL => write!(f, "sll"),
            RVOp::SRL => write!(f, "srl"),
            RVOp::SRA => write!(f, "sra"),
            RVOp::MUL => write!(f, "mul"),
            RVOp::DIV => write!(f, "div"),
            RVOp::REM => write!(f, "rem"),
            RVOp::LI => write!(f, "li"),
            RVOp::LA => write!(f, "la"),
            RVOp::MV => write!(f, "mv"),
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Reg {
    ZERO = 0, // hardwired zero
    RA = 1,   // return address
    SP = 2,   // stack pointer
    GP = 3,   // global pointer
    TP = 4,   // thread pointer
    T0 = 5,
    T1 = 6,
    T2 = 7, // temporaries
    S0 = 8, // fp
    S1 = 9, // saved registers / frame sc_var
    A0 = 10,
    A1 = 11,
    A2 = 12,
    A3 = 13,
    A4 = 14,
    A5 = 15,
    A6 = 16,
    A7 = 17, // FnDecl arguments / return values
    S2 = 18,
    S3 = 19,
    S4 = 20,
    S5 = 21,
    S6 = 22,
    S7 = 23,
    S8 = 24,
    S9 = 25,
    S10 = 26,
    S11 = 27, // saved registers
    T3 = 28,
    T4 = 29,
    T5 = 30,
    T6 = 31, // temporaries
}

impl Reg {
    pub fn is_temp(&self) -> bool {
        (*self as u8 >= Reg::T0 as u8 && *self as u8 <= Reg::T2 as u8)
            || (*self as u8 >= Reg::T3 as u8 && *self as u8 <= Reg::T6 as u8)
            || (*self as u8 >= Reg::A0 as u8 && *self as u8 <= Reg::A7 as u8)
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::ZERO => write!(f, "x0"),
            Reg::RA => write!(f, "x1"),
            Reg::SP => write!(f, "x2"),
            Reg::GP => write!(f, "x3"),
            Reg::TP => write!(f, "x4"),
            Reg::T0 => write!(f, "x5"),
            Reg::T1 => write!(f, "x6"),
            Reg::T2 => write!(f, "x7"),
            Reg::S0 => write!(f, "x8"),
            Reg::S1 => write!(f, "x9"),
            Reg::A0 => write!(f, "x10"),
            Reg::A1 => write!(f, "x11"),
            Reg::A2 => write!(f, "x12"),
            Reg::A3 => write!(f, "x13"),
            Reg::A4 => write!(f, "x14"),
            Reg::A5 => write!(f, "x15"),
            Reg::A6 => write!(f, "x16"),
            Reg::A7 => write!(f, "x17"),
            Reg::S2 => write!(f, "x18"),
            Reg::S3 => write!(f, "x19"),
            Reg::S4 => write!(f, "x20"),
            Reg::S5 => write!(f, "x21"),
            Reg::S6 => write!(f, "x22"),
            Reg::S7 => write!(f, "x23"),
            Reg::S8 => write!(f, "x24"),
            Reg::S9 => write!(f, "x25"),
            Reg::S10 => write!(f, "x26"),
            Reg::S11 => write!(f, "x27"),
            Reg::T3 => write!(f, "x28"),
            Reg::T4 => write!(f, "x29"),
            Reg::T5 => write!(f, "x30"),
            Reg::T6 => write!(f, "x31"),
        }
    }
}

impl Reg {
    /// Get numeric index (0..=31) for use as array index
    pub fn idx(self) -> usize {
        self as usize
    }
}
