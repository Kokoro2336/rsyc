use crate::asm::reg::RVREG_ALLOCATOR;

pub const REG_IDLE: u32 = u32::MAX;
pub const STK_FRM_BASE_LENGTH: u32 = 16; // 16 bytes for minimum
pub const RISCV_BITS: u32 = 32;

#[derive(Clone, Debug)]
pub enum RVOpCode {
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

impl std::fmt::Display for RVOpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RVOpCode::BEQZ => write!(f, "beqz"),
            RVOpCode::BNEZ => write!(f, "bnez"),
            RVOpCode::J => write!(f, "j"),
            RVOpCode::CALL => write!(f, "call"),
            RVOpCode::RET => write!(f, "ret"),
            RVOpCode::LW => write!(f, "lw"),
            RVOpCode::SW => write!(f, "sw"),
            RVOpCode::ADD => write!(f, "add"),
            RVOpCode::ADDI => write!(f, "addi"),
            RVOpCode::SUB => write!(f, "sub"),
            RVOpCode::SLT => write!(f, "slt"),
            RVOpCode::SGT => write!(f, "sgt"),
            RVOpCode::SEQZ => write!(f, "seqz"),
            RVOpCode::SNEZ => write!(f, "snez"),
            RVOpCode::XOR => write!(f, "xor"),
            RVOpCode::XORI => write!(f, "xori"),
            RVOpCode::OR => write!(f, "or"),
            RVOpCode::ORI => write!(f, "ori"),
            RVOpCode::AND => write!(f, "and"),
            RVOpCode::ANDI => write!(f, "andi"),
            RVOpCode::SLL => write!(f, "sll"),
            RVOpCode::SRL => write!(f, "srl"),
            RVOpCode::SRA => write!(f, "sra"),
            RVOpCode::MUL => write!(f, "mul"),
            RVOpCode::DIV => write!(f, "div"),
            RVOpCode::REM => write!(f, "rem"),
            RVOpCode::LI => write!(f, "li"),
            RVOpCode::LA => write!(f, "la"),
            RVOpCode::MV => write!(f, "mv"),
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RVRegCode {
    ZERO = 0, // hardwired zero
    RA = 1,   // return address
    SP = 2,   // stack pointer
    GP = 3,   // global pointer
    TP = 4,   // thread pointer
    T0 = 5,
    T1 = 6,
    T2 = 7, // temporaries
    S0 = 8,
    S1 = 9, // saved registers / frame pointer
    A0 = 10,
    A1 = 11,
    A2 = 12,
    A3 = 13,
    A4 = 14,
    A5 = 15,
    A6 = 16,
    A7 = 17, // function arguments / return values
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

#[derive(Debug, Clone)]
pub enum RVOperandType {
    Temp(RVRegCode),                            // reg temporarily allocated, often for asms transformed from the same IR.
    Perm(RVRegCode),                            // reg permanently allocated, often for the reg eventually used by the whole IR.
    Label(String),                              // label for branch/jump
    MemWithReg { offset: u32, reg: RVRegCode }, // memory location in stack frame
    None,                                       // no reg allocated
}

impl std::fmt::Display for RVOperandType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RVOperandType::Temp(reg) => write!(f, "{}", reg),
            RVOperandType::Perm(reg) => write!(f, "{}", reg),
            RVOperandType::MemWithReg { offset, reg } => {
                write!(f, "{}({})", offset, reg)
            }
            RVOperandType::Label(label) => write!(f, "{}", label),
            RVOperandType::None => write!(f, ""),
        }
    }
}

impl RVOperandType {
    pub fn get_reg(&self) -> RVRegCode {
        match self {
            RVOperandType::Temp(reg) => *reg,
            RVOperandType::Perm(reg) => *reg,
            RVOperandType::MemWithReg { reg, .. } => *reg,
            RVOperandType::Label(_) => panic!("Label type has no register!"),
            RVOperandType::None => panic!("No register allocated!"),
        }
    }

    pub fn get_offset(&self) -> u32 {
        match self {
            RVOperandType::MemWithReg { offset, .. } => *offset,
            _ => panic!("Not a MemWithReg type!"),
        }
    }

    /// this function would only free temporary registers.
    pub fn free_temp(&self) {
        if let RVOperandType::Temp(reg) = self {
            RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().free_reg(*reg));
        }
    }
}

impl std::fmt::Display for RVRegCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RVRegCode::ZERO => write!(f, "x0"),
            RVRegCode::RA => write!(f, "x1"),
            RVRegCode::SP => write!(f, "x2"),
            RVRegCode::GP => write!(f, "x3"),
            RVRegCode::TP => write!(f, "x4"),
            RVRegCode::T0 => write!(f, "x5"),
            RVRegCode::T1 => write!(f, "x6"),
            RVRegCode::T2 => write!(f, "x7"),
            RVRegCode::S0 => write!(f, "x8"),
            RVRegCode::S1 => write!(f, "x9"),
            RVRegCode::A0 => write!(f, "x10"),
            RVRegCode::A1 => write!(f, "x11"),
            RVRegCode::A2 => write!(f, "x12"),
            RVRegCode::A3 => write!(f, "x13"),
            RVRegCode::A4 => write!(f, "x14"),
            RVRegCode::A5 => write!(f, "x15"),
            RVRegCode::A6 => write!(f, "x16"),
            RVRegCode::A7 => write!(f, "x17"),
            RVRegCode::S2 => write!(f, "x18"),
            RVRegCode::S3 => write!(f, "x19"),
            RVRegCode::S4 => write!(f, "x20"),
            RVRegCode::S5 => write!(f, "x21"),
            RVRegCode::S6 => write!(f, "x22"),
            RVRegCode::S7 => write!(f, "x23"),
            RVRegCode::S8 => write!(f, "x24"),
            RVRegCode::S9 => write!(f, "x25"),
            RVRegCode::S10 => write!(f, "x26"),
            RVRegCode::S11 => write!(f, "x27"),
            RVRegCode::T3 => write!(f, "x28"),
            RVRegCode::T4 => write!(f, "x29"),
            RVRegCode::T5 => write!(f, "x30"),
            RVRegCode::T6 => write!(f, "x31"),
        }
    }
}

impl RVRegCode {
    /// Get numeric index (0..=31) for use as array index
    pub fn idx(self) -> usize {
        self as usize
    }
}
