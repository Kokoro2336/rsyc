use crate::base::ir::Operand;

pub struct BranchInfo {
    pub then_block: Option<Operand>,
    pub else_block: Option<Operand>,
    pub end_block: Option<Operand>,
}

pub struct LoopInfo {
    pub while_entry: Option<Operand>,
    pub end_block: Option<Operand>,
}

impl LoopInfo {
    pub fn new(while_entry: Operand, end_block: Operand) -> Self {
        LoopInfo {
            while_entry: Some(while_entry),
            end_block: Some(end_block),
        }
    }
}
