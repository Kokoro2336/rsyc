use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::base::ir::{BBId, OpId};

pub struct LoopInfo {
    pub while_entry: Option<BBId>,
    pub end_block: Option<BBId>,
    pub break_inst_list: Vec<OpId>,
    pub continue_inst_list: Vec<OpId>,
}

impl LoopInfo {
    pub fn new() -> Self {
        LoopInfo {
            while_entry: None,
            end_block: None,
            break_inst_list: vec![],
            continue_inst_list: vec![],
        }
    }

    pub fn add_while_entry(&mut self, block_id: BBId) {
        self.while_entry = Some(block_id);
    }

    pub fn add_end_block(&mut self, block_id: BBId) {
        self.end_block = Some(block_id);
    }

    pub fn add_break(&mut self, inst_id: OpId) {
        self.break_inst_list.push(inst_id);
    }

    pub fn add_continue(&mut self, inst_id: OpId) {
        self.continue_inst_list.push(inst_id);
    }
}
