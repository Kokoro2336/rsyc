use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::base::ir::{BBId, OpId};

pub struct LoopInfo {
    pub while_entry: Option<BBId>,
    pub end_block: Option<BBId>,
}

impl LoopInfo {
    pub fn new(while_entry: BBId, end_block: BBId) -> Self {
        LoopInfo {
            while_entry: Some(while_entry),
            end_block: Some(end_block),
        }
    }
}
