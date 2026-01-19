use std::cell::RefCell;
use std::vec::Vec;

use crate::base::ir::BBId;
use crate::log::error;
use crate::utils::arena::*;

pub type OpId = usize;
pub type DFG = IndexedArena<Op>;

#[derive(Debug, Clone)]
pub enum OpData {
    Ne {
        lhs: OpId,
        rhs: OpId,
    },
    Eq {
        lhs: OpId,
        rhs: OpId,
    },
    Gt {
        lhs: OpId,
        rhs: OpId,
    },
    Lt {
        lhs: OpId,
        rhs: OpId,
    },
    Ge {
        lhs: OpId,
        rhs: OpId,
    },
    Le {
        lhs: OpId,
        rhs: OpId,
    },
    Add {
        lhs: OpId,
        rhs: OpId,
    },
    Sub {
        lhs: OpId,
        rhs: OpId,
    },
    Mul {
        lhs: OpId,
        rhs: OpId,
    },
    Div {
        lhs: OpId,
        rhs: OpId,
    },
    Mod {
        lhs: OpId,
        rhs: OpId,
    }, // arithmetic
    And {
        lhs: OpId,
        rhs: OpId,
    },
    Or {
        lhs: OpId,
        rhs: OpId,
    },
    Xor {
        lhs: OpId,
        rhs: OpId,
    }, // bitwise
    Shl {
        lhs: OpId,
        rhs: OpId,
    },
    Shr {
        lhs: OpId,
        rhs: OpId,
    },
    Sar {
        lhs: OpId,
        rhs: OpId,
    }, // bitwise shift
    Store {
        addr: OpId,
        value: OpId,
    },
    Load {
        addr: OpId,
    },
    Alloca {
        size: usize,
    },
    Call {
        func: String,
        args: Vec<OpId>,
    },
    Br {
        cond: OpId,
        then_bb: BBId,
        else_bb: BBId,
    },
    Jump {
        target_bb: BBId,
    },
    GetPtr {
        base: OpId,
        offset: OpId,
    },
    GetElemPtr {
        base: OpId,
        index: OpId,
    },
    Ret {
        value: Option<OpId>,
    },
}

impl std::fmt::Display for OpData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpData::Ne { lhs, rhs } => write!(f, "ne {}, {}", lhs, rhs),
            OpData::Eq { lhs, rhs } => write!(f, "eq {}, {}", lhs, rhs),
            OpData::Gt { lhs, rhs } => write!(f, "gt {}, {}", lhs, rhs),
            OpData::Lt { lhs, rhs } => write!(f, "lt {}, {}", lhs, rhs),
            OpData::Ge { lhs, rhs } => write!(f, "ge {}, {}", lhs, rhs),
            OpData::Le { lhs, rhs } => write!(f, "le {}, {}", lhs, rhs),
            OpData::Add { lhs, rhs } => write!(f, "add {}, {}", lhs, rhs),
            OpData::Sub { lhs, rhs } => write!(f, "sub {}, {}", lhs, rhs),
            OpData::Mul { lhs, rhs } => write!(f, "mul {}, {}", lhs, rhs),
            OpData::Div { lhs, rhs } => write!(f, "div {}, {}", lhs, rhs),
            OpData::Mod { lhs, rhs } => write!(f, "mod {}, {}", lhs, rhs),
            OpData::And { lhs, rhs } => write!(f, "and {}, {}", lhs, rhs),
            OpData::Or { lhs, rhs } => write!(f, "or {}, {}", lhs, rhs),
            OpData::Xor { lhs, rhs } => write!(f, "xor {}, {}", lhs, rhs),
            OpData::Shl { lhs, rhs } => write!(f, "shl {}, {}", lhs, rhs),
            OpData::Shr { lhs, rhs } => write!(f, "shr {}, {}", lhs, rhs),
            OpData::Sar { lhs, rhs } => write!(f, "sar {}, {}", lhs, rhs),
            OpData::Store { addr, value } => write!(f, "store {}, {}", addr, value),
            OpData::Load { addr } => write!(f, "load {}", addr),
            OpData::Alloca { size } => write!(f, "alloc {}", size),
            OpData::Call { func, args } => write!(f, "call {} {:?}", func, args),
            OpData::Br {
                cond,
                then_bb,
                else_bb,
            } => write!(f, "br {}, {}, {}", cond, then_bb, else_bb),
            OpData::Jump { target_bb } => write!(f, "jump {}", target_bb),
            OpData::GetPtr { base, offset } => write!(f, "getptr {}, {}", base, offset),
            OpData::GetElemPtr { base, index } => write!(f, "getelemptr {}, {}", base, index),
            OpData::Ret { value } => write!(f, "ret {:?}", value),
        }
    }
}

pub struct Op {
    pub prev: Option<OpId>,
    pub data: OpData,
    pub uses: Vec<OpId>,
    pub next: Option<OpId>,
}

// impl dfg
impl Arena<Op> for IndexedArena<Op> {
    fn remove(&mut self, idx: OpId) -> Result<OpId, String> {
        let node = self.get(idx)?;
        let (prev, next) = if let Some(node) = node {
            (node.prev, node.next)
        } else {
            return Err("IndexedArena remove: index not found".to_string());
        };

        // update the surrounding nodes
        if let Some(prev) = prev {
            if let Some(prev_node) = self.get_mut(prev)? {
                prev_node.next = next;
            }
        } else {
            self.head = next;
        }

        if let Some(next) = next {
            if let Some(next_node) = self.get_mut(next)? {
                next_node.prev = prev;
            }
        }

        // mark this slot as deleted
        self.storage[idx] = ArenaItem::None;
        Ok(idx)
    }

    fn gc(&mut self) -> Result<Vec<ArenaItem<Op>>, String> {
        let mut new_arena: Vec<ArenaItem<Op>> = vec![];
        let mut at = self.head;
        if at.is_none() {
            return Ok(vec![]);
        }

        // Transport
        // DFG is actually a linked list, we can reorder the nodes' layout by the way of transporting.
        while let Some(idx) = at {
            // check if the slot is occupied by data
            if matches!(self.storage.get(idx), Some(ArenaItem::Data(_))) {
                let new_idx = new_arena.len();
                let data = self.storage[idx].replace(new_idx);
                let next = match data {
                    ArenaItem::Data(ref node) => node.next,
                    _ => None,
                };
                new_arena.push(data);
                at = next;
            }
        }

        // rewrite idx
        for item in new_arena.iter_mut() {
            // item can't be any other variant than Data here
            if let ArenaItem::Data(node) = item {
                node.prev = match node.prev {
                    Some(old_idx) => match self.storage[old_idx] {
                        ArenaItem::NewIndex(new_idx) => Some(new_idx),
                        _ => None,
                    },
                    None => None,
                };
                // TODO: rewrite uses.
                node.next = match node.next {
                    Some(old_idx) => match self.storage[old_idx] {
                        ArenaItem::NewIndex(new_idx) => Some(new_idx),
                        _ => None,
                    },
                    None => None,
                };
            }
        }

        // replace old storage
        Ok(std::mem::replace(&mut self.storage, new_arena))
    }
}

impl IndexedArena<Op> {
    pub fn insert_at(&mut self, idx: OpId, mut data: Op) -> Result<OpId, String> {
        let mut prev_idx = None;
        if let Some(node) = self.get(idx)? {
            prev_idx = node.prev;
        }
        // update data's links
        data.prev = prev_idx;
        data.next = Some(idx);

        let index = self.alloc(data)?;

        // update the surrounding nodes
        if let Some(prev) = prev_idx {
            if let Some(node) = self.get_mut(prev)? {
                node.next = Some(index);
            }
        } else {
            self.head = Some(index);
        }

        if let Some(node) = self.get_mut(idx)? {
            node.prev = Some(index);
        }

        Ok(index)
    }
}
