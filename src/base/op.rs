use std::vec::Vec;

use crate::asm::config::Reg;
use crate::base::ir::BBId;
use crate::base::Type;
use crate::utils::arena::*;

pub type OpId = usize;
pub type DFG = IndexedArena<Op>;

#[derive(Debug, Clone)]
pub enum OpData {
    // customized instructions for convenience
    GetGlobal(OpId),
    GetArg(OpId),
    // for immediate values
    Int(i32),
    Float(f32),

    // regular instructions
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
    },
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
    Ret {
        value: Option<OpId>,
    },
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            OpData::Int(val) => write!(f, "{}", val),
            OpData::Float(val) => write!(f, "{}", val),
            OpData::GetGlobal(addr) => write!(f, "get_global {}", addr),
            OpData::GetArg(addr) => write!(f, "get_arg {}", addr),

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
            OpData::Call { args } => {
                write!(
                    f,
                    "call {} {:?}",
                    self.attrs
                        .iter()
                        .find(|attr| matches!(attr, Attr::Symbol(_)))
                        .map(|attr| {
                            if let Attr::Symbol(name) = attr {
                                name.clone()
                            } else {
                                "".to_string()
                            }
                        })
                        .unwrap_or_else(
                            || Err("Call op missing symbol attribute".to_string()).unwrap()
                        ),
                    args
                )
            }
            OpData::Br {
                cond,
                then_bb,
                else_bb,
            } => write!(f, "br {}, {}, {}", cond, then_bb, else_bb),
            OpData::Jump { target_bb } => write!(f, "jump {}", target_bb),
            OpData::Ret { value } => write!(f, "ret {:?}", value),
        }
    }
}

pub struct Op {
    pub prev: Option<OpId>,
    pub typ: Type,
    pub attrs: Vec<Attr>,
    pub data: OpData,
    pub uses: Vec<OpId>,
    pub next: Option<OpId>,
}

impl Op {
    pub fn new(
        typ: Type,
        attrs: Vec<Attr>,
        data: OpData,
    ) -> Self {
        Self {
            prev: None,
            typ,
            attrs,
            data,
            uses: vec![],
            next: None,
        }
    }
}

// attributes of instructions
pub enum Attr {
    PhysReg(Reg),
    Symbol(String),
}

impl std::fmt::Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attr::PhysReg(reg) => write!(f, "<phys_reg = {}>", reg),
            Attr::Symbol(name) => write!(f, "<symbol = {}>", name),
        }
    }
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
