use std::vec::Vec;

use crate::asm::config::Reg;
use crate::base::ir::{BBId, GlobalId};
use crate::base::Type;
use crate::utils::arena::*;

pub type OpId = usize;
pub type ParamId = u32;
pub type DFG = IndexedArena<Op>;

#[derive(Debug, Clone)]
pub enum OpData {
    // customized instructions for convenience
    GetGlobal(GlobalId),
    GlobalAlloca,
    GetArg,
    // for immediate values
    Int,
    Float,

    // regular instructions
    Ne { lhs: OpId, rhs: OpId },
    Eq { lhs: OpId, rhs: OpId },
    Gt { lhs: OpId, rhs: OpId },
    Lt { lhs: OpId, rhs: OpId },
    Ge { lhs: OpId, rhs: OpId },
    Le { lhs: OpId, rhs: OpId },
    Add { lhs: OpId, rhs: OpId },
    Sub { lhs: OpId, rhs: OpId },
    Mul { lhs: OpId, rhs: OpId },
    Div { lhs: OpId, rhs: OpId },
    Mod { lhs: OpId, rhs: OpId },
    And { lhs: OpId, rhs: OpId },
    Or { lhs: OpId, rhs: OpId },
    Xor { lhs: OpId, rhs: OpId }, // bitwise
    Shl { lhs: OpId, rhs: OpId },
    Shr { lhs: OpId, rhs: OpId },
    Sar { lhs: OpId, rhs: OpId }, // bitwise shift
    Store { addr: OpId, value: OpId },
    Load { addr: OpId },
    Alloca,
    Call { args: Vec<OpId> },
    Br { cond: OpId },
    Jump,
    Ret { value: Option<OpId> },
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            OpData::Int => write!(
                f,
                "{}",
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Int(val) = attr {
                            Some(val)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&0)
            ),
            OpData::Float => write!(
                f,
                "{}",
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Float(val) = attr {
                            Some(val)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&0.0)
            ),
            OpData::GetGlobal(addr) => write!(f, "get_global {}", addr),
            OpData::GetArg => write!(
                f,
                "get_arg {}",
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Param(idx) = attr {
                            Some(idx)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&0)
            ),
            OpData::GlobalAlloca => write!(
                f,
                "global_alloc {}",
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Symbol(name) = attr {
                            Some(name)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&"".to_string())
            ),

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
            OpData::Alloca => write!(
                f,
                "alloc {}",
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Size(size) = attr {
                            Some(size)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&0)
            ),
            OpData::Call { args } => {
                write!(
                    f,
                    "call {} {:?}",
                    self.attrs
                        .iter()
                        .find(|attr| matches!(attr, Attr::Function(_)))
                        .map(|attr| {
                            if let Attr::Function(name) = attr {
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
            OpData::Br { cond } => write!(
                f,
                "br {}, {}, {}",
                cond,
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Branch { then_bb, .. } = attr {
                            Some(then_bb)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&0),
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Branch {
                            else_bb: Some(else_bb),
                            ..
                        } = attr
                        {
                            Some(else_bb)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&0)
            ),
            OpData::Jump => write!(
                f,
                "jump {}",
                self.attrs
                    .iter()
                    .find_map(|attr| {
                        if let Attr::Jump(target_bb) = attr {
                            Some(target_bb)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(&0)
            ),
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
    pub fn new(typ: Type, attrs: Vec<Attr>, data: OpData) -> Self {
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
    // for pysical register allocation
    PhysReg(Reg),
    // for call
    Function(String),
    // for Branch
    Branch {
        then_bb: BBId,
        else_bb: Option<BBId>,
    },
    Symbol(String),
    // for Jump
    Jump(BBId),
    // for GetArg
    Param(u32),
    // for Alloca
    Size(u32),
    // for int
    Int(i32),
    // for float
    Float(f32),
}

impl std::fmt::Display for Attr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attr::PhysReg(reg) => write!(f, "<phys_reg = {}>", reg),
            Attr::Function(name) => write!(f, "<function = {}>", name),
            Attr::Branch { then_bb, else_bb } => {
                if let Some(else_bb) = else_bb {
                    write!(f, "<branch then: {}, else: {}>", then_bb, else_bb)
                } else {
                    write!(f, "<branch then: {}>", then_bb)
                }
            }
            Attr::Symbol(name) => write!(f, "<symbol = {}>", name),
            Attr::Jump(target_bb) => write!(f, "<jump target: {}>", target_bb),
            Attr::Param(idx) => write!(f, "<param idx: {}>", idx),
            Attr::Size(size) => write!(f, "<alloca size: {}>", size),
            Attr::Int(val) => write!(f, "<int: {}>", val),
            Attr::Float(val) => write!(f, "<float: {}>", val),
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
    // insert at the front of idx. If idx is None, insert at the end.
    pub fn insert_at(&mut self, idx: Option<OpId>, mut data: Op) -> Result<OpId, String> {
        if let Some(id) = idx {
            let mut prev_idx = None;
            if let Some(node) = self.get(id)? {
                prev_idx = node.prev;
            }
            // update data's links
            data.prev = prev_idx;
            data.next = Some(id);

            let index = self.alloc(data)?;

            // update the surrounding nodes
            if let Some(prev) = prev_idx {
                if let Some(node) = self.get_mut(prev)? {
                    node.next = Some(index);
                }
            } else {
                self.head = Some(index);
            }

            if let Some(node) = self.get_mut(id)? {
                node.prev = Some(index);
            }

            Ok(index)
        } else {
            let mut tail_idx = None;
            let mut curr = self.head;
            while let Some(c) = curr {
                tail_idx = Some(c);
                if let Some(node) = self.get(c)? {
                    curr = node.next;
                } else {
                    break;
                }
            }

            data.prev = tail_idx;
            data.next = None;

            let index = self.alloc(data)?;

            if let Some(tail) = tail_idx {
                if let Some(node) = self.get_mut(tail)? {
                    node.next = Some(index);
                }
            } else {
                self.head = Some(index);
            }

            Ok(index)
        }
    }

    pub fn add_use(&mut self, op_idx: OpId, use_idx: OpId) -> Result<(), String> {
        if let Some(node) = self.get_mut(op_idx)? {
            node.uses.push(use_idx);
            Ok(())
        } else {
            Err("DFG add_use: op index not found".to_string())
        }
    }
}
