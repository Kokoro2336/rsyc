use std::vec::Vec;

use crate::asm::config::Reg;
use crate::base::ir::{BBId, GlobalId};
use crate::base::Type;
use crate::frontend::ast::Literal;
use crate::utils::arena::*;

pub type OpId = usize;
pub type ParamId = u32;
pub type DFG = IndexedArena<Op>;

#[derive(Debug, Clone)]
pub enum OpData {
    // customized instructions for convenience
    GlobalAlloca,
    GetArg,
    // getelementptr
    GEP {
        base: OpId,
        indices: Vec<OpId>,
    },
    // for immediate values
    Int,
    Float,

    /* regular instructions */
    /// Integer
    AddI {
        lhs: OpId,
        rhs: OpId,
    },
    SubI {
        lhs: OpId,
        rhs: OpId,
    },
    MulI {
        lhs: OpId,
        rhs: OpId,
    },
    DivI {
        lhs: OpId,
        rhs: OpId,
    },
    ModI {
        lhs: OpId,
        rhs: OpId,
    },

    // The comparisons are logical.
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
    },

    // Comparison(S: Signed. And SysY only has signed comparison)
    SNe {
        lhs: OpId,
        rhs: OpId,
    },
    SEq {
        lhs: OpId,
        rhs: OpId,
    },
    SGt {
        lhs: OpId,
        rhs: OpId,
    },
    SLt {
        lhs: OpId,
        rhs: OpId,
    },
    SGe {
        lhs: OpId,
        rhs: OpId,
    },
    SLe {
        lhs: OpId,
        rhs: OpId,
    },

    // Bitwise shift
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
    },

    /// Float
    AddF {
        lhs: OpId,
        rhs: OpId,
    },
    SubF {
        lhs: OpId,
        rhs: OpId,
    },
    MulF {
        lhs: OpId,
        rhs: OpId,
    },
    DivF {
        lhs: OpId,
        rhs: OpId,
    },
    // Mod is invalid for float in SysY

    // On the language level, SysY doesn't support And, Or, Xor for float

    // Comparison. SysY doesn't support NaN, so we only have one type of comparison here.
    ONe {
        lhs: OpId,
        rhs: OpId,
    },
    OEq {
        lhs: OpId,
        rhs: OpId,
    },
    OGt {
        lhs: OpId,
        rhs: OpId,
    },
    OLt {
        lhs: OpId,
        rhs: OpId,
    },
    OGe {
        lhs: OpId,
        rhs: OpId,
    },
    OLe {
        lhs: OpId,
        rhs: OpId,
    },

    /// Cast operations
    Sitofp {
        value: OpId,
    }, // int to float
    Fptosi {
        value: OpId,
    }, // float to int

    // SysY doesn't support bitwise shift for float
    /// Memory operations
    Store {
        addr: OpId,
        value: OpId,
    },
    Load {
        addr: OpId,
    },
    Alloca,

    /// Control flow
    Call {
        args: Vec<OpId>,
    },
    Br {
        cond: OpId,
    },
    Jump,
    Ret {
        value: Option<OpId>,
    },
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
            OpData::GEP { base, indices } => {
                write!(f, "gep {}, [", base)?;
                for (i, index) in indices.iter().enumerate() {
                    write!(f, "{}", index)?;
                    if i != indices.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
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

            OpData::AddI { lhs, rhs } => write!(f, "add {}, {}", lhs, rhs),
            OpData::SubI { lhs, rhs } => write!(f, "sub {}, {}", lhs, rhs),
            OpData::MulI { lhs, rhs } => write!(f, "mul {}, {}", lhs, rhs),
            OpData::DivI { lhs, rhs } => write!(f, "div {}, {}", lhs, rhs),
            OpData::ModI { lhs, rhs } => write!(f, "mod {}, {}", lhs, rhs),

            OpData::And { lhs, rhs } => write!(f, "and {}, {}", lhs, rhs),
            OpData::Or { lhs, rhs } => write!(f, "or {}, {}", lhs, rhs),
            OpData::Xor { lhs, rhs } => write!(f, "xor {}, {}", lhs, rhs),

            OpData::SNe { lhs, rhs } => write!(f, "sne {}, {}", lhs, rhs),
            OpData::SEq { lhs, rhs } => write!(f, "seq {}, {}", lhs, rhs),
            OpData::SGt { lhs, rhs } => write!(f, "sgt {}, {}", lhs, rhs),
            OpData::SLt { lhs, rhs } => write!(f, "slt {}, {}", lhs, rhs),
            OpData::SGe { lhs, rhs } => write!(f, "sge {}, {}", lhs, rhs),
            OpData::SLe { lhs, rhs } => write!(f, "sle {}, {}", lhs, rhs),

            OpData::Shl { lhs, rhs } => write!(f, "shl {}, {}", lhs, rhs),
            OpData::Shr { lhs, rhs } => write!(f, "shr {}, {}", lhs, rhs),
            OpData::Sar { lhs, rhs } => write!(f, "sar {}, {}", lhs, rhs),

            OpData::AddF { lhs, rhs } => write!(f, "addf {}, {}", lhs, rhs),
            OpData::SubF { lhs, rhs } => write!(f, "subf {}, {}", lhs, rhs),
            OpData::MulF { lhs, rhs } => write!(f, "mulf {}, {}", lhs, rhs),
            OpData::DivF { lhs, rhs } => write!(f, "divf {}, {}", lhs, rhs),

            OpData::ONe { lhs, rhs } => write!(f, "one {}, {}", lhs, rhs),
            OpData::OEq { lhs, rhs } => write!(f, "oeq {}, {}", lhs, rhs),
            OpData::OGt { lhs, rhs } => write!(f, "ogt {}, {}", lhs, rhs),
            OpData::OLt { lhs, rhs } => write!(f, "olt {}, {}", lhs, rhs),
            OpData::OGe { lhs, rhs } => write!(f, "oge {}, {}", lhs, rhs),
            OpData::OLe { lhs, rhs } => write!(f, "ole {}, {}", lhs, rhs),

            OpData::Sitofp { value } => write!(f, "sitofp {}", value),
            OpData::Fptosi { value } => write!(f, "fptosi {}", value),

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
    pub typ: Type,
    pub attrs: Vec<Attr>,
    pub data: OpData,
    pub uses: Vec<OpId>,
}

impl Op {
    pub fn new(typ: Type, attrs: Vec<Attr>, data: OpData) -> Self {
        Self {
            typ,
            attrs,
            data,
            uses: vec![],
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
    // for global var
    GlobalArray {
        // if mutable -> .data; else .rodata
        name: String,
        mutable: bool,
        typ: Type,
        values: Vec<Literal>,
    },
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
            Attr::GlobalArray {
                name,
                mutable: _,
                typ: _,
                values: _,
            } => write!(f, "<global array: {}>", name),
        }
    }
}

// impl dfg
impl Arena<Op> for IndexedArena<Op> {
    fn remove(&mut self, idx: OpId) -> Result<OpId, String> {
        // mark this slot as deleted
        self.storage[idx] = ArenaItem::None;
        Ok(idx)
    }

    fn gc(&mut self) -> Result<Vec<ArenaItem<Op>>, String> {
        let mut new_arena: Vec<ArenaItem<Op>> = vec![];

        // Transport
        for item in self.storage.iter_mut() {
            // check if the slot is occupied by data
            if matches!(item, ArenaItem::Data(_)) {
                let new_idx = new_arena.len();
                let data = item.replace(new_idx);
                new_arena.push(data);
            }
        }

        // rewrite idx
        for item in new_arena.iter_mut() {
            // item can't be any other variant than Data here
            if let ArenaItem::Data(node) = item {
                for use_idx in node.uses.iter_mut() {
                    *use_idx = match self.storage[*use_idx] {
                        ArenaItem::NewIndex(new_idx) => new_idx,
                        _ => *use_idx,
                    };
                }

                // rewrite operands
                match &mut node.data {
                    OpData::AddI { lhs, rhs }
                    | OpData::SubI { lhs, rhs }
                    | OpData::MulI { lhs, rhs }
                    | OpData::DivI { lhs, rhs }
                    | OpData::ModI { lhs, rhs }
                    | OpData::SNe { lhs, rhs }
                    | OpData::SEq { lhs, rhs }
                    | OpData::SGt { lhs, rhs }
                    | OpData::SLt { lhs, rhs }
                    | OpData::SGe { lhs, rhs }
                    | OpData::SLe { lhs, rhs }
                    | OpData::And { lhs, rhs }
                    | OpData::Or { lhs, rhs }
                    | OpData::Xor { lhs, rhs }
                    | OpData::Shl { lhs, rhs }
                    | OpData::Shr { lhs, rhs }
                    | OpData::Sar { lhs, rhs }
                    | OpData::AddF { lhs, rhs }
                    | OpData::SubF { lhs, rhs }
                    | OpData::MulF { lhs, rhs }
                    | OpData::DivF { lhs, rhs }
                    | OpData::ONe { lhs, rhs }
                    | OpData::OEq { lhs, rhs }
                    | OpData::OGt { lhs, rhs }
                    | OpData::OLt { lhs, rhs }
                    | OpData::OGe { lhs, rhs }
                    | OpData::OLe { lhs, rhs } => {
                        *lhs = match self.storage[*lhs] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *lhs,
                        };
                        *rhs = match self.storage[*rhs] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *rhs,
                        };
                    }

                    OpData::Sitofp { value } | OpData::Fptosi { value } => {
                        *value = match self.storage[*value] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *value,
                        };
                    }
                    OpData::Store { addr, value } => {
                        *addr = match self.storage[*addr] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *addr,
                        };
                        *value = match self.storage[*value] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *value,
                        };
                    }
                    OpData::Load { addr } => {
                        *addr = match self.storage[*addr] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *addr,
                        };
                    }
                    OpData::Call { args } => {
                        for arg in args.iter_mut() {
                            *arg = match self.storage[*arg] {
                                ArenaItem::NewIndex(new_idx) => new_idx,
                                _ => *arg,
                            };
                        }
                    }
                    OpData::Br { cond } => {
                        *cond = match self.storage[*cond] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *cond,
                        };
                    }
                    OpData::Ret { value } => {
                        if let Some(val) = value {
                            *val = match self.storage[*val] {
                                ArenaItem::NewIndex(new_idx) => new_idx,
                                _ => *val,
                            };
                        }
                    }

                    OpData::GEP { base, indices } => {
                        *base = match self.storage[*base] {
                            ArenaItem::NewIndex(new_idx) => new_idx,
                            _ => *base,
                        };
                        for index in indices.iter_mut() {
                            *index = match self.storage[*index] {
                                ArenaItem::NewIndex(new_idx) => new_idx,
                                _ => *index,
                            };
                        }
                    }

                    // Get global should be processed outside of gc()
                    // TODO: As long as program global arena is not changed, the indices are stable.
                    OpData::GlobalAlloca
                    | OpData::GetArg
                    | OpData::Int
                    | OpData::Float
                    | OpData::Alloca
                    | OpData::Jump => { /* no operands to rewrite */ }
                }
            }
        }

        // replace old storage
        Ok(std::mem::replace(&mut self.storage, new_arena))
    }
}

impl IndexedArena<Op> {
    pub fn add_use(&mut self, op_idx: OpId, use_idx: OpId) -> Result<(), String> {
        if let Some(node) = self.get_mut(op_idx)? {
            node.uses.push(use_idx);
            Ok(())
        } else {
            Err("DFG add_use: op index not found".to_string())
        }
    }
}
