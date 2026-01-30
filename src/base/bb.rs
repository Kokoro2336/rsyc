use crate::base::ir::{OpId, DFG};
use crate::utils::arena::*;

pub type BBId = usize;
pub type GlobalId = usize;
pub type CFG = IndexedArena<BasicBlock>;

pub struct Program {
    // global vars
    pub globals: DFG,
    // global funcs
    pub funcs: Vec<Function>,
}

impl Program {
    pub fn new() -> Self {
        Self { globals: DFG::new(), funcs: vec![] }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Function {
    pub cfg: CFG,
    pub dfg: DFG,
}

impl Function {
    pub fn new() -> Self {
        Self {
            cfg: IndexedArena::new(),
            dfg: DFG::new(),
        }
    }
}

pub struct BasicBlock {
    pub preds: Vec<BBId>,
    pub cur: Vec<OpId>,
    pub succs: Vec<BBId>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            preds: vec![],
            cur: vec![],
            succs: vec![],
        }
    }
}

// impl cfg
impl Arena<BasicBlock> for IndexedArena<BasicBlock> {
    fn remove(&mut self, idx: BBId) -> Result<BBId, String> {
        // TODO
        unimplemented!()
    }

    fn gc(&mut self) -> Result<Vec<ArenaItem<BasicBlock>>, String> {
        let new_arena: Vec<ArenaItem<BasicBlock>> = vec![];
        let old_arena = std::mem::replace(&mut self.storage, new_arena);
        if self.head.is_none() {
            return Err("CFG gc: head is none".to_string());
        }

        // Transport
        // CFG is a complex graph, reorder the layout seems impossible, so we just iterate the original storage directly.
        let old_arena = old_arena
            .into_iter()
            .map(|mut item| {
                if matches!(item, ArenaItem::Data(_)) {
                    let new_idx = self.storage.len();
                    let data = item.replace(new_idx);
                    self.storage.push(data);
                    ArenaItem::NewIndex(new_idx)
                } else {
                    ArenaItem::None
                }
            })
            .collect::<Vec<ArenaItem<BasicBlock>>>();

        // rewrite idx
        for item in self.storage.iter_mut() {
            // item can't be any other variant than Data here
            if let ArenaItem::Data(node) = item {
                for pred in node.preds.iter_mut() {
                    *pred = match old_arena.get(*pred).unwrap() {
                        ArenaItem::NewIndex(new_idx) => *new_idx,
                        _ => {
                            return Err("CFG gc: predecessor index not found".to_string());
                        }
                    };
                }
                // rewrite data.cur needs the old arena of DFG, we'll do it in Compaction pass
                for succ in node.succs.iter_mut() {
                    *succ = match old_arena.get(*succ).unwrap() {
                        ArenaItem::NewIndex(new_idx) => *new_idx,
                        _ => {
                            return Err("CFG gc: successor index not found".to_string());
                        }
                    };
                }
            }
        }

        // replace old storage
        Ok(old_arena)
    }
}
