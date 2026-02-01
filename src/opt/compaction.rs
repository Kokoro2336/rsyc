use crate::base::ir::{Attr, Program};
use crate::base::Pass;
use crate::utils::arena::{Arena, ArenaItem};

pub struct Compaction<'a> {
    program: &'a mut Program,
}

impl<'a> Compaction<'a> {
    pub fn new(program: &'a mut Program) -> Self {
        Self { program }
    }
}

impl<'a> Pass<()> for Compaction<'a> {
    fn run(&mut self) -> Result<(), String> {
        self.program.funcs.iter_mut().try_for_each(|func| {
            let old_arena_dfg = func.dfg.gc()?;
            let old_arena_cfg = func.cfg.gc()?;
            // TODO: We don't need to clean globals for now.

            // rewrite op refs in BasicBlocks
            func.cfg.storage.iter_mut().try_for_each(|item| {
                if let ArenaItem::Data(bb) = item {
                    for op_idx in bb.cur.iter_mut() {
                        *op_idx = match old_arena_dfg.get(*op_idx).unwrap() {
                            ArenaItem::NewIndex(new_idx) => *new_idx,
                            _ => {
                                return Err("Compaction gc: op index in BB not found".to_string());
                            }
                        };
                    }
                }
                Ok(())
            })?;

            // rewrite bb refs in DFG ops
            func.dfg.storage.iter_mut().try_for_each(|item| {
                if let ArenaItem::Data(op) = item {
                    // rewrite .attrs
                    for attr in op.attrs.iter_mut() {
                        match attr {
                            Attr::Jump(target_bb) => {
                                *target_bb = match old_arena_cfg.get(*target_bb).unwrap() {
                                    ArenaItem::NewIndex(new_idx) => *new_idx,
                                    _ => {
                                        return Err("Compaction gc: bb index in op attr not found"
                                            .to_string());
                                    }
                                };
                            }

                            Attr::Branch { then_bb, else_bb } => {
                                *then_bb = match old_arena_cfg.get(*then_bb).unwrap() {
                                    ArenaItem::NewIndex(new_idx) => *new_idx,
                                    _ => {
                                        return Err(
                                            "Compaction gc: then bb index in op attr not found"
                                                .to_string(),
                                        );
                                    }
                                };
                                if let Some(else_bb) = else_bb {
                                    *else_bb = match old_arena_cfg.get(*else_bb).unwrap() {
                                        ArenaItem::NewIndex(new_idx) => *new_idx,
                                        _ => {
                                            return Err(
                                                "Compaction gc: else bb index in op attr not found"
                                                    .to_string(),
                                            );
                                        }
                                    };
                                }
                            }

                            // List all the cases. Don't use _. Or you might miss new added Attr variants.
                            Attr::Float(_)
                            | Attr::Int(_)
                            | Attr::Size(_)
                            | Attr::Symbol(_)
                            | Attr::Param(_)
                            | Attr::PhysReg(_)
                            | Attr::Function(_)
                            | Attr::GlobalArray {..} => {}
                        }
                    }
                }
                Ok(())
            })?;

            Ok(())
        })
    }
}
