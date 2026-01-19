use crate::base::ir::Program;
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

impl<'a> Pass for Compaction<'a> {
    fn run(&mut self) -> Result<(), String> {
        self.program.funcs.iter_mut().try_for_each(|func| {
            let old_arena_dfg = func.dfg.gc()?;
            let _ = func.cfg.gc()?;

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
            })
        })
    }
}
