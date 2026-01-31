use crate::base::ir::*;
use crate::base::{LoopInfo, Type};

pub struct Builder {
    pub loop_stack: Vec<LoopInfo>,
    // current basic block
    pub current_block: Option<BBId>,
    // current instruction
    pub current_inst: Option<OpId>,
}

pub struct BuilderContext<'a> {
    pub cfg: Option<&'a mut CFG>,
    pub dfg: Option<&'a mut DFG>,
    pub globals: &'a mut DFG,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            loop_stack: vec![],
            current_block: None,
            current_inst: None,
        }
    }

    pub fn push_loop(&mut self, loop_info: LoopInfo) {
        self.loop_stack.push(loop_info);
    }

    pub fn pop_loop(&mut self) -> Option<LoopInfo> {
        self.loop_stack.pop()
    }

    pub fn current_loop(&mut self) -> Option<&mut LoopInfo> {
        self.loop_stack.last_mut()
    }

    pub fn guard(&mut self) -> BuilderGuard {
        BuilderGuard::new(self)
    }

    pub fn set_current_block(&mut self, block_id: BBId) {
        self.current_block = Some(block_id);
        // update current_inst, set to end of the block by default(Since we don't know whether the block has instructions or not)
        self.current_inst = None;
    }

    // set builder's location before inst
    // current_inst must be an instruction in the current block
    pub fn set_before_inst(
        &mut self,
        ctx: &mut BuilderContext,
        inst_id: Option<OpId>,
    ) -> Result<(), String> {
        let cfg = if ctx.cfg.is_none() {
            return Err("Builder set_before_inst: ctx.cfg is None".to_string());
        } else {
            ctx.cfg.as_mut().unwrap()
        };
        let bb = cfg.get_mut(self.current_block.unwrap())?;
        let bb = if bb.is_none() {
            return Err(format!(
                "Builder set_before_inst: current_block {:?} points to None",
                self.current_block
            ));
        } else {
            bb.unwrap()
        };
        if inst_id.is_none() {
            // set to the end of the block
            self.current_inst = None;
            return Ok(());
        }
        if bb.cur.iter().any(|op_id| *op_id == inst_id.unwrap()) {
            self.current_inst = inst_id;
        } else {
            return Err(format!(
                "Builder set_before_inst: inst {:?} not in current_block {:?}",
                inst_id, self.current_block
            ));
        }
        Ok(())
    }

    pub fn add_uses(&mut self, ctx: &mut BuilderContext, op: OpId) -> Result<(), String> {
        let dfg = if ctx.dfg.is_none() {
            return Err("Builder add_uses: ctx.dfg is None".to_string());
        } else {
            ctx.dfg.as_mut().unwrap()
        };
        let data = dfg.get(op)?;
        let data = if data.is_none() {
            return Err("Builder add_uses: op points to None".to_string());
        } else {
            data.unwrap().data.clone()
        };

        match data {
            OpData::Load { addr } => {
                dfg.add_use(addr, op)?;
            }
            OpData::Store { addr, value } => {
                dfg.add_use(addr, op)?;
                dfg.add_use(value, op)?;
            }
            OpData::Br { cond } => {
                dfg.add_use(cond, op)?;
            }
            OpData::Call { args } => {
                for arg in args {
                    dfg.add_use(arg, op)?;
                }
            }
            OpData::Ret { value } => {
                if let Some(val) = value {
                    dfg.add_use(val, op)?;
                }
            }

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
                dfg.add_use(lhs, op)?;
                dfg.add_use(rhs, op)?;
            }

            OpData::Sitofp { value } | OpData::Fptosi { value } => {
                dfg.add_use(value, op)?;
            }

            // GlobalAlloca: Do not maintain uses for global alloca
            _ => { /* do nothing */ }
        }
        Ok(())
    }

    // create an instruction after current instruction
    pub fn create(&mut self, ctx: &mut BuilderContext, op: Op) -> Result<OpId, String> {
        match op.data {
            OpData::GlobalAlloca => {
                let globals = &mut ctx.globals;
                let op_id = globals.alloc(op)?;
                Ok(op_id)
            },
            _ => {
                let dfg = if ctx.dfg.is_none() {
                    return Err("Builder create: ctx.dfg is None".to_string());
                } else {
                    ctx.dfg.as_mut().unwrap()
                };
                let cfg = if ctx.cfg.is_none() {
                    return Err("Builder create: ctx.cfg is None".to_string());
                } else {
                    ctx.cfg.as_mut().unwrap()
                };

                // append_at will update the prev and next pointers accordingly
                let op_id = dfg.alloc(op)?;
                let bb = cfg.get_mut(self.current_block.unwrap())?;
                let bb = if bb.is_none() {
                    return Err(format!(
                        "Builder create: current_block {:?} points to None",
                        self.current_block
                    ));
                } else {
                    bb.unwrap()
                };

                if let Some(current_inst) = self.current_inst {
                    // insert before current_inst
                    let pos = bb
                        .cur
                        .iter()
                        .position(|&id| id == current_inst)
                        .ok_or_else(|| {
                            format!(
                                "Builder create: current_inst {:?} not found in current_block {:?}",
                                current_inst, self.current_block
                            )
                        })?;
                    bb.cur.insert(pos, op_id);
                } else {
                    // insert at the end
                    bb.cur.push(op_id);
                }
                // We don't update current_inst, so that the next created instruction is still before the same instruction
                Ok(op_id)
            }
        }
    }

    pub fn create_new_block(&mut self, ctx: &mut BuilderContext) -> Result<BBId, String> {
        let cfg = ctx.cfg.as_mut().ok_or("Builder create_new_block: ctx.cfg is None")?;
        let bb_id = cfg.alloc(BasicBlock::new())?;
        // we separate block creation and setting current block
        Ok(bb_id)
    }
}

pub struct BuilderGuard<'a> {
    pub builder: &'a mut Builder,
    pub saved_loop_stack: Vec<LoopInfo>,
    pub saved_block: Option<BBId>,
    pub saved_inst: Option<OpId>,
}

impl<'a> BuilderGuard<'a> {
    pub fn new(builder: &'a mut Builder) -> Self {
        let saved_loop_stack = std::mem::take(&mut builder.loop_stack);
        let saved_block = builder.current_block;
        let saved_inst = builder.current_inst;
        Self {
            builder,
            saved_block,
            saved_loop_stack,
            saved_inst,
        }
    }
}

impl Drop for BuilderGuard<'_> {
    fn drop(&mut self) {
        self.builder.loop_stack = std::mem::take(&mut self.saved_loop_stack);
        self.builder.current_block = self.saved_block;
        self.builder.current_inst = self.saved_inst;
    }
}
