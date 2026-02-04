use crate::base::ir::*;
use crate::base::{BranchInfo, LoopInfo};

pub struct Builder {
    pub loop_stack: Vec<LoopInfo>,
    pub branch_stack: Vec<BranchInfo>,
    // current basic block
    pub current_block: Option<Operand>,
    // current instruction
    pub current_inst: Option<Operand>,
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
            branch_stack: vec![],
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

    pub fn push_branch(&mut self, branch_info: BranchInfo) {
        self.branch_stack.push(branch_info);
    }

    pub fn pop_branch(&mut self) -> Option<BranchInfo> {
        self.branch_stack.pop()
    }

    pub fn current_loop(&mut self) -> Option<&mut LoopInfo> {
        self.loop_stack.last_mut()
    }

    pub fn guard(&mut self) -> BuilderGuard {
        BuilderGuard::new(self)
    }

    pub fn set_current_block(&mut self, block_id: Operand) -> Result<(), String> {
        // Emm... just set current_block, no check.
        self.current_block = Some(block_id);
        // update current_inst, set to end of the block by default(Since we don't know whether the block has instructions or not)
        self.current_inst = None;
        Ok(())
    }

    // set builder's location before inst
    // current_inst must be an instruction in the current block
    pub fn set_before_inst(
        &mut self,
        ctx: &mut BuilderContext,
        inst_id: Option<Operand>,
    ) -> Result<(), String> {
        let cfg = if ctx.cfg.is_none() {
            return Err("Builder set_before_inst: ctx.cfg is None".to_string());
        } else {
            ctx.cfg.as_mut().unwrap()
        };
        if self.current_block.is_none() {
            return Err("Builder set_before_inst: current_block is None".to_string());
        };

        let current_block = self.current_block.clone().unwrap().get_bb_id()?;
        let bb = cfg.get_mut(current_block)?;
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
        if bb.cur.iter().any(|op_id| {
            if let Some(inst_id) = &inst_id {
                let op_id = match op_id.get_op_id() {
                    Ok(id) => id,
                    Err(_) => return false,
                };
                let inst_id = match inst_id.get_op_id() {
                    Ok(id) => id,
                    Err(_) => return false,
                };
                op_id == inst_id
            } else {
                false
            }
        }) {
            self.current_inst = inst_id;
        } else {
            return Err(format!(
                "Builder set_before_inst: inst {:?} not in current_block {:?}",
                inst_id, self.current_block
            ));
        }
        Ok(())
    }

    pub fn add_uses(&mut self, ctx: &mut BuilderContext, op: Operand) -> Result<(), String> {
        let dfg = if ctx.dfg.is_none() {
            return Err("Builder add_uses: ctx.dfg is None".to_string());
        } else {
            ctx.dfg.as_mut().unwrap()
        };
        let data = dfg.get(op.get_bb_id()?)?;
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
                dfg.add_use(addr, op.clone())?;
                dfg.add_use(value, op)?;
            }
            OpData::Br { cond, .. } => {
                dfg.add_use(cond, op)?;
            }
            OpData::Call { args, .. } => {
                for arg in args {
                    dfg.add_use(arg, op.clone())?;
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
                dfg.add_use(lhs, op.clone())?;
                dfg.add_use(rhs, op)?;
            }

            OpData::Sitofp { value } | OpData::Fptosi { value } => {
                dfg.add_use(value, op)?;
            }

            OpData::GEP { base, indices } => {
                dfg.add_use(base, op.clone())?;
                for index in indices {
                    dfg.add_use(index, op.clone())?;
                }
            }

            OpData::Move { value, .. } => {
                dfg.add_use(value, op)?;
            }

            // GlobalAlloca: Do not maintain uses for global alloca
            OpData::GlobalAlloca(_)
            | OpData::GetArg(_)
            | OpData::Int(_)
            | OpData::Float(_)
            // ?
            | OpData::Alloca(_)
            | OpData::Jump {..} => { /* do nothing */ }
        }
        Ok(())
    }

    // create an instruction after current instruction
    pub fn create(&mut self, ctx: &mut BuilderContext, op: Op) -> Result<Operand, String> {
        match op.data {
            OpData::GlobalAlloca(_) => {
                let globals = &mut ctx.globals;
                let op_id = globals.alloc(op)?;
                Ok(Operand::Op(op_id))
            }
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
                let current_block = if self.current_block.is_none() {
                    return Err("Builder create: current_block is None".to_string());
                } else {
                    self.current_block.as_ref().unwrap().get_bb_id()?
                };
                let bb = cfg.get_mut(current_block)?;
                let bb = if bb.is_none() {
                    return Err(format!(
                        "Builder create: current_block {:?} points to None",
                        self.current_block
                    ));
                } else {
                    bb.unwrap()
                };

                if let Some(current_inst) = &self.current_inst {
                    // insert before current_inst
                    let pos = bb
                        .cur
                        .iter()
                        .position(|id| {
                            let id = match id.get_op_id() {
                                Ok(id) => id,
                                Err(_) => return false,
                            };
                            let inst_id = match current_inst.get_op_id() {
                                Ok(id) => id,
                                Err(_) => return false,
                            };
                            id == inst_id
                        })
                        .ok_or_else(|| {
                            format!(
                                "Builder create: current_inst {:?} not found in current_block {:?}",
                                current_inst, self.current_block
                            )
                        })?;
                    bb.cur.insert(pos, Operand::Op(op_id));
                } else {
                    // insert at the end
                    bb.cur.push(Operand::Op(op_id));
                }
                // We don't update current_inst, so that the next created instruction is still before the same instruction
                Ok(Operand::Op(op_id))
            }
        }
    }

    pub fn create_new_block(&mut self, ctx: &mut BuilderContext) -> Result<Operand, String> {
        let cfg = ctx
            .cfg
            .as_mut()
            .ok_or("Builder create_new_block: ctx.cfg is None")?;
        let bb_id = cfg.alloc(BasicBlock::new())?;
        // we separate block creation and setting current block
        Ok(Operand::BB(bb_id))
    }
}

pub struct BuilderGuard<'a> {
    pub builder: &'a mut Builder,
    pub saved_loop_stack: Vec<LoopInfo>,
    pub saved_block: Option<Operand>,
    pub saved_inst: Option<Operand>,
}

impl<'a> BuilderGuard<'a> {
    pub fn new(builder: &'a mut Builder) -> Self {
        let saved_loop_stack = std::mem::take(&mut builder.loop_stack);
        let saved_block = builder.current_block.clone();
        let saved_inst = builder.current_inst.clone();
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
        self.builder.current_block = std::mem::take(&mut self.saved_block);
        self.builder.current_inst = std::mem::take(&mut self.saved_inst);
    }
}
