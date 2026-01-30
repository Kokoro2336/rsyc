use crate::base::ir::*;
use crate::base::{LoopInfo, Type};

pub struct Builder {
    pub loop_stack: Vec<LoopInfo>,
    // current basic block
    pub current_block: Option<BBId>,
    // current instruction
    pub current_inst: Option<OpId>,
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
    }

    // set builder's location before inst
    pub fn set_before_inst(&mut self, inst_id: OpId) {
        self.current_inst = Some(inst_id);
    }

    pub fn add_uses(&mut self, dfg: &mut DFG, op: OpId) -> Result<(), String> {
        let data = dfg.get(op)?;
        let data = if data.is_none() {
            return Err(("Builder add_uses: op points to None").to_string());
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

            OpData::Add { lhs, rhs }
            | OpData::Sub { lhs, rhs }
            | OpData::Mul { lhs, rhs }
            | OpData::Div { lhs, rhs }
            | OpData::Mod { lhs, rhs }
            | OpData::Ne { lhs, rhs }
            | OpData::Eq { lhs, rhs }
            | OpData::Gt { lhs, rhs }
            | OpData::Lt { lhs, rhs }
            | OpData::Ge { lhs, rhs }
            | OpData::Le { lhs, rhs }
            | OpData::And { lhs, rhs }
            | OpData::Or { lhs, rhs }
            | OpData::Xor { lhs, rhs }
            | OpData::Shl { lhs, rhs }
            | OpData::Shr { lhs, rhs }
            | OpData::Sar { lhs, rhs } => {
                dfg.add_use(lhs, op)?;
                dfg.add_use(rhs, op)?;
            }

            // GlobalAlloca: Do not maintain uses for global alloca
            _ => { /* do nothing */}
        }
        Ok(())
    }

    // create an instruction after current instruction
    pub fn create(
        &mut self,
        dfg: &mut DFG,
        typ: Type,
        attrs: Vec<Attr>,
        data: OpData,
    ) -> Result<OpId, String> {
        // append_at will update the prev and next pointers accordingly
        let op_id = dfg.insert_at(self.current_inst, Op::new(typ, attrs, data))?;
        // update current_inst to the newly created instruction
        self.current_inst = Some(op_id);
        Ok(op_id)
    }

    pub fn create_new_block(&mut self, func: &mut Function) -> Result<BBId, String> {
        let cfg = &mut func.cfg;
        let bb_id = cfg.alloc(BasicBlock::new())?;
        self.current_block = Some(bb_id);
        // set current_inst to None, which is the head of the new block
        self.current_inst = None;
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
