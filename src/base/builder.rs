use crate::base::ir::{Attr, BBId, Function, Op, OpData, OpId};
use crate::base::{LoopInfo, Type};

pub struct Builder<'a> {
    pub loop_stack: Vec<LoopInfo>,
    // current func
    pub current_func: Option<&'a mut Function>,
    // current basic block
    pub current_block: Option<BBId>,
    // current instruction
    pub current_inst: Option<OpId>,
}

impl<'a> Builder<'a> {
    pub fn new() -> Self {
        Self {
            loop_stack: vec![],
            current_func: None,
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

    pub fn guard(&'a mut self) -> BuilderGuard<'a, 'a> {
        BuilderGuard::new(self)
    }

    pub fn set_current_function(&mut self, func: &'a mut Function) {
        self.current_func = Some(func);
    }

    pub fn set_current_block(&mut self, block_id: BBId) {
        self.current_block = Some(block_id);
    }

    pub fn set_current_inst(&mut self, inst_id: OpId) {
        self.current_inst = Some(inst_id);
    }

    // create an instruction before current instruction
    pub fn create(&mut self, typ: Type, attrs: Vec<Attr>, data: OpData) -> Result<OpId, String> {
        let dfg = if let Some(func) = &mut self.current_func {
            &mut func.dfg
        } else {
            return Err("Builder create: no current function".to_string());
        };
        let current_inst = if let Some(inst_id) = self.current_inst {
            inst_id
        } else {
            return Err("Builder create: no current instruction".to_string());
        };

        // insert_at will update the prev and next pointers accordingly
        let op_id = dfg.insert_at(current_inst, Op::new(typ, attrs, data))?;
        // update current_inst to the newly created instruction
        self.current_inst = Some(op_id);
        Ok(op_id)
    }
}

pub struct BuilderGuard<'a, 'builder>
where 'a: 'builder {
    pub builder: &'a mut Builder<'builder>,
    pub saved_loop_stack: Vec<LoopInfo>,
    pub saved_func: Option<&'builder mut Function>,
    pub saved_block: Option<BBId>,
    pub saved_inst: Option<OpId>,
}

impl<'a, 'builder> BuilderGuard<'a, 'builder> {
    pub fn new(builder: &'a mut Builder<'builder>) -> Self {
        let saved_loop_stack = std::mem::take(&mut builder.loop_stack);
        let saved_func = builder.current_func.take();
        let saved_block = builder.current_block;
        let saved_inst = builder.current_inst;
        Self {
            builder,
            saved_func,
            saved_loop_stack,
            saved_block,
            saved_inst,
        }
    }
}

impl Drop for BuilderGuard<'_, '_> {
    fn drop(&mut self) {
        self.builder.loop_stack = std::mem::take(&mut self.saved_loop_stack);
        self.builder.current_func = self.saved_func.take();
        self.builder.current_block = self.saved_block;
        self.builder.current_inst = self.saved_inst;
    }
}
