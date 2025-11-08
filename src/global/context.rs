use crate::sc::exp::IRObj;
use crate::ir::koopa::{BasicBlock, BlockId, DataFlowGraph, Func, InstId};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct SymbolContext {
    // current function's symbol tables
    pub global_const_table: HashMap<String, IRObj>,
    // current function's pointer tables
    pub global_pointer_table: HashMap<String, IRObj>,
}

impl SymbolContext {
    pub fn new() -> Self {
        SymbolContext {
            global_const_table: HashMap::new(),
            global_pointer_table: HashMap::new(),
        }
    }
}

pub struct LoopContext {
    pub while_entry: Option<BlockId>,
    pub end_block: Option<BlockId>,
    pub break_inst_list: Vec<InstId>,
    pub continue_inst_list: Vec<InstId>,
}

impl LoopContext {
    pub fn new() -> Self {
        LoopContext {
            while_entry: None,
            end_block: None,
            break_inst_list: vec![],
            continue_inst_list: vec![],
        }
    }

    pub fn add_while_entry(&mut self, block_id: BlockId) {
        self.while_entry = Some(block_id);
    }

    pub fn add_end_block(&mut self, block_id: BlockId) {
        self.end_block = Some(block_id);
    }

    pub fn add_break(&mut self, inst_id: InstId) {
        self.break_inst_list.push(inst_id);
    }

    pub fn add_continue(&mut self, inst_id: InstId) {
        self.continue_inst_list.push(inst_id);
    }
}

/// source code's context stack during parsing
pub struct ScContextStack {
    // current function
    pub func: Option<Rc<Func>>,
    // current block
    pub basic_block: Option<Rc<BasicBlock>>,
    // loop context stack, mainly used for recording break/continue inst in current loop
    pub loop_cxt_stack: Vec<LoopContext>,
    // symbol context stack
    pub sym_cxt_stack: Vec<SymbolContext>,
}

impl ScContextStack {
    pub fn new() -> Self {
        ScContextStack {
            func: None,
            basic_block: None,
            loop_cxt_stack: vec![],
            sym_cxt_stack: vec![],
        }
    }

    pub fn enter_block(&mut self, basic_block: Rc<BasicBlock>) {
        self.basic_block = Some(basic_block);
    }

    pub fn enter_func(&mut self, func: Rc<Func>) {
        self.func = Some(func);
        self.sym_cxt_stack.push(SymbolContext::new());  // remember to create a new symbol context as function's initial scope
    }

    pub fn exit_func(&mut self) {
        let stack = &mut self.sym_cxt_stack;
        stack.pop();
    }

    pub fn enter_scope(&mut self) {
        let stack = &mut self.sym_cxt_stack;
        stack.push(SymbolContext::new());
    }

    pub fn exit_scope(&mut self) {
        let stack = &mut self.sym_cxt_stack;
        stack.pop();
    }

    pub fn insert_const(&mut self, name: String, value: IRObj) {
        if let Some(current_context) = self.sym_cxt_stack.last_mut() {
            current_context.global_const_table.insert(name, value);
        } else {
            panic!("No context available to insert constant");
        }
    }

    pub fn insert_pointer(&mut self, name: String, value: IRObj) {
        if let Some(current_context) = self.sym_cxt_stack.last_mut() {
            current_context.global_pointer_table.insert(name, value);
        } else {
            panic!("No context available to insert pointer");
        }
    }

    pub fn set_pointer_initialized(&mut self, name: &str) {
        for context in self.sym_cxt_stack.iter_mut().rev() {
            if let Some(IRObj::Pointer {
                initialized,
                pointer_id: _,
            }) = context.global_pointer_table.get_mut(name)
            {
                *initialized = true;
                return;
            }
        }
        panic!("Pointer {} not found to set initialized", name);
    }

    pub fn get_latest_const(&self, name: &str) -> Option<IRObj> {
        for context in self.sym_cxt_stack.iter().rev() {
            if let Some(value) = context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_latest_pointer(&self, name: &str) -> Option<IRObj> {
        for context in self.sym_cxt_stack.iter().rev() {
            if let Some(value) = context.global_pointer_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_current_const(&self, name: &str) -> Option<IRObj> {
        let stack = &self.sym_cxt_stack;
        if let Some(current_context) = stack.last() {
            if let Some(value) = current_context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_current_pointer(&self, name: &str) -> Option<IRObj> {
        let stack = &self.sym_cxt_stack;
        if let Some(current_context) = stack.last() {
            if let Some(value) = current_context.global_pointer_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_current_dfg(&self) -> Rc<RefCell<DataFlowGraph>> {
        if let Some(func) = &self.func {
            Rc::clone(&func.dfg)
        } else {
            panic!("No current function available to get DFG");
        }
    }

    pub fn get_current_inst_list(&self) -> Rc<RefCell<Vec<InstId>>> {
        if let Some(basic_block) = &self.basic_block {
            Rc::clone(&basic_block.inst_list)
        } else {
            panic!("You couldn't call this func when current basic_block is None");
        }
    }

    pub fn get_current_func(&self) -> Rc<Func> {
        if let Some(func) = &self.func {
            Rc::clone(func)
        } else {
            panic!("Current function is none");
        }
    }

    pub fn get_current_basic_block(&self) -> Rc<BasicBlock> {
        if let Some(basic_block) = &self.basic_block {
            Rc::clone(basic_block)
        } else {
            panic!("Current basic block is none");
        }
    }

    /// find any type of named IRObj with highest priority in the stack.
    pub fn find_highest_priority(&self, name: &str) -> Option<IRObj> {
        for context in self.sym_cxt_stack.iter().rev() {
            if let Some(value) = context.global_pointer_table.get(name) {
                return Some(value.clone());
            }
            if let Some(value) = context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn enter_new_loop(&mut self) {
        self.loop_cxt_stack.push(LoopContext::new());
    }

    /// when exit, fill all break/continue inst's label
    pub fn exit_current_loop(&mut self) {
        self.loop_cxt_stack.pop();
    }

    /// add break/continue inst to loop_cxt_stack
    pub fn add_new_break(&mut self, inst_id: InstId) {
        if let Some(cxt) = self.loop_cxt_stack.last_mut() {
            cxt.add_break(inst_id);
        } else {
            panic!("No loop context available to add break instruction")
        };
    }

    pub fn add_new_continue(&mut self, inst_id: InstId) {
        if let Some(cxt) = self.loop_cxt_stack.last_mut() {
            cxt.add_continue(inst_id);
        } else {
            panic!("No loop context available to add continue instruction")
        };
    }

    pub fn fill_labels_for_current_loop(&self) {
        let cxt = self
            .loop_cxt_stack
            .last()
            .expect("No LoopContext available");

        let while_entry: BlockId =
            BlockId::from(cxt.while_entry.expect("While entry block is not set"));
        let end_block: BlockId = BlockId::from(cxt.end_block.expect("End block is not set"));

        let dfg = self.get_current_dfg();
        let mut dfg_mut = dfg.borrow_mut();

        for &continue_inst in &cxt.continue_inst_list {
            dfg_mut.fill_jump(continue_inst, while_entry);
        }

        for &break_inst in &cxt.break_inst_list {
            dfg_mut.fill_jump(break_inst, end_block);
        }
    }

    pub fn add_while_entry_to_current_loop(&mut self, block_id: BlockId) {
        if let Some(cxt) = self.loop_cxt_stack.last_mut() {
            cxt.add_while_entry(block_id);
        } else {
            panic!("No loop context available to add while entry block")
        };
    }

    pub fn add_end_block_to_current_loop(&mut self, block_id: BlockId) {
        if let Some(cxt) = self.loop_cxt_stack.last_mut() {
            cxt.add_end_block(block_id);
        } else {
            panic!("No loop context available to add end block")
        };
    }
}

thread_local! {
    pub static SC_CONTEXT_STACK: RefCell<ScContextStack> = RefCell::new(ScContextStack::new());
}
