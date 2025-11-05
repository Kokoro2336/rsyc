use crate::ast::exp::IRObj;
use crate::koopa_ir::koopa_ir::{BasicBlock, DataFlowGraph, Func, InstId};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// type of value
#[derive(Debug, Clone)]
pub enum BType {
    Int,
    Void,
}

impl BType {
    pub fn as_str(&self) -> &'static str {
        match self {
            BType::Int => "int",
            BType::Void => "void",
        }
    }

    pub fn size_in_bytes(&self) -> u32 {
        match self {
            BType::Int => 4,
            BType::Void => 0,
        }
    }
}

impl std::fmt::Display for BType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BType::Int => write!(f, "i32"),
            BType::Void => write!(f, "void"),
        }
    }
}

pub struct Context {
    // current function
    pub func: Rc<Func>,
    // current block
    pub basic_block: Option<Rc<BasicBlock>>,
    // current function's symbol tables
    pub global_const_table: HashMap<String, IRObj>,
    // current function's pointer tables
    pub global_pointer_table: HashMap<String, IRObj>,
}

impl Context {
    pub fn new(func: Rc<Func>, basic_block: Option<Rc<BasicBlock>>) -> Self {
        Context {
            func: Rc::clone(&func),
            basic_block,
            global_const_table: HashMap::new(),
            global_pointer_table: HashMap::new(),
        }
    }
}

pub struct ContextStack {
    pub stack: Vec<Context>,
}

impl ContextStack {
    pub fn new() -> Self {
        ContextStack { stack: vec![] }
    }

    pub fn enter_block(&mut self, basic_block: Rc<BasicBlock>) {
        let stack = &mut self.stack;
        let last_context = stack.last_mut().unwrap();
        // simple replacement of basic_block
        last_context.basic_block = Some(basic_block);
    }

    pub fn enter_func(&mut self, func: Rc<Func>) {
        let stack = &mut self.stack;
        stack.push(Context::new(func, None));
    }

    pub fn enter_scope(&mut self) {
        let stack = &mut self.stack;
        let last_context = stack.last_mut().unwrap();
        let func = Rc::clone(&last_context.func);
        let basic_block = Rc::clone(last_context.basic_block.as_ref().unwrap());

        stack.push(Context::new(func, Some(basic_block)));
    }

    pub fn exit_func(&mut self) {
        let stack = &mut self.stack;
        stack.pop();
    }

    pub fn exit_scope(&mut self) {
        let stack = &mut self.stack;
        stack.pop();
    }

    pub fn insert_const(&mut self, name: String, value: IRObj) {
        if let Some(current_context) = self.stack.last_mut() {
            current_context.global_const_table.insert(name, value);
        } else {
            panic!("No context available to insert constant");
        }
    }

    pub fn insert_pointer(&mut self, name: String, value: IRObj) {
        if let Some(current_context) = self.stack.last_mut() {
            current_context.global_pointer_table.insert(name, value);
        } else {
            panic!("No context available to insert pointer");
        }
    }

    pub fn set_pointer_initialized(&mut self, name: &str) {
        for context in self.stack.iter_mut().rev() {
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
        for context in self.stack.iter().rev() {
            if let Some(value) = context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_latest_pointer(&self, name: &str) -> Option<IRObj> {
        for context in self.stack.iter().rev() {
            if let Some(value) = context.global_pointer_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_current_const(&self, name: &str) -> Option<IRObj> {
        let stack = &self.stack;
        if let Some(current_context) = stack.last() {
            if let Some(value) = current_context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_current_pointer(&self, name: &str) -> Option<IRObj> {
        let stack = &self.stack;
        if let Some(current_context) = stack.last() {
            if let Some(value) = current_context.global_pointer_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get_current_dfg(&self) -> Rc<RefCell<DataFlowGraph>> {
        let stack = &self.stack;
        if let Some(current_context) = stack.last() {
            Rc::clone(&current_context.func.dfg)
        } else {
            panic!("No context available to get DFG");
        }
    }
    
    pub fn get_current_inst_list(&self) -> Rc<RefCell<Vec<InstId>>> {
        let stack = &self.stack;
        if let Some(current_context) = stack.last() {
            if let Some(basic_block) = &current_context.basic_block {
                Rc::clone(&basic_block.inst_list)
            } else {
                panic!("You couldn't call this func when current basic_block is None");
            }
        } else {
            panic!("No context available to get instruction list");
        }
    }

    pub fn get_current_func(&self) -> Rc<Func> {
        let stack = &self.stack;
        if let Some(current_context) = stack.last() {
            Rc::clone(&current_context.func)
        } else {
            panic!("No context available to get current function");
        }
    }

    pub fn get_current_basic_block(&self) -> Rc<BasicBlock> {
        let stack = &self.stack;
        if let Some(current_context) = stack.last() {
            if let Some(basic_block) = &current_context.basic_block {
                Rc::clone(basic_block)
            } else {
                panic!("You couldn't call this func when basic_block is None");
            }
        } else {
            panic!("No context available to get current IR block");
        }
    }

    /// find any type of named IRObj with highest priority in the stack.
    pub fn find_highest_priority(&self, name: &str) -> Option<IRObj> {
        for context in self.stack.iter().rev() {
            if let Some(value) = context.global_pointer_table.get(name) {
                return Some(value.clone());
            }
            if let Some(value) = context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        None
    }
}

thread_local! {
    pub static CONTEXT_STACK: RefCell<ContextStack> = RefCell::new(ContextStack::new());
}
