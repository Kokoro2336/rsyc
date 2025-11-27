use crate::ir::config::SYSY_STD_LIB;
use crate::ir::koopa::IRObj;
use crate::ir::koopa::{BasicBlock, BlockId, DataFlowGraph, Func, InstId};
use crate::sc::ast::{CompUnit, FuncDef, FuncFParam, ReturnVal};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct SymbolContext {
    // current function's symbol tables
    pub global_const_table: HashMap<String, IRObj>,
    // current function's sc_var tables
    pub global_sc_var_table: HashMap<String, IRObj>,
}

impl SymbolContext {
    pub fn new() -> Self {
        SymbolContext {
            global_const_table: HashMap::new(),
            global_sc_var_table: HashMap::new(),
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
    // CompUnit
    pub comp_unit: Option<Rc<CompUnit>>,
    // current function
    pub func: Option<Rc<Func>>,
    // current block
    pub basic_block: Option<Rc<BasicBlock>>,
    // global symbol table
    pub global_sym_table: HashMap<String, IRObj>,
    // loop context stack, mainly used for recording break/continue inst in current loop
    pub loop_cxt_stack: Vec<LoopContext>,
    // symbol context stack
    pub sym_cxt_stack: Vec<SymbolContext>,
}

impl ScContextStack {
    pub fn new() -> Self {
        // import SysY standard library
        let global_sym_table = SYSY_STD_LIB.with(|lib| {
            lib.iter()
                .map(|func_decl| {
                    (
                        func_decl.ident.clone(),
                        IRObj::FuncSym(func_decl.ident.clone()),
                    )
                })
                .collect::<HashMap<_, _>>()
        });

        ScContextStack {
            comp_unit: None,
            func: None,
            basic_block: None,
            global_sym_table,
            loop_cxt_stack: vec![],
            sym_cxt_stack: vec![],
        }
    }

    pub fn set_comp_unit(&mut self, comp_unit: Rc<CompUnit>) {
        self.comp_unit = Some(comp_unit);
    }

    pub fn insert_global_sym(&mut self, name: String, value: IRObj) {
        let global_sym = self.global_sym_table.get(&name);
        if global_sym.is_some()
            && (matches!(global_sym.unwrap(), IRObj::FuncSym(_))
                | matches!(global_sym.unwrap(), IRObj::Const(_)))
        {
            panic!(
                "Global symbol {name} already exists, and you can't update value of FuncSym or Const"
            );
        }

        self.global_sym_table.insert(name, value);
    }

    pub fn get_global_sym(&self, name: &str) -> Option<IRObj> {
        self.global_sym_table.get(name).cloned()
    }

    pub fn has_local_cxt(&self) -> bool {
        !self.sym_cxt_stack.is_empty()
    }

    pub fn get_func_def(&self, func_ident: String) -> FuncDef {
        if let Some(comp_unit) = &self.comp_unit {
            if let Some(func_def) = comp_unit.func_defs.iter().find(|fd| fd.ident == func_ident) {
                func_def.clone()
            } else if let Some(func_decl) = SYSY_STD_LIB.with(|lib| {
                lib.iter()
                    .find(|func_decl| func_decl.ident == func_ident)
                    .cloned()
            }) {
                func_decl.clone()
            } else {
                panic!("No function definitions found in CompUnit");
            }
        } else {
            panic!("CompUnit is not set in context stack");
        }
    }

    pub fn get_func_def_params(&self, func_ident: String) -> Vec<FuncFParam> {
        let func_def = self.get_func_def(func_ident);
        func_def.params.clone()
    }

    pub fn enter_block(&mut self, basic_block: Rc<BasicBlock>) {
        self.basic_block = Some(basic_block);
    }

    pub fn enter_func(&mut self, func: Rc<Func>) {
        self.func = Some(func);
        self.sym_cxt_stack.push(SymbolContext::new()); // remember to create a new symbol context as function's initial scope
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

    pub fn insert_sc_var(&mut self, name: String, value: IRObj) {
        if let Some(current_context) = self.sym_cxt_stack.last_mut() {
            current_context.global_sc_var_table.insert(name, value);
        } else {
            panic!("No context available to insert sc_var");
        }
    }

    pub fn set_sc_var_initialized(&mut self, name: &str) {
        for context in self.sym_cxt_stack.iter_mut().rev() {
            if let Some(IRObj::ScVar {
                initialized,
                sc_var_id: _,
            }) = context.global_sc_var_table.get_mut(name)
            {
                *initialized = true;
                return;
            }
        }
        // if the val is in global context, ignore it.
        if !matches!(self.global_sym_table.get(name), Some(IRObj::ScVar { .. })) {
            panic!("ScVar {name} not found to set initialized");
        }
    }

    pub fn get_latest_const(&self, name: &str) -> Option<IRObj> {
        for context in self.sym_cxt_stack.iter().rev() {
            if let Some(value) = context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        // if not found in local contexts, check global context
        if let Some(value) = self.global_sym_table.get(name) {
            if let IRObj::Const(_) = value {
                return Some(value.clone());
            }
        }
        None
    }

    /// this func will search in both local and global contexts
    pub fn get_latest_var(&self, name: &str) -> Option<IRObj> {
        for context in self.sym_cxt_stack.iter().rev() {
            if let Some(value) = context.global_sc_var_table.get(name) {
                return Some(value.clone());
            }
        }
        // if not found in local contexts, check global context
        if let Some(value) = self.global_sym_table.get(name) {
            if matches!(value, IRObj::GlobalVar { .. }) {
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

    pub fn get_current_sc_var(&self, name: &str) -> Option<IRObj> {
        let stack = &self.sym_cxt_stack;
        if let Some(current_context) = stack.last() {
            if let Some(value) = current_context.global_sc_var_table.get(name) {
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
            if let Some(value) = context.global_sc_var_table.get(name) {
                return Some(value.clone());
            }
            if let Some(value) = context.global_const_table.get(name) {
                return Some(value.clone());
            }
        }
        if let Some(value) = self.global_sym_table.get(name) {
            return Some(value.clone());
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

        let while_entry: BlockId = cxt
            .while_entry
            .clone()
            .expect("While entry block is not set");
        let end_block: BlockId = cxt.end_block.clone().expect("End block is not set");

        let dfg = self.get_current_dfg();
        let mut dfg_mut = dfg.borrow_mut();

        for &continue_inst in &cxt.continue_inst_list {
            dfg_mut.fill_jump(continue_inst, while_entry.clone());
        }

        for &break_inst in &cxt.break_inst_list {
            dfg_mut.fill_jump(break_inst, end_block.clone());
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

    pub fn get_comp_unit(&self) -> Rc<CompUnit> {
        if let Some(comp_unit) = &self.comp_unit {
            Rc::clone(comp_unit)
        } else {
            panic!("CompUnit is not set in context stack");
        }
    }
}

pub struct ReturnTypes {
    pub return_vals: Option<Vec<IRObj>>,
}

impl ReturnTypes {
    pub fn new() -> Self {
        ReturnTypes { return_vals: None }
    }

    pub fn enter_func(&mut self) {
        if self.return_vals.is_none() {
            self.return_vals = Some(vec![]);
        } else {
            self.return_vals.as_mut().unwrap().clear();
        }
    }

    pub fn add_return_val(&mut self, ret_val: IRObj) {
        if let Some(return_vals) = &mut self.return_vals {
            return_vals.push(ret_val);
        } else {
            panic!("No return_vals available to add return value");
        }
    }

    pub fn analyze_return_type(&self) -> ReturnVal {
        if let Some(return_vals) = &self.return_vals {
            if return_vals.is_empty() {
                ReturnVal::Other
            } else if return_vals.iter().all(|val| matches!(val, IRObj::Const(0))) {
                ReturnVal::AlwaysZero
            } else if return_vals.iter().all(|val| match val {
                IRObj::Const(i) => *i != 0,
                IRObj::ReturnVal {
                    ir_var_id: _,
                    inst_id: _,
                    return_val,
                } => matches!(return_val, ReturnVal::AlwaysNonZero),
                _ => false,
            }) {
                ReturnVal::AlwaysNonZero
            } else {
                ReturnVal::Other
            }
        } else {
            ReturnVal::Other
        }
    }
}

thread_local! {
    pub static RETURN_TYPES: RefCell<ReturnTypes> = RefCell::new(ReturnTypes::new());
    pub static SC_CONTEXT_STACK: RefCell<ScContextStack> = RefCell::new(ScContextStack::new());
}
