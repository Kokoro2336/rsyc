use crate::asm::rv::{AsmBlock, AsmInst};
use crate::ir::config::SYSY_STD_LIB;
use crate::ir::koopa::{BasicBlock, BlockId, DataFlowGraph, Func, InstData, InstId, Program};

use std::cell::RefCell;
use std::rc::Rc;
use std::vec::Vec;

// AsmContext records the IR context processing now.
pub struct AsmContext {
    pub program: Option<Program>,
    pub current_func: Option<Rc<Func>>,
    pub current_ir_block: Option<Rc<BasicBlock>>,
    pub current_ir: Option<InstId>,
    pub current_asm_block: Option<Rc<AsmBlock>>,
}

impl AsmContext {
    pub fn new() -> Self {
        AsmContext {
            program: None,
            current_func: None,
            current_ir_block: None,
            current_ir: None,
            current_asm_block: None,
        }
    }

    pub fn set_program(&mut self, program: Program) {
        self.program = Some(program)
    }

    pub fn get_asm_label(&self, block_id: BlockId) -> String {
        let mut label: Option<String> = None;
        self.program
            .as_ref()
            .expect("No program available!")
            .funcs
            .iter()
            .for_each(|func| {
                let res = func.get_asm_label(block_id.clone());
                if let Some(block_label) = res {
                    if label.is_none() {
                        label = Some(block_label);
                    } else {
                        panic!("Block with deplicate name {block_id} found.")
                    }
                }
            });
        if let Some(block_label) = label {
            return block_label;
        }

        if let Some(func_decl) = SYSY_STD_LIB.with(|lib| {
            lib.iter()
                .find(|func_decl| func_decl.ident == block_id)
                .cloned()
        }) {
            func_decl.ident.clone()

        } else {
            panic!("No block named {block_id} found!");
        }
    }

    pub fn enter_func(&mut self, func: Rc<Func>) {
        self.current_func = Some(func);
    }

    pub fn enter_ir_block(&mut self, basic_block: Rc<BasicBlock>) {
        self.current_ir_block = Some(basic_block);
    }

    pub fn enter_asm_block(&mut self, asm_block: Rc<AsmBlock>) {
        self.current_asm_block = Some(asm_block);
    }

    pub fn enter_ir(&mut self, inst_id: InstId) {
        self.current_ir = Some(inst_id);
    }

    pub fn get_current_func(&self) -> Rc<Func> {
        Rc::clone(self.current_func.as_ref().unwrap())
    }

    pub fn get_current_ir_block(&self) -> Rc<BasicBlock> {
        Rc::clone(self.current_ir_block.as_ref().unwrap())
    }

    pub fn get_current_inst_list(&self) -> Rc<RefCell<Vec<InstId>>> {
        Rc::clone(&self.get_current_ir_block().inst_list)
    }

    pub fn get_current_dfg(&self) -> Rc<RefCell<DataFlowGraph>> {
        Rc::clone(&self.get_current_func().dfg)
    }

    pub fn get_current_ir_data(&self) -> InstData {
        let dfg = self.get_current_dfg();
        let dfg_borrow = dfg.borrow();
        dfg_borrow
            .get_inst(self.current_ir.as_ref().unwrap())
            .unwrap()
            .clone()
    }

    pub fn add_asm_inst(&mut self, asm_inst: AsmInst) {
        if let Some(asm_block) = &mut self.current_asm_block {
            asm_block.insts.borrow_mut().push(asm_inst);
        } else {
            panic!("No current ASM block to add instruction to.");
        }
    }
}

thread_local! {
    pub static ASM_CONTEXT: RefCell<AsmContext> = RefCell::new(AsmContext::new());
}
