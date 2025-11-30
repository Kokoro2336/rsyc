use crate::asm::config::RVRegCode;
use crate::asm::reg::RVREG_ALLOCATOR;
use crate::global::config::BType;
use crate::global::context::SC_CONTEXT_STACK;
use crate::ir::config::{KoopaOpCode, BLOCK_ID_ALLOCATOR, IR_VAR_ID_ALLOCATOR, SYSY_STD_LIB};
use crate::sc::ast::{FuncFParam, ReturnVal};
use crate::sc::exp::{Exp, Expression};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Program {
    pub global_vals: Vec<IRObj>,
    pub funcs: Vec<Rc<Func>>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            global_vals: vec![],
            funcs: vec![],
        }
    }

    pub fn push_func(&mut self, func: Rc<Func>) {
        self.funcs.push(func);
    }
}

// customize formatting for Program
impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        SYSY_STD_LIB.with(|std_lib| {
            for func_decl in std_lib.iter() {
                writeln!(
                    f,
                    "decl @{}({}): {}",
                    func_decl.ident,
                    func_decl
                        .params
                        .iter()
                        .map(|p| format!("{}", p.param_type))
                        .collect::<Vec<_>>()
                        .join(", "),
                    func_decl.func_type
                )?;
            }
            Ok(())
        })?;
        writeln!(f)?;

        // print global vals
        for global_val in &self.global_vals {
            match global_val {
                IRObj::Array {
                    ident, typ, vals, ..
                } => {
                    writeln!(
                        f,
                        "global @{} = alloc {}, {{{}}}",
                        ident,
                        typ,
                        vals.as_ref()
                            .expect("Global Array should have init vals!")
                            .iter()
                            .map(|init_val| init_val.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }

                IRObj::GlobalVar {
                    initialized,
                    global_var_id,
                    init_val,
                } => {
                    writeln!(
                        f,
                        "global @{} = alloc {}, {}",
                        global_var_id,
                        BType::Int,
                        if *initialized {
                            init_val.to_string()
                        } else if !*initialized && *init_val == 0 {
                            IRObj::ZeroInit.to_string()
                        } else {
                            panic!("Invalid global variable initialization")
                        }
                    )?;
                }

                _ => unreachable!("Invalid global value: {:?}", global_val),
            }
        }
        writeln!(f)?;

        for func in &self.funcs {
            SC_CONTEXT_STACK.with(|stack| stack.borrow_mut().enter_func(Rc::clone(func)));
            writeln!(f, "{func}")?;
            SC_CONTEXT_STACK.with(|stack| stack.borrow_mut().exit_func());
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct DataFlowGraph {
    next_inst_id: InstId,
    pub inst_map: HashMap<InstId, InstData>,
}

impl DataFlowGraph {
    pub fn new() -> Self {
        Self {
            next_inst_id: 0,
            inst_map: HashMap::new(),
        }
    }

    pub fn insert_inst(&mut self, inst: InstData) -> InstId {
        let inst_id = self.next_inst_id;
        self.inst_map.insert(inst_id, inst);
        self.next_inst_id += 1;
        inst_id
    }

    pub fn get_next_inst_id(&self) -> InstId {
        self.next_inst_id
    }

    pub fn get_inst(&self, inst_id: &InstId) -> Option<&InstData> {
        self.inst_map.get(inst_id)
    }

    pub fn free_reg_used(&mut self, inst_id: InstId) {
        if let Some(inst) = self.inst_map.get_mut(&inst_id) {
            RVREG_ALLOCATOR
                .with(|allocator| allocator.borrow_mut().free_reg(inst.reg_used.unwrap()));

            inst.free_reg_used();
        } else {
            panic!("Instruction not found for inst_id {:?}", inst_id);
        }
    }

    pub fn set_reg(&mut self, inst_id: &InstId, reg: Option<RVRegCode>) {
        if let Some(inst) = self.inst_map.get_mut(&*inst_id) {
            inst.set_reg(reg.unwrap());
            // concerning that InstData doesn't contain its inst_id, we have to occupy the reg on DFG layer
            RVREG_ALLOCATOR
                .with(|allocator| allocator.borrow_mut().occupy_reg(reg.unwrap(), *inst_id));
        } else {
            panic!("Instruction not found for inst_id {:?}", inst_id);
        }
    }

    pub fn add_user(&mut self, inst_id: &InstId, user_inst_id: InstId) {
        if let Some(inst) = self.inst_map.get_mut(&*inst_id) {
            inst.add_user(user_inst_id);
        } else {
            panic!("Instruction not found for inst_id {:?}", inst_id);
        }
    }

    pub fn remove_user(&mut self, inst_id: &InstId, user_inst_id: InstId) {
        if let Some(inst) = self.inst_map.get_mut(&*inst_id) {
            inst.remove_user(user_inst_id);

            // if no users, free the register
            if inst.users.is_empty() {
                if let Some(_) = inst.reg_used {
                    RVREG_ALLOCATOR
                        .with(|allocator| allocator.borrow_mut().free_reg(inst.reg_used.unwrap()));
                }
            }
        } else {
            panic!("Instruction not found for inst_id {:?}", inst_id);
        }
    }

    // api to modify operands of an inst.
    pub fn append_operands(&mut self, inst_id: InstId, operands: Vec<IRObj>) {
        if let Some(inst) = self.inst_map.get_mut(&inst_id) {
            inst.operands.extend(operands);
        } else {
            panic!("Instruction not found for inst_id {:?}", inst_id);
        }
    }

    // fill br inst
    pub fn fill_br(&mut self, inst_id: InstId, block_id1: BlockId, block_id2: BlockId) {
        if let Some(inst) = self.inst_map.get_mut(&inst_id) {
            if inst.operands.len() == 1 {
                inst.operands.push(IRObj::FuncSym(block_id1));
                inst.operands.push(IRObj::FuncSym(block_id2));
            } else {
                panic!("Branch instruction already has target blocks");
            }
        } else {
            panic!("Instruction not found for inst_id {:?}", inst_id);
        }
    }

    pub fn fill_jump(&mut self, inst_id: InstId, block_id: BlockId) {
        if let Some(inst) = self.inst_map.get_mut(&inst_id) {
            if inst.operands.is_empty() {
                inst.operands.push(IRObj::FuncSym(block_id));
            } else {
                panic!("Jump instruction already has target block");
            }
        } else {
            panic!("Instruction not found for inst_id {:?}", inst_id);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub func_type: BType,
    pub params: Vec<FuncFParam>,
    pub dfg: Rc<RefCell<DataFlowGraph>>,
    pub basic_blocks: Rc<RefCell<Vec<Rc<BasicBlock>>>>,
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "fun @{}({}): {} {{",
            self.name,
            self.get_params_str(),
            self.func_type
        )?;
        for block in &*self.basic_blocks.borrow() {
            SC_CONTEXT_STACK.with(|stack| stack.borrow_mut().enter_block(Rc::clone(block)));
            writeln!(f, "{}", block)?;
        }

        writeln!(f, "}}");
        Ok(())
    }
}

impl Func {
    pub fn new(name: String, func_type: BType, params: Vec<FuncFParam>) -> Self {
        Self {
            name,
            func_type,
            params,
            dfg: Rc::new(RefCell::new(DataFlowGraph::new())),
            basic_blocks: Rc::new(RefCell::new(vec![])),
        }
    }

    pub fn push_basic_block(&self, block: Rc<BasicBlock>) {
        self.basic_blocks.borrow_mut().push(Rc::clone(&block));
    }

    pub fn get_asm_label(&self, block_id: BlockId) -> Option<String> {
        if let Some(block) = self
            .basic_blocks
            .borrow()
            .iter()
            .find(|block| block.get_block_id() == block_id)
        {
            Some(block.block_id.clone())
        } else {
            None
        }
    }

    pub fn get_block(&self, block_id: BlockId) -> Rc<BasicBlock> {
        if let Some(basic_block) = self
            .basic_blocks
            .borrow()
            .iter()
            .find(|block| block.get_block_id() == block_id)
        {
            Rc::clone(&basic_block)
        } else {
            panic!("Cannot find block with id {}", block_id);
        }
    }

    pub fn remove_block(&mut self, block_id: BlockId) {
        let mut blocks = self.basic_blocks.borrow_mut();
        if let Some(pos) = blocks.iter().position(|b| b.block_id == block_id) {
            blocks.remove(pos);
        } else {
            panic!("Basic block with id {} not found", block_id);
        }
    }

    pub fn get_params_str(&self) -> String {
        self.params
            .iter()
            .map(|p| format!("{}: {}", p.ident, p.param_type))
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub fn change_block_type(&self, block_id: BlockId, new_type: BasicBlockType) {
        let blocks = self.basic_blocks.borrow_mut();
        if let Some(block) = blocks.iter().find(|b| b.block_id == block_id) {
            let mut block_type = block.block_type.borrow_mut();
            *block_type = new_type;
        } else {
            panic!("Basic block with id {} not found", block_id);
        }
    }

    pub fn has_block(&self, block_id: BlockId) -> bool {
        let blocks = self.basic_blocks.borrow();
        blocks.iter().any(|b| b.block_id == block_id)
    }

    pub fn end_with_return(&self) -> bool {
        let blocks = self.basic_blocks.borrow();
        if let Some(last_block) = blocks.last() {
            let inst_list = last_block.inst_list.borrow();
            if let Some(last_inst_id) = inst_list.last() {
                let dfg = self.dfg.borrow();
                if let Some(last_inst) = dfg.get_inst(last_inst_id) {
                    return matches!(last_inst.opcode, KoopaOpCode::RET);
                }
            }
        }
        false
    }
}

#[derive(Debug, Clone)]
pub enum BasicBlockType {
    FuncEntry,
    Normal,
    If,
    Else,
    Break,
    Continue,
    WhileEntry,
    WhileBody,
}

pub type BlockId = String;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub block_id: BlockId,
    pub block_type: RefCell<BasicBlockType>,
    pub inst_list: Rc<RefCell<Vec<InstId>>>,
    pub jump_to_inst: Vec<InstId>, // record inst that jump to current block
}

impl BasicBlock {
    pub fn new(block_type: BasicBlockType) -> Self {
        Self {
            block_id: match block_type {
                BasicBlockType::FuncEntry => {
                    SC_CONTEXT_STACK.with(|stack| stack.borrow().get_current_func().name.clone())
                }
                _ => format!(
                    "block_{}",
                    BLOCK_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc())
                ),
            },
            block_type: RefCell::new(block_type),
            inst_list: Rc::new(RefCell::new(vec![])),
            jump_to_inst: vec![],
        }
    }

    pub fn get_block_id(&self) -> BlockId {
        self.block_id.clone()
    }

    pub fn get_block_type(&self) -> BasicBlockType {
        self.block_type.borrow().clone()
    }
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "%{}: ", self.block_id)?;

        let dfg = SC_CONTEXT_STACK.with(|stack| stack.borrow().get_current_dfg());
        let dfg_borrow = dfg.borrow();

        for inst in &*self.inst_list.borrow() {
            let inst_data = dfg_borrow.get_inst(inst).unwrap();
            match inst_data.opcode {
                KoopaOpCode::RET => {
                    if let Some(operand) = inst_data.operands.first() {
                        writeln!(f, "  ret {}", operand.to_string())?;
                    } else {
                        writeln!(f, "  ret")?;
                    }
                    continue;
                }
                _ => match inst_data.ir_obj {
                    IRObj::IRVar((ir_var_id, _)) | IRObj::ReturnVal { ir_var_id, .. } => {
                        writeln!(f, "  %{ir_var_id} = {inst_data}")?;
                    }
                    IRObj::ScVar {
                        initialized: _,
                        sc_var_id,
                    } => {
                        writeln!(f, "  @{sc_var_id} = {inst_data}")?;
                    }
                    _ => {
                        writeln!(f, "  {inst_data}")?;
                    }
                },
            }
        }

        Ok(())
    }
}

/// instruction id for DFG
pub type InstId = u32;
pub type IRVarId = u32;

#[derive(Debug, Clone)]
pub enum IRObj {
    // regular operands
    Const(i32),               // maybe the operand is a constant value
    IRVar((IRVarId, InstId)), // we need to store InstId along with IRVarId for user tracking
    ScVar {
        initialized: bool,
        sc_var_id: u32,
    }, // sc_var to a variable in memory,
    GlobalVar {
        initialized: bool,
        global_var_id: String,
        init_val: i32,
    },
    // init values in array
    Array {
        ident: String,
        typ: BType,
        dimensions: Vec<u32>,
        vals: Option<Vec<IRObj>>,
    },
    ReturnVal {
        ir_var_id: IRVarId,
        inst_id: InstId,
        return_val: ReturnVal,
    },

    // special operands
    ZeroInit,         // zero initializer
    BType(BType),     // maybe the operand is a type
    FuncSym(BlockId), // block label(using id to represent)

    // function call
    Args(Vec<IRObj>), // argument list for call instruction
    Param {
        param_type: BType,
        idx: u32,
        ident: String,
    }, // parameters

    // None
    None,
}

impl IRObj {
    pub fn get_value(&self) -> i32 {
        match self {
            IRObj::Const(v) => *v,
            _ => panic!("Not a constant value: {:?}", self),
        }
    }

    pub fn get_id(&self) -> InstId {
        match self {
            IRObj::IRVar((id, _)) => *id,
            _ => panic!("Not an instruction ID: {:?}", self),
        }
    }

    pub fn new_ir_var() -> Self {
        IRObj::IRVar((
            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
            SC_CONTEXT_STACK
                .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
        ))
    }
}

impl IntoIterator for IRObj {
    type Item = IRObj;
    type IntoIter = std::vec::IntoIter<IRObj>;

    fn into_iter(self) -> Self::IntoIter {
        vec![self].into_iter()
    }
}

impl ToString for IRObj {
    fn to_string(&self) -> String {
        match self {
            IRObj::IRVar((ir_var_id, _)) => format!("%{}", ir_var_id),
            IRObj::Const(c) => format!("{}", c),
            IRObj::ScVar {
                initialized: _,
                sc_var_id,
            } => format!("@{}", sc_var_id),
            IRObj::GlobalVar {
                initialized: _,
                global_var_id,
                init_val: _,
            } => format!("@{}", global_var_id),
            IRObj::Array { ident, .. } => format!("@{}", ident),
            IRObj::ReturnVal {
                ir_var_id,
                inst_id: _,
                return_val: _,
            } => format!("%{}", ir_var_id),

            IRObj::BType(b_type) => format!("{}", b_type),
            IRObj::ZeroInit => "zeroinit".to_string(),
            IRObj::FuncSym(block_id) => format!("{}", block_id),

            IRObj::Args(args) => args
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            IRObj::Param {
                param_type: _,
                idx: _,
                ident,
            } => format!("#{ident}"),

            IRObj::None => "".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InstData {
    pub typ: BType,
    pub ir_obj: IRObj,
    pub opcode: KoopaOpCode,
    pub operands: Vec<IRObj>,
    pub users: Vec<InstId>, // instructions use this instruction's result
    pub reg_used: Option<RVRegCode>, // reg used by this instruction(excluding the source regs)
}

impl InstData {
    // id is either sc_var_id or inst_id
    pub fn new(typ: BType, ir_obj: IRObj, opcode: KoopaOpCode, operands: Vec<IRObj>) -> Self {
        Self {
            typ,
            ir_obj,
            opcode,
            operands,
            users: vec![],
            reg_used: None,
        }
    }

    pub fn add_user(&mut self, user_inst_id: InstId) {
        self.users.push(user_inst_id);
    }

    pub fn remove_user(&mut self, user_inst_id: InstId) {
        if let Some(pos) = self.users.iter().position(|&id| id == user_inst_id) {
            self.users.swap_remove(pos);
        }
    }

    pub fn free_reg_used(&mut self) {
        self.reg_used = None;
    }

    pub fn set_reg(&mut self, reg: RVRegCode) {
        self.reg_used = Some(reg);
    }
}

impl std::fmt::Display for InstData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if matches!(self.opcode, KoopaOpCode::CALL) {
            write!(
                f,
                "{} @{}({})",
                self.opcode,
                self.operands.first().unwrap().to_string(),
                self.operands[1].to_string()
            )
        } else {
            let operands_str = self
                .operands
                .iter()
                .map(|op| op.to_string())
                .collect::<Vec<_>>()
                .join(", ");

            write!(f, "{} {}", self.opcode, operands_str)
        }
    }
}

/// key function to insert instruction into current basic block and DFG
pub fn insert_ir(inst_data: InstData) -> InstId {
    let dfg = SC_CONTEXT_STACK.with(|stack| stack.borrow().get_current_dfg());
    let mut dfg_mut = dfg.borrow_mut();
    let inst_list = SC_CONTEXT_STACK.with(|stack| stack.borrow().get_current_inst_list());
    let mut inst_list_mut = inst_list.borrow_mut();

    let inst_id = dfg_mut.insert_inst(inst_data.clone());

    // add this inst as a user to all its operand instructions
    for operand in &inst_data.operands {
        if let IRObj::IRVar((_, ir_var_inst_id)) = operand {
            dfg_mut.add_user(ir_var_inst_id, inst_id);
        }
    }

    inst_list_mut.push(inst_id);
    inst_id
}

/// @return: IRObj::Var | IRObj::Const | IRObj::Returnval
pub fn parse_arr_item(array: &IRObj, indexes: &Vec<Exp>) -> IRObj {
    let typ = match &array {
        IRObj::Array {
            typ, dimensions, ..
        } => typ.clone(),
        _ => panic!("Not an array type: {:?}", array),
    };

    let (final_ptr, final_typ) = indexes
        .iter()
        .fold(
            (IRObj::None, typ.clone()),
            |(mut ir_obj, mut cur_typ), index_exp| {
                let result = index_exp.parse_var_exp();

                // TODO: boudary check
                ir_obj = IRObj::IRVar((
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                    SC_CONTEXT_STACK
                        .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
                ));

                let typ = match &cur_typ {
                    BType::Array { typ, len: _ } => BType::Pointer { typ: typ.clone() },
                    _ => panic!("Array type expected"),
                };
                insert_ir(InstData::new(
                    typ.clone(),
                    ir_obj.clone(),
                    KoopaOpCode::GETELEMPTR,
                    vec![result],
                ));

                (ir_obj, typ)
            },
        );

    let ir_obj = IRObj::new_ir_var();
    insert_ir(InstData::new(
        final_typ.clone(),
        ir_obj.clone(),
        KoopaOpCode::LOAD,
        vec![final_ptr.clone()],
    ));

    ir_obj
}
