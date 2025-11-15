use crate::asm::config::{
    RVOperandType, RVRegCode, REG_PARAMS_MAX_NUM, RISCV_BITS, STK_FRM_BASE_LENGTH,
};
use crate::asm::context::ASM_CONTEXT;
use crate::global::config::BType;
use crate::ir::config::KoopaOpCode;
use crate::ir::koopa::{Func, IRObj};

use std::cell::RefCell;
use std::cmp::max;
use std::collections::HashMap;

#[derive(Debug, Clone)]
/// sp, fp, size of frame
pub struct StackFrame {
    // total size of stack frame
    size: u32,
    // saved regs
    saved_map: HashMap<String, u32>, // name: offset
    // saved reg offset
    saved_reg_offset: u32,
    // ra_offset
    ra_offset: Option<u32>,
    // offset relative to the top of l_val area.
    cur_l_val_offset: u32,
    // offset relative to the top of param area,
    cur_param_offset: u32,
    // variable map in this stack frame
    var_map: HashMap<String, (u32, u32)>, // variable name to (offset, size) in stack frame
}

impl StackFrame {
    pub fn reset_param_area(&mut self) {
        self.cur_param_offset = 0;
    }

    pub fn reset_saved_regs_area(&mut self) {
        self.saved_reg_offset = self.size
            - self.ra_offset.unwrap_or(0)
            - self.saved_map.len() as u32 * (RISCV_BITS / 8);
        self.saved_map.clear();
    }
}

#[derive(Debug)]
pub struct StackFrameManager {
    frame: Option<StackFrame>,
}

impl StackFrameManager {
    pub fn new() -> Self {
        Self { frame: None }
    }

    pub fn new_frame(&mut self, func: &Func) {
        // iterate inst_map, calculate stack frame size. as koopa ir is in SSA form,
        // each inst is only assigned once, so we can simply sum up the size of all as long as the inst have return value.
        let dfg = func.dfg.borrow();
        let mut ra_size = 0;
        let mut saved_reg_size = 0;
        let mut params_size = 0;

        let local_val_size = dfg.inst_map.iter().fold(0, |acc, (_, inst)| {
            if matches!(inst.opcode, KoopaOpCode::CALL) {
                // ra_size, reserve space for ra
                ra_size = 4;

                //TODO: saved_reg_size.
                // we need to know which caller-saved registers are used in this function when CALL executed,
                // but for now all the var are saved in mem, so no Perm Reg need to be saved acutally.

                // params_size, reserve space for outgoing parameters
                params_size = max(
                    params_size,
                    if let Some(IRObj::Args(args)) = &inst.operands.last() {
                        let total_size: u32 = args.iter().enumerate().fold(0, |acc, (idx, arg)| {
                            acc + if idx > (REG_PARAMS_MAX_NUM - 1) as usize {
                                match *arg {
                                    IRObj::IRVar((_, inst_id)) => ASM_CONTEXT.with(|ctx| {
                                        ctx.borrow()
                                            .get_current_dfg()
                                            .borrow()
                                            .get_inst(&inst_id)
                                            .expect("IR variable type not found in ASM context!")
                                            .typ
                                            .size_in_bytes()
                                    }),

                                    IRObj::ScVar { .. } => 4,
                                    _ => 0,
                                }
                            } else {
                                0
                            }
                        });
                        total_size
                    } else {
                        panic!("CALL instruction's last operand couldn't be non IRObj::Args!")
                    },
                );
            }

            // size of local variable
            acc + inst.typ.size_in_bytes()
        });

        // actually manager doesn't know the initial value of sp and fp,
        // so we use positive offset to represent them for convenience.
        let size =
            ((ra_size + saved_reg_size + params_size + local_val_size + (STK_FRM_BASE_LENGTH - 1))
                / STK_FRM_BASE_LENGTH)
                * STK_FRM_BASE_LENGTH;

        self.frame = Some(StackFrame {
            size,

            saved_map: HashMap::new(),
            saved_reg_offset: params_size + local_val_size,

            ra_offset: match ra_size {
                0 => None,
                _ => Some(size - ra_size),
            },

            cur_l_val_offset: params_size,
            cur_param_offset: 0,

            var_map: HashMap::new(),
        });
    }

    pub fn get_size(&self) -> u32 {
        self.frame.as_ref().expect("No active stack frame!").size
    }

    // @return (offset, size)
    pub fn alloc_l_val(&mut self, name: String, typ: BType) -> (u32, u32) {
        let frame = self.frame.as_mut().expect("No active stack frame!");

        let offset = frame.cur_l_val_offset;
        let size = typ.size_in_bytes();

        frame.var_map.insert(name, (offset, size));
        frame.cur_l_val_offset += size;

        (offset, size)
    }

    /// this would return RVOperandType with eventual offset in stack frame
    pub fn alloc_l_val_wrapped(&mut self, name: String, typ: BType) -> RVOperandType {
        let (offset, _) = self.alloc_l_val(name, typ);
        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::SP,
        }
    }

    pub fn get_l_val_wrapped(&self, name: String) -> RVOperandType {
        let frame = self.frame.as_ref().expect("No active stack frame!");
        let (offset, _) = frame
            .var_map
            .get(&name)
            .cloned()
            .unwrap_or_else(|| panic!("Variable {name} not found in current stack frame!"));

        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::SP,
        }
    }

    // reset param_area's offset
    pub fn reset_param_area(&mut self) {
        self.frame
            .as_mut()
            .expect("No active stack frame!")
            .reset_param_area();
    }

    pub fn reset_saved_regs_area(&mut self) {
        self.frame
            .as_mut()
            .expect("No active stack frame!")
            .reset_saved_regs_area();
    }

    pub fn alloc_param(&mut self, typ: BType) -> (u32, u32) {
        let frame = self.frame.as_mut().expect("No active stack frame!");
        let offset = frame.cur_param_offset;
        let size = typ.size_in_bytes();

        frame.cur_param_offset += size;
        (offset, size)
    }

    pub fn get_origin_param_wrapped(&self, idx: u32) -> RVOperandType {
        let offset = ASM_CONTEXT.with(|ctx| {
            ctx.borrow().get_current_func().params[0..(idx as usize)]
                .iter()
                .fold(0, |acc, param| acc + param.param_type.size_in_bytes())
        });

        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::S0,
        }
    }

    pub fn get_saved_reg_wrapped(&self, name: String) -> RVOperandType {
        let frame = self.frame.as_ref().expect("No active stack frame!");
        let offset =
            frame.saved_map.get(&name).cloned().unwrap_or_else(|| {
                panic!("Saved register {name} not found in current stack frame!")
            });

        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::SP,
        }
    }

    pub fn alloc_saved_reg_wrapped(&mut self, name: String) -> RVOperandType {
        let frame = self.frame.as_mut().expect("No active stack frame!");

        let offset = frame.saved_reg_offset;

        frame.saved_map.insert(name, offset);
        frame.saved_reg_offset += 4;

        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::SP,
        }
    }

    pub fn has_callee(&self) -> bool {
        self.frame
            .as_ref()
            .expect("No active stack frame!")
            .ra_offset
            .is_some()
    }

    pub fn get_ra_offset(&self) -> u32 {
        self.frame
            .as_ref()
            .expect("No active stack frame!")
            .ra_offset
            .expect("No return address reserved in current stack frame!")
    }
}

thread_local! {
    pub static STK_FRM_MANAGER: RefCell<StackFrameManager> = RefCell::new(StackFrameManager::new());
}
