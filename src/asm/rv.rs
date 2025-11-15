use crate::asm::config::{RVOpCode, RVRegCode, RVOperandType, REG_PARAMS_MAX_NUM};
use crate::asm::reg::{RVREG_ALLOCATOR, RVRegAllocator};
use crate::asm::mem::STK_FRM_MANAGER;
use crate::ir::config::KoopaOpCode;
use crate::ir::koopa::{Func, InstData, IRObj, Program};
use crate::global::config::BType;
use crate::asm::context::ASM_CONTEXT;

use std::rc::Rc;
use std::cell::RefCell;

pub struct Asm {
    pub global_vals: Vec<AsmGlobalVal>,
    pub blocks: Vec<Rc<AsmBlock>>,
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // data section
        // declaration for global values
        for val in &self.global_vals {
            writeln!(f, "    .data")?;
            writeln!(f, "    .globl {}", val.name)?;
            writeln!(f, "{}", val)?;
        }

        // print the blocks
        for block in &self.blocks {
            // text section
            if block.is_global {
                writeln!(f, "    .text")?;
                writeln!(f, "    .globl {}", block.label)?;
            }

            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

impl Asm {
    pub fn new() -> Self {
        Self {
            global_vals: Vec::new(),
            blocks: Vec::new(),
        }
    }

    pub fn from(program: &Program) -> Self {
        let mut asm = Asm::new();

        // add global_syms
        program.global_vals.iter().for_each(|val| {
            match val {
                IRObj::GlobalVar { initialized, global_var_id, init_val } => {
                    asm.global_vals.push(AsmGlobalVal 
                        { name: global_var_id.clone(), init_val: *init_val, initialized: *initialized })
                },
                _ => unreachable!("Only GlobalVar can be converted to AsmGlobalVal!"),
            }
        });

        // add blocks
        for func in &program.funcs {
            STK_FRM_MANAGER.with(|manager| {
                let mut manager = manager.borrow_mut();
                manager.new_frame(func);
            });

            ASM_CONTEXT.with(|asm_cxt| {
                let mut asm_cxt = asm_cxt.borrow_mut();
                asm_cxt.enter_func(Rc::clone(func));
            });

            asm.blocks.extend(AsmBlock::from(func));
        }

        asm
    }

}

pub struct AsmGlobalVal {
    pub name: String,
    pub init_val: i32,
    pub initialized: bool,
}

impl std::fmt::Display for AsmGlobalVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;

        if !self.initialized && self.init_val == 0 {
            writeln!(f, "    .zero 4")?;

        } else {
            writeln!(f, "    .word {}", self.init_val)?;
        }
        Ok(())
    }
}

impl AsmGlobalVal {
    pub fn new(name: String, init_val: i32, initialized: bool) -> Self {
        Self { name, init_val, initialized }
    }
}

pub struct AsmBlock {
    pub label: String,
    pub is_global: bool,
    pub insts: RefCell<Vec<AsmInst>>,
}

impl std::fmt::Display for AsmBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.label)?;
        for inst in self.insts.borrow().iter() {
            write!(f, "    {}", inst)?;
        }
        Ok(())
    }
}

impl AsmBlock {
    pub fn new(label: String, is_global: bool) -> Self {
        Self {
            label,
            is_global,
            insts: RefCell::new(Vec::new()),
        }
    }

    pub fn from(func: &Func) -> Vec<Rc<Self>> {
        let mut asm_blocks: Vec<Rc<AsmBlock>> = Vec::new();

        // process basic blocks
        for (idx, basic_block) in func.basic_blocks.borrow().iter().enumerate() {
            let asm_block = Rc::new(AsmBlock::new(func.get_asm_label(basic_block.get_block_id()).expect(&format!("No block found")), idx == 0));

            ASM_CONTEXT.with(|asm_cxt| {
                let mut asm_cxt = asm_cxt.borrow_mut();
                asm_cxt.enter_ir_block(Rc::clone(basic_block));
                asm_cxt.enter_asm_block(Rc::clone(&asm_block));
            });

            // 1. whether to allocate return address
            // 2. how much space to allocate for callee-saved registers
            // 3. how much space to allocate for local variables
            // 4. whether to allocate space for function calling.
            // 5. you might need to handle params here
            if idx == 0 {
                // allocate stack frame
                ASM_CONTEXT.with(|asm_cxt| {
                    let mut asm_cxt = asm_cxt.borrow_mut();
                    asm_cxt.add_asm_inst(AsmInst {
                        opcode: RVOpCode::ADDI,
                        rd: Some(RVOperandType::Temp(RVRegCode::SP)),
                        rs1: Some(RVOperandType::Temp(RVRegCode::SP)),
                        rs2: None,
                        imm: Some(-STK_FRM_MANAGER.with(|manager| manager.borrow().get_size() as i32)),
                    });
                });

                // store return address
                if STK_FRM_MANAGER.with(|manager| manager.borrow().has_callee()) {
                    ASM_CONTEXT.with(|asm_cxt| {
                        let mut asm_cxt = asm_cxt.borrow_mut();
                        asm_cxt.add_asm_inst(AsmInst {
                            opcode: RVOpCode::SW,
                            rd: None,
                            rs1: Some(RVOperandType::MemWithReg {
                                reg: RVRegCode::SP,
                                offset: STK_FRM_MANAGER.with(|manager| manager.borrow().get_ra_offset()),
                            }),
                            rs2: Some(RVOperandType::Temp(RVRegCode::RA)),
                            imm: None,
                        });
                    });
                }
            }

            for inst in ASM_CONTEXT.with(|asm_cxt| {
                let asm_cxt = asm_cxt.borrow();
                asm_cxt.get_current_inst_list().borrow().clone()
            }) {
                ASM_CONTEXT.with(|asm_cxt| {
                    let mut asm_cxt = asm_cxt.borrow_mut();
                    asm_cxt.enter_ir(inst);
                });

                let inst_data = {
                    ASM_CONTEXT.with(|asm_cxt| asm_cxt.borrow().get_current_ir_data())
                };

                AsmInst::from(&inst, &inst_data)
            }

            asm_blocks.push(asm_block);
        }

        asm_blocks
    }

}

#[derive(Clone)]
pub struct AsmInst {
    pub opcode: RVOpCode,
    pub rd: Option<RVOperandType>,
    pub rs1: Option<RVOperandType>,
    pub rs2: Option<RVOperandType>,
    pub imm: Option<i32>,
}

impl std::fmt::Display for AsmInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.opcode)?;
        match self.opcode {
            RVOpCode::LA => {
                write!(f, " {}, {}", &self.rd.as_ref().unwrap(), &self.rs1.as_ref().unwrap())?;
            }

            RVOpCode::LW => {
                write!(f, " {}, {}", &self.rs1.as_ref().unwrap(), &self.rd.as_ref().unwrap())?;
            }

            RVOpCode::SW => {
                write!(f, " {}, {}", &self.rs2.as_ref().unwrap(), &self.rs1.as_ref().unwrap())?;
            }

            RVOpCode::BEQZ | RVOpCode::BNEZ => {
                write!(f, " {}, {}", &self.rs1.as_ref().unwrap(), &self.rd.as_ref().unwrap())?;
            }

            RVOpCode::J => {
                write!(f, " {}", &self.rd.as_ref().unwrap())?;
            }

            _ => {
                if self.rd.is_some() {
                    write!(f, " {}", &self.rd.as_ref().unwrap())?;
                }
                if self.rs1.is_some() {
                    write!(f, ", {}", &self.rs1.as_ref().unwrap())?;
                }
                if self.rs2.is_some() {
                    write!(f, ", {}", &self.rs2.as_ref().unwrap())?;
                }
                if self.imm.is_some() {
                    write!(f, ", {}", self.imm.as_ref().unwrap())?;
                }
            }
        }
        writeln!(f, "")?;
        Ok(())
    }
}

impl AsmInst {
    pub fn new() -> Self {
        Self {
            opcode: RVOpCode::LI,
            rd: None,
            rs1: None,
            rs2: None,
            imm: None,
        }
    }

    pub fn from(
        inst: &u32,
        inst_data: &InstData,
    ) {
        let reg_used: RVOperandType = match inst_data.opcode {
            KoopaOpCode::EQ | KoopaOpCode::NE => {
                let rs1 = process_op(inst_data.operands.first().unwrap());
                let rs2 = process_op(inst_data.operands.get(1).unwrap());

                let rd1 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::XOR,
                        rd: Some(rd1.clone()),
                        rs1: Some(rs1.clone()),
                        rs2: Some(rs2.clone()),
                        imm: None,
                    });
                });

                let rd2 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: match inst_data.opcode {
                            KoopaOpCode::EQ => RVOpCode::SEQZ,
                            KoopaOpCode::NE => RVOpCode::SNEZ,
                            _ => unreachable!(),
                        },
                        rd: Some(rd2.clone()),
                        rs1: Some(rd1.clone()),
                        rs2: None,
                        imm: None,
                    });
                });

                rs1.free_temp(); rs2.free_temp(); rd1.free_temp(); rd2.free_temp();
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SW,
                        rd: None,
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_l_val_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                        rs2: Some(rd2.clone()),
                        imm: None,
                    });
                });
                RVOperandType::None
            }

            KoopaOpCode::AND
            | KoopaOpCode::OR => {
                let rs1 = process_op(inst_data.operands.first().unwrap());
                let rs2 = process_op(inst_data.operands.get(1).unwrap());

                let rd1 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SNEZ,
                        rd: Some(rd1.clone()),
                        rs1: Some(rs1.clone()),
                        rs2: None,
                        imm: None,
                    });
                });

                let rd2 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SNEZ,
                        rd: Some(rd2.clone()),
                        rs1: Some(rs2.clone()),
                        rs2: None,
                        imm: None,
                    });
                });

                let rd = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: match inst_data.opcode {
                            KoopaOpCode::AND => RVOpCode::AND,
                            KoopaOpCode::OR => RVOpCode::OR,
                            _ => unreachable!(),
                        },
                        rd: Some(rd.clone()),
                        rs1: Some(rd1.clone()),
                        rs2: Some(rd2.clone()),
                        imm: None,
                    });
                });

                rs1.free_temp(); rs2.free_temp(); rd1.free_temp(); rd2.free_temp(); rd.free_temp();
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SW,
                        rd: None,
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_l_val_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                        rs2: Some(rd.clone()),
                        imm: None,
                    });
                });
                // Some(rd)
                RVOperandType::None
            }

            KoopaOpCode::ADD
            | KoopaOpCode::SUB
            | KoopaOpCode::MUL
            | KoopaOpCode::DIV
            | KoopaOpCode::MOD

            // warning: SysY doesn't have bitwise and, or, xor,
            // but bitwise ops are the default ops in rv
            | KoopaOpCode::XOR 

            | KoopaOpCode::LT
            | KoopaOpCode::LE
            | KoopaOpCode::GT 
            | KoopaOpCode::GE 

            | KoopaOpCode::SAR
            | KoopaOpCode::SHL
            | KoopaOpCode::SHR => {
                let rs1 = process_op(inst_data.operands.first().unwrap());
                let rs2 = process_op(inst_data.operands.get(1).unwrap());

                let rv_opcode = match inst_data.opcode {
                    KoopaOpCode::ADD => RVOpCode::ADD,
                    KoopaOpCode::SUB => RVOpCode::SUB,
                    KoopaOpCode::MUL => RVOpCode::MUL,
                    KoopaOpCode::DIV => RVOpCode::DIV,
                    KoopaOpCode::MOD => RVOpCode::REM,

                    KoopaOpCode::XOR => RVOpCode::XOR,

                    KoopaOpCode::LT => RVOpCode::SLT,
                    KoopaOpCode::LE => RVOpCode::SLT,
                    KoopaOpCode::GT => RVOpCode::SGT, 
                    KoopaOpCode::GE => RVOpCode::SGT, 

                    KoopaOpCode::SAR => RVOpCode::SRA,
                    KoopaOpCode::SHL => RVOpCode::SLL,
                    KoopaOpCode::SHR => RVOpCode::SRL,
                    _ => unreachable!(),
                };

                let rd = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: rv_opcode,
                        rd: Some(rd.clone()),
                        rs1: Some(rs1.clone()),
                        rs2: Some(rs2.clone()),
                        imm: None,
                    });
                });

                rs1.free_temp(); rs2.free_temp(); rd.free_temp();
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SW,
                        rd: None,
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_l_val_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                        rs2: Some(rd.clone()),
                        imm: None,
                    });
                });
                // Some(rd)
                RVOperandType::None
            }

            KoopaOpCode::ALLOC => {
                // only alloc space for ALLOC inst
                STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_l_val_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()));
                RVOperandType::None
            }

            KoopaOpCode::LOAD => {
                // little trick: assuming that rd use t0 to load and then free it
                // later rs1 could reuse the freed t0.
                let rd = process_op(inst_data.operands.first().unwrap());
                let rs1 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));

                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::LW,
                        rd: Some(rd.clone()), // the rd must be SP, so we directly use ::Temp
                        rs1: Some(rs1.clone()),
                        rs2: None,
                        imm: None,
                    });
                });

                rs1.free_temp(); rd.free_temp();
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SW,
                        rd: None,
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_l_val_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                        rs2: Some(rs1.clone()),
                        imm: None,
                    });
                });
                RVOperandType::None
            }

            KoopaOpCode::STORE => {
                let rs1 = process_op(inst_data.operands.get(1).unwrap());    
                let rs2 = process_op(inst_data.operands.first().unwrap());

                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SW,
                        rd: None,
                        rs1: Some(rs1.clone()),
                        rs2: Some(rs2.clone()),
                        imm: None,
                    });
                });

                rs1.free_temp(); rs2.free_temp();
                RVOperandType::None
            }

            KoopaOpCode::BR => {
                let op_reg = process_op(inst_data.operands.first().unwrap());
                let label1 = process_op(inst_data.operands.get(1).unwrap());
                let label2 = process_op(inst_data.operands.get(2).unwrap());

                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::BNEZ,
                        rd: Some(label1),
                        rs1: Some(op_reg.clone()),
                        rs2: None,
                        imm: None,
                    });
                });

                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::J,
                        rd: Some(label2),
                        rs1: None,
                        rs2: None,
                        imm: None,
                    });
                });

                op_reg.free_temp();
                RVOperandType::None
            }

            KoopaOpCode::JUMP => {
                let label = process_op(inst_data.operands.first().unwrap());
                
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::J,
                        rd: Some(label),
                        rs1: None,
                        rs2: None,
                        imm: None,
                    });
                });

                RVOperandType::None
            }

            KoopaOpCode::CALL => {
                // store caller-saved regs
                let occupied_perm_regs = RVREG_ALLOCATOR.with(|allocator| allocator.borrow().find_inst_occupied_perm_reg(*inst));
                let saved_regs = occupied_perm_regs.iter().map(|item| {
                    let reg = if let RVOperandType::Perm(reg) = item {reg} else {unreachable!("Item couldn't be non Perm type")};
                    let name = format!("%saved_{reg}").to_string();
                    let mem_space = STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_saved_reg_wrapped(name.clone()));

                    ASM_CONTEXT.with(|cxt| {
                        cxt.borrow_mut().add_asm_inst(AsmInst {
                            opcode: RVOpCode::SW,
                            rd: None,
                            rs1: Some(mem_space),
                            rs2: Some(RVOperandType::Perm(*reg)),
                            imm: None,
                        });
                    });

                    (name, reg)
                });

                // parse label and params
                let params = if let RVOperandType::Params(params) = process_op(inst_data.operands.get(1).unwrap()) {params} else {unreachable!("params couldn't be non RVOperandType::Params")};
                let label = process_op(inst_data.operands.first().unwrap());

                // call function
                ASM_CONTEXT.with(|cxt| cxt.borrow_mut().add_asm_inst(AsmInst {
                    opcode: RVOpCode::CALL,
                    rd: Some(label),
                    rs1: None,
                    rs2: None,
                    imm: None,
                }));


                // saved A0 to mem for now, we would return it as Perm reg later
                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::SW,
                        rd: None,
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_l_val_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                        rs2: Some(RVOperandType::Temp(RVRegCode::A0)),
                        imm: None,
                    });
                });

                // free params reg and mem
                params.iter().for_each(|reg| reg.free_temp());
                STK_FRM_MANAGER.with(|manager| manager.borrow_mut().reset_param_area());

                // restore caller-saved regs
                saved_regs.for_each(|(name, reg)| {
                    let mem_space = STK_FRM_MANAGER.with(|manager| manager.borrow().get_saved_reg_wrapped(name));
                    ASM_CONTEXT.with(|cxt| {
                        cxt.borrow_mut().add_asm_inst(AsmInst {
                            opcode: RVOpCode::LW,
                            rd: Some(mem_space),
                            rs1: Some(RVOperandType::Perm(*reg)),
                            rs2: None,
                            imm: None,
                        });
                    });
                });
                STK_FRM_MANAGER.with(|manager| manager.borrow_mut().reset_saved_regs_area());

                RVOperandType::None
            }

            KoopaOpCode::RET => {
                // if we need to load imm at return point, we must use a0 anyway.
                let operand = inst_data.operands.first();
                let op_reg = if let Some(res) = operand {process_op(res)} else {RVOperandType::None};

                // epilogue should be binded with ret
                if STK_FRM_MANAGER.with(|manager| manager.borrow().has_callee()) {
                    ASM_CONTEXT.with(|asm_cxt| {
                        let mut asm_cxt = asm_cxt.borrow_mut();
                        asm_cxt.add_asm_inst(AsmInst {
                            opcode: RVOpCode::LW,
                            rd: Some(RVOperandType::MemWithReg {
                                reg: RVRegCode::SP,
                                offset: STK_FRM_MANAGER.with(|manager| manager.borrow().get_ra_offset()),
                            }),
                            rs1: Some(RVOperandType::Temp(RVRegCode::RA)),
                            rs2: None,
                            imm: None,
                        });
                    });
                }

                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::ADDI,
                        rd: Some(RVOperandType::Temp(RVRegCode::SP)),
                        rs1: Some(RVOperandType::Temp(RVRegCode::SP)),
                        rs2: None,
                        imm: Some(STK_FRM_MANAGER.with(|manager| manager.borrow().get_size() as i32)),
                    });
                });

                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                        opcode: RVOpCode::RET,
                        rd: None,
                        rs1: None,
                        rs2: None,
                        imm: None,
                    });
                });

                op_reg.free_temp();
                RVOperandType::None
            }
        };

        if let RVOperandType::Perm(reg) = reg_used {
            ASM_CONTEXT.with(|asm_cxt| {
                let asm_cxt = asm_cxt.borrow();
                asm_cxt.get_current_dfg().borrow_mut().set_reg(inst, Some(reg));
            });
        }
    }
}

fn process_op(
    operand: &IRObj,
) -> RVOperandType {
    let current_inst_id = ASM_CONTEXT.with(|asm_cxt| asm_cxt.borrow().current_ir.unwrap());
    let inst_data = ASM_CONTEXT.with(|asm_cxt| asm_cxt.borrow().get_current_ir_data());

    match operand {
        IRObj::Const(val) => {
            if *val == 0 {
                return RVOperandType::Temp(RVRegCode::ZERO);
            }

            // find a free temp reg
            if let RVOperandType::Temp(temp_reg) = match inst_data.opcode {
                KoopaOpCode::RET => RVOperandType::Temp(RVRegCode::A0), // for return, we must use a0

                KoopaOpCode::CALL => {
                    let param_reg = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_param_reg(current_inst_id));
                    match param_reg {
                        RVOperandType::None => {
                            // if no param reg available, load from stack_frame first and then store it directly
                            let temp_reg = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(current_inst_id));

                            ASM_CONTEXT.with(|asm_cxt| {
                                asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                                    opcode: RVOpCode::LI,
                                    rd: Some(temp_reg.clone()),
                                    rs1: None,
                                    rs2: None,
                                    imm: Some(*val),
                                });
                            });

                            let param_space = RVOperandType::MemWithReg {
                                offset: STK_FRM_MANAGER.with(|manager| 
                                            manager.borrow_mut().alloc_param(
                                            BType::Int
                                        )).0,
                                reg: RVRegCode::SP
                            };

                            ASM_CONTEXT.with(|asm_cxt| {
                                asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                                    opcode: RVOpCode::SW,
                                    rd: None,
                                    rs1: Some(param_space.clone()),
                                    rs2: Some(temp_reg.clone()),
                                    imm: None,
                                });
                            });

                            temp_reg.free_temp();
                            // return directly, don't need to load again
                            return param_space;
                        },
                        _ => param_reg
                    }
                }

                _ => RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(current_inst_id))
            } {
                // load imm into the free reg
                let asm_inst = AsmInst {
                    opcode: RVOpCode::LI,
                    rd: Some(RVOperandType::Temp(temp_reg)),
                    rs1: None,
                    rs2: None,
                    imm: Some(*val),
                };

                ASM_CONTEXT.with(|asm_cxt| {
                    asm_cxt.borrow_mut().add_asm_inst(asm_inst);
                });
                RVOperandType::Temp(temp_reg)
            } else {
                unimplemented!()
            }
        }

        IRObj::IRVar((_, inst_id)) => {
            let mem_with_reg = STK_FRM_MANAGER.with(|manager| manager.borrow().get_l_val_wrapped(operand.to_string()));
            let rs1 = match inst_data.opcode {
                KoopaOpCode::RET => RVOperandType::Temp(RVRegCode::A0), // for return, we must use a0

                // if opcode == CALL, it demonstrates that current operand is an arg.
                KoopaOpCode::CALL => {
                    let param_reg = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_param_reg(current_inst_id));
                    match param_reg {
                        RVOperandType::None => {
                            // if no param reg available, load from stack_frame first and then store it directly
                            let temp_reg = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(current_inst_id));
                            ASM_CONTEXT.with(|asm_cxt| {
                                asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                                    opcode: RVOpCode::LW,
                                    rd: Some(mem_with_reg.clone()), // the rd
                                    rs1: Some(temp_reg.clone()),
                                    rs2: None,
                                    imm: None,
                                });
                            });

                            let param_space = RVOperandType::MemWithReg {
                                offset: STK_FRM_MANAGER.with(|manager| 
                                            manager.borrow_mut().alloc_param(
                                            ASM_CONTEXT.with(|cxt| 
                                                cxt.borrow().get_current_dfg().borrow().get_inst(inst_id).unwrap().clone()).typ
                                        )).0,
                                reg: RVRegCode::SP
                            };

                            ASM_CONTEXT.with(|asm_cxt| {
                                asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                                    opcode: RVOpCode::SW,
                                    rd: None,
                                    rs1: Some(param_space.clone()),
                                    rs2: Some(temp_reg.clone()),
                                    imm: None,
                                });
                            });

                            temp_reg.free_temp();
                            // return directly, don't need to load again
                            return param_space;
                        },
                        _ => param_reg
                    }
                },
                _ => RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(current_inst_id))
            };

            ASM_CONTEXT.with(|asm_cxt| {
                asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                    opcode: RVOpCode::LW,
                    rd: Some(mem_with_reg.clone()), // the rd
                    rs1: Some(rs1.clone()),
                    rs2: None,
                    imm: None,
                });
            });

            rs1
        }

        IRObj::ScVar {..} => {
            STK_FRM_MANAGER.with(|manager| manager.borrow().get_l_val_wrapped(operand.to_string()))
        }

        IRObj::GlobalVar { initialized: _, global_var_id, init_val: _ } => {
            let temp_reg = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(current_inst_id));
            ASM_CONTEXT.with(|asm_cxt| {
                asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                    opcode: RVOpCode::LA,
                    rd: Some(temp_reg.clone()),
                    rs1: Some(RVOperandType::Label(global_var_id.clone())),
                    rs2: None,
                    imm: None,
                });
            });
            temp_reg.free_temp();

            RVOperandType::MemWithReg {
                reg: temp_reg.get_reg(),
                offset: 0,
            }
        }

        IRObj::FuncSym(block_id) => {
            RVOperandType::Label(ASM_CONTEXT.with(|asm_cxt| {
                let asm_cxt_borrow = asm_cxt.borrow();
                asm_cxt_borrow.get_asm_label(block_id.clone())
            }))
        }

        IRObj::BType(_)
        | IRObj::ZeroInit => {
            unreachable!("{:#?} as an operand would never reach here.", operand)
        }

        IRObj::Param { param_type: _, idx, ident: _ } => {
            if *idx > REG_PARAMS_MAX_NUM - 1 {
                // if the param is not in reg, we need to load it from older stack frame
                let origin_param = STK_FRM_MANAGER.with(|manager| {
                    manager.borrow().get_origin_param_wrapped(*idx)
                });
                let temp_reg = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(current_inst_id));

                ASM_CONTEXT.with(|asm_cxt| asm_cxt.borrow_mut().add_asm_inst(AsmInst {
                    opcode: RVOpCode::LW,
                    rd: Some(origin_param.clone()),
                    rs1: Some(temp_reg.clone()),
                    rs2: None,
                    imm: None,
                }));

                temp_reg

            } else {
                // if the param is in reg, we can use the corresponding param reg directly
                RVOperandType::Temp(RVRegAllocator::param_reg_from_idx(*idx as usize))
            }
        }

        IRObj::Args(args) => {
            RVOperandType::Params(args.iter()
                .map(|arg| Box::new(process_op(arg)))
                .collect::<Vec<Box<RVOperandType>>>()
            )
        }

        IRObj::None => RVOperandType::None,
    }
}
