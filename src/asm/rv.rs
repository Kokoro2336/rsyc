use crate::asm::config::{RVOpCode, RVRegCode, RVOperandType};
use crate::asm::reg::RVREG_ALLOCATOR;
use crate::asm::mem::STK_FRM_MANAGER;
use crate::ir::config::KoopaOpCode;
use crate::ir::koopa::{Func, InstData, Operand, Program};
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
        writeln!(f, ".data")?;
        // declaration for global values
        for val in &self.global_vals {
            writeln!(f, ".globl {}", val)?;
        }

        // assignment for global values
        for val in &self.global_vals {
            writeln!(f, "{}", val)?;
        }

        // text section
        writeln!(f, ".text")?;
        // print the global func symbols
        for block in &self.blocks {
            writeln!(f, ".globl {}", block.label)?;
        }

        // print the blocks
        for block in &self.blocks {
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

    pub fn from(program: &Program) -> Result<Self, Box<dyn std::error::Error>> {
        let mut asm = Asm::new();

        // add global_syms
        for val in &program.global_vals {
            asm.global_vals
                .push(AsmGlobalVal::new(val.name.clone(), val.val));
        }

        // add blocks
        for func in &program.funcs {
            STK_FRM_MANAGER.with(|manager| {
                let mut manager = manager.borrow_mut();
                manager.prologue(func);
            });

            ASM_CONTEXT.with(|asm_cxt| {
                let mut asm_cxt = asm_cxt.borrow_mut();
                asm_cxt.enter_func(Rc::clone(func));
            });

            asm.blocks.extend(AsmBlock::from(func));

            STK_FRM_MANAGER.with(|manager| {
                let mut manager = manager.borrow_mut();
                manager.epilogue();
            });
        }

        Ok(asm)
    }

}

pub struct AsmGlobalVal {
    pub name: String,
    pub val: i32,
}

impl std::fmt::Display for AsmGlobalVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;
        writeln!(f, "    .word {}", self.val)?;
        Ok(())
    }
}

impl AsmGlobalVal {
    pub fn new(name: String, val: i32) -> Self {
        Self { name, val }
    }
}

pub struct AsmBlock {
    pub label: String,
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
    pub fn new(label: String) -> Self {
        Self {
            label,
            insts: RefCell::new(Vec::new()),
        }
    }

    pub fn from(func: &Func) -> Vec<Rc<Self>> {
        let mut asm_blocks: Vec<Rc<AsmBlock>> = Vec::new();

        // process basic blocks
        for (idx, basic_block) in func.basic_blocks.borrow().iter().enumerate() {
            let asm_block = Rc::new(AsmBlock::new(func.get_asm_label(basic_block.get_block_id())));
            ASM_CONTEXT.with(|asm_cxt| {
                let mut asm_cxt = asm_cxt.borrow_mut();
                asm_cxt.enter_ir_block(Rc::clone(basic_block));
                asm_cxt.enter_asm_block(Rc::clone(&asm_block));
            });

            // TODO: epilogue
            // 1. whether to allocate return address
            // 2. how much space to allocate for callee-saved registers
            // 3. how much space to allocate for local variables
            // 4. whether to allocate space for function calling.
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
                if STK_FRM_MANAGER.with(|manager| manager.borrow().is_callee()) {
                    ASM_CONTEXT.with(|asm_cxt| {
                        let mut asm_cxt = asm_cxt.borrow_mut();
                        asm_cxt.add_asm_inst(AsmInst {
                            opcode: RVOpCode::SW,
                            rd: None,
                            rs1: Some(RVOperandType::MemWithReg {
                                reg: RVRegCode::SP,
                                offset: 0,
                            }),
                            rs2: None,
                            imm: None,
                        });
                    });
                }
            }

            // TODO: for now, we don't need to process funct_type and params

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
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
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
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
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
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                        rs2: Some(rd.clone()),
                        imm: None,
                    });
                });
                // Some(rd)
                RVOperandType::None
            }

            KoopaOpCode::ALLOC => {
                // only alloc space for ALLOC inst
                STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()));
                RVOperandType::None
            }

            KoopaOpCode::LOAD => {
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
                        rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
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

            KoopaOpCode::RET => {
                // if we need to load imm at return point, we must use a0 anyway.
                let op_reg = process_op(inst_data.operands.first().unwrap());

                // epilogue here
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
    operand: &Operand,
) -> RVOperandType {
    let current_inst_id = ASM_CONTEXT.with(|asm_cxt| asm_cxt.borrow().current_ir.unwrap());
    let inst_data = ASM_CONTEXT.with(|asm_cxt| asm_cxt.borrow().get_current_ir_data());

    match operand {
        Operand::Const(val) => {
            if *val == 0 {
                return RVOperandType::Temp(RVRegCode::ZERO);
            }

            // find a free temp reg
            if let RVOperandType::Temp(temp_reg) = match inst_data.opcode {
                KoopaOpCode::RET => RVOperandType::Temp(RVRegCode::A0), // for return, we must use a0
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

        Operand::InstId(inst_id) => {
            let mem_with_reg = STK_FRM_MANAGER.with(|manager| manager.borrow().get_named_var_wrapped(Operand::InstId(*inst_id).to_string()));
            let rs1 = match inst_data.opcode {
                KoopaOpCode::RET => RVOperandType::Temp(RVRegCode::A0), // for return, we must use a0
                _ => RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst_id))
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

        Operand::Pointer(pointer_id) => {
            STK_FRM_MANAGER.with(|manager| manager.borrow().get_named_var_wrapped(Operand::Pointer(*pointer_id).to_string()))
        }

        Operand::BlockId(block_id) => {
            RVOperandType::Label(ASM_CONTEXT.with(|asm_cxt| {
                let asm_cxt_borrow = asm_cxt.borrow();
                asm_cxt_borrow.get_current_func().get_asm_label(*block_id)
            }))
        }

        Operand::BType(_) => {
            unreachable!("BType as an operand would never reach here.")
        }

        Operand::None => RVOperandType::None,
    }
}
