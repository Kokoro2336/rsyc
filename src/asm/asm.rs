use crate::asm::config::{RVOpCode, RVRegCode, RVREG_ALLOCATOR, RVOperandType, STK_FRM_MANAGER};
use crate::koopa_ir::config::KoopaOpCode;
use crate::koopa_ir::koopa_ir::{Func, InstData, Operand, Program};
use crate::global::context::CONTEXT_STACK;

use std::rc::Rc;

pub struct Asm {
    pub global_vals: Vec<AsmGlobalVal>,
    pub blocks: Vec<AsmBlock>,
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

            CONTEXT_STACK.with(|stack| {
                let mut stack = stack.borrow_mut();
                stack.enter_func(Rc::clone(func));
            });

            asm.blocks.extend(AsmBlock::from(func));

            CONTEXT_STACK.with(|stack| {
                let mut stack = stack.borrow_mut();
                stack.exit_func();
            });

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
    pub insts: Vec<AsmInst>,
}

impl std::fmt::Display for AsmBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.label)?;
        for inst in &self.insts {
            write!(f, "    {}", inst)?;
        }
        Ok(())
    }
}

impl AsmBlock {
    pub fn new(label: String) -> Self {
        Self {
            label,
            insts: Vec::new(),
        }
    }

    pub fn from(func: &Func) -> Vec<Self> {
        let mut asm_blocks: Vec<AsmBlock> = Vec::new();

        // process basic blocks
        for (idx, basic_block) in func.basic_blocks.borrow().iter().enumerate() {
            let mut asm_block = AsmBlock::new(func.get_asm_label(basic_block.get_block_id()));

            // TODO:
            // 1. whether to allocate return address
            // 2. how much space to allocate for callee-saved registers
            // 3. how much space to allocate for local variables
            // 4. whether to allocate space for function calling.
            if idx == 0 {
                let mut asm_insts: Vec<AsmInst> = vec![];

                // allocate stack frame
                asm_insts.push(AsmInst {
                    opcode: RVOpCode::ADDI,
                    rd: Some(RVOperandType::Temp(RVRegCode::SP)),
                    rs1: Some(RVOperandType::Temp(RVRegCode::SP)),
                    rs2: None,
                    imm: Some(-STK_FRM_MANAGER.with(|manager| manager.borrow().get_size() as i32)),
                });

                // store return address
                if STK_FRM_MANAGER.with(|manager| manager.borrow().is_callee()) {
                    asm_insts.push(AsmInst {
                        opcode: RVOpCode::SW,
                        rd: None,
                        rs1: Some(RVOperandType::MemWithReg {
                            reg: RVRegCode::SP,
                            offset: 0,
                        }),
                        rs2: None,
                        imm: None,
                    });
                }

                asm_block.insts.extend(asm_insts);
            }

            // TODO: for now, we don't need to process funct_type and params

            CONTEXT_STACK.with(|stack| {
                let mut stack = stack.borrow_mut();
                stack.enter_block(Rc::clone(basic_block));
            });

            for inst in CONTEXT_STACK.with(|stack| {
                let stack = stack.borrow();
                stack.get_current_inst_list().borrow().clone()
            }) {
                let inst_data = {
                    let dfg = CONTEXT_STACK.with(|stack| stack.borrow().get_current_dfg());
                    let dfg_borrow = dfg.borrow();
                    dfg_borrow.get_inst(&inst).unwrap().clone()
                };

                let asm_insts = {
                    AsmInst::from(&inst, &inst_data)
                };

                asm_block.insts.extend(asm_insts);
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
    ) -> Vec<Self> {
        let mut v = Vec::new();

        let reg_used: RVOperandType = match inst_data.opcode {
            KoopaOpCode::EQ | KoopaOpCode::NE => {
                let rs1 = process_op(&mut v, inst, inst_data.operands.first().unwrap());
                let rs2 = process_op(&mut v, inst, inst_data.operands.get(1).unwrap());

                let rd1 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                v.push(AsmInst {
                    opcode: RVOpCode::XOR,
                    rd: Some(rd1.clone()),
                    rs1: Some(rs1.clone()),
                    rs2: Some(rs2.clone()),
                    imm: None,
                });

                let rd2 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                v.push(AsmInst {
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

                rs1.free_temp(); rs2.free_temp(); rd1.free_temp(); rd2.free_temp();
                v.push(AsmInst {
                    opcode: RVOpCode::SW,
                    rd: None,
                    rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                    rs2: Some(rd2.clone()),
                    imm: None,
                });
                RVOperandType::None
            }

            KoopaOpCode::AND
            | KoopaOpCode::OR => {
                let rs1 = process_op(&mut v, inst, inst_data.operands.first().unwrap());
                let rs2 = process_op(&mut v, inst, inst_data.operands.get(1).unwrap());

                let rd1 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                v.push(AsmInst {
                    opcode: RVOpCode::SNEZ,
                    rd: Some(rd1.clone()),
                    rs1: Some(rs1.clone()),
                    rs2: None,
                    imm: None,
                });

                let rd2 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                v.push(AsmInst {
                    opcode: RVOpCode::SNEZ,
                    rd: Some(rd2.clone()),
                    rs1: Some(rs2.clone()),
                    rs2: None,
                    imm: None,
                });

                let rd = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));
                v.push(AsmInst {
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

                rs1.free_temp(); rs2.free_temp(); rd1.free_temp(); rd2.free_temp(); rd.free_temp();
                v.push(AsmInst {
                    opcode: RVOpCode::SW,
                    rd: None,
                    rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                    rs2: Some(rd.clone()),
                    imm: None,
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
                let rs1 = process_op(&mut v, inst, inst_data.operands.first().unwrap());
                let rs2 = process_op(&mut v, inst, inst_data.operands.get(1).unwrap());

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
                v.push(AsmInst {
                    opcode: rv_opcode,
                    rd: Some(rd.clone()),
                    rs1: Some(rs1.clone()),
                    rs2: Some(rs2.clone()),
                    imm: None,
                });

                rs1.free_temp(); rs2.free_temp(); rd.free_temp();
                v.push(AsmInst {
                    opcode: RVOpCode::SW,
                    rd: None,
                    rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                    rs2: Some(rd.clone()),
                    imm: None,
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
                let rd = process_op(&mut v, inst, inst_data.operands.first().unwrap());
                let rs1 = RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*inst));

                v.push(AsmInst {
                    opcode: RVOpCode::LW,
                    rd: Some(rd.clone()), // the rd must be SP, so we directly use ::Temp
                    rs1: Some(rs1.clone()),
                    rs2: None,
                    imm: None,
                });

                rs1.free_temp(); rd.free_temp();
                v.push (AsmInst {
                    opcode: RVOpCode::SW,
                    rd: None,
                    rs1: Some(STK_FRM_MANAGER.with(|manager| manager.borrow_mut().alloc_named_var_wrapped(inst_data.ir_obj.to_string(), inst_data.typ.clone()))),
                    rs2: Some(rs1.clone()),
                    imm: None,
                });
                RVOperandType::None
            }

            KoopaOpCode::STORE => {
                let rs1 = process_op(&mut v, inst, inst_data.operands.get(1).unwrap());    
                let rs2 = process_op(&mut v, inst, inst_data.operands.first().unwrap());

                v.push(AsmInst {
                    opcode: RVOpCode::SW,
                    rd: None,
                    rs1: Some(rs1.clone()),
                    rs2: Some(rs2.clone()),
                    imm: None,
                });

                rs1.free_temp(); rs2.free_temp();
                RVOperandType::None
            }

            KoopaOpCode::BR => {
                let op_reg = process_op(&mut v, inst, inst_data.operands.first().unwrap());
                let label1 = process_op(&mut v, inst, inst_data.operands.get(1).unwrap());
                let label2 = process_op(&mut v, inst, inst_data.operands.get(2).unwrap());

                v.push(AsmInst {
                    opcode: RVOpCode::BNEZ,
                    rd: Some(label1),
                    rs1: Some(op_reg.clone()),
                    rs2: None,
                    imm: None,
                });

                v.push(AsmInst {
                    opcode: RVOpCode::J,
                    rd: Some(label2),
                    rs1: None,
                    rs2: None,
                    imm: None,
                });

                op_reg.free_temp();
                RVOperandType::None
            }

            KoopaOpCode::JUMP => {
                let label = process_op(&mut v, inst, inst_data.operands.first().unwrap());
                
                v.push(AsmInst {
                    opcode: RVOpCode::J,
                    rd: Some(label),
                    rs1: None,
                    rs2: None,
                    imm: None,
                });

                RVOperandType::None
            }

            KoopaOpCode::RET => {
                // if we need to load imm at return point, we must use a0 anyway.
                let op_reg = process_op(&mut v, inst, inst_data.operands.first().unwrap());

                // epilogue here
                v.push(AsmInst {
                    opcode: RVOpCode::ADDI,
                    rd: Some(RVOperandType::Temp(RVRegCode::SP)),
                    rs1: Some(RVOperandType::Temp(RVRegCode::SP)),
                    rs2: None,
                    imm: Some(STK_FRM_MANAGER.with(|manager| manager.borrow().get_size() as i32)),
                });

                v.push(AsmInst {
                    opcode: RVOpCode::RET,
                    rd: None,
                    rs1: None,
                    rs2: None,
                    imm: None,
                });

                op_reg.free_temp();
                RVOperandType::None
            }
        };

        if let RVOperandType::Perm(reg) = reg_used {
            CONTEXT_STACK.with(|stack| {
                let stack = stack.borrow();
                stack.get_current_dfg().borrow_mut().set_reg(inst, Some(reg));
            });
        }
        v
    }
}

fn process_op(
    v: &mut Vec<AsmInst>,
    current_inst_id: &u32,
    operand: &Operand,
) -> RVOperandType {
    let inst_data = CONTEXT_STACK.with(|stack| 
        stack
        .borrow()
        .get_current_dfg()
        .borrow()
        .get_inst(current_inst_id).unwrap().clone());

    match operand {
        Operand::Const(val) => {
            if *val == 0 {
                return RVOperandType::Temp(RVRegCode::ZERO);
            }

            // find a free temp reg
            if let RVOperandType::Temp(temp_reg) = match inst_data.opcode {
                KoopaOpCode::RET => RVOperandType::Temp(RVRegCode::A0), // for return, we must use a0
                _ => RVREG_ALLOCATOR.with(|allocator| allocator.borrow_mut().find_and_occupy_temp_reg(*current_inst_id))
            } {
                // load imm into the free reg
                let asm_inst = AsmInst {
                    opcode: RVOpCode::LI,
                    rd: Some(RVOperandType::Temp(temp_reg)),
                    rs1: None,
                    rs2: None,
                    imm: Some(*val),
                };

                v.push(asm_inst);
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

            v.push(AsmInst {
                opcode: RVOpCode::LW,
                rd: Some(mem_with_reg.clone()), // the rd
                rs1: Some(rs1.clone()),
                rs2: None,
                imm: None,
            });

            rs1
        }

        Operand::Pointer(pointer_id) => {
            STK_FRM_MANAGER.with(|manager| manager.borrow().get_named_var_wrapped(Operand::Pointer(*pointer_id).to_string()))
        }

        Operand::BlockId(block_id) => {
            RVOperandType::Label(CONTEXT_STACK.with(|stack| {
                let stack_borrow = stack.borrow();
                stack_borrow.get_current_func().get_asm_label(*block_id) 
            }))
        }

        Operand::BType(_) => {
            unreachable!("BType as an operand would never reach here.")
        }

        Operand::None => RVOperandType::None,
    }
}
