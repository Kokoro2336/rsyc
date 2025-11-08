use crate::ast::ast::{Block, LVal};
use crate::ast::exp::{Exp, Expression, IRObj};
use crate::global::config::BType;
use crate::global::context::CONTEXT_STACK;
use crate::koopa_ir::config::KoopaOpCode;
use crate::koopa_ir::koopa_ir::{insert_ir, BasicBlock, BasicBlockType, InstData, Operand};

use std::rc::Rc;

pub trait Statement {
    fn parse(&self);
}

#[derive(Debug, Clone)]
pub enum Stmt {
    OpenStmt { open_stmt: Box<OpenStmt> },
    CloseStmt { close_stmt: Box<CloseStmt> },
}

impl Statement for Stmt {
    fn parse(&self) {
        match self {
            Stmt::OpenStmt { open_stmt } => {
                open_stmt.parse();
            }
            Stmt::CloseStmt { close_stmt } => {
                close_stmt.parse();
            }
        }
    }
}

pub trait ConditionStatement {
    fn condition(&self) -> &Exp;
    fn then_stmt(&self) -> &dyn Statement;
    fn else_stmt(&self) -> &dyn Statement;

    fn parse_single_condition(&self) {
        let result = self.condition().parse_var_exp();
        match result {
            IRObj::InstId(inst_id) => {
                let br_id = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::BR,
                    vec![
                        Operand::InstId(inst_id),
                        // we would add label(block id) here later
                    ],
                ));

                // create brand new block scope for then_stmt
                let then_block = Rc::new(BasicBlock::new(BasicBlockType::If));
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_block(Rc::clone(&then_block));
                    // push block here, don't do post-order traverse
                    stack
                        .borrow()
                        .get_current_func()
                        .push_basic_block(Rc::clone(&then_block));
                });
                self.then_stmt().parse();
                // add jump to the end block
                let jump_id = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::JUMP,
                    vec![],
                ));

                // block right after the condition statement
                let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_block(Rc::clone(&end_block));
                    stack
                        .borrow()
                        .get_current_func()
                        .push_basic_block(Rc::clone(&end_block));
                });

                // add label for br inst
                CONTEXT_STACK.with(|stack| {
                    let dfg = stack.borrow_mut().get_current_dfg();
                    let mut dfg_mut = dfg.borrow_mut();
                    dfg_mut.append_operands(
                        match br_id {
                            IRObj::InstId(id) => id,
                            _ => unreachable!("br_id couldn't be non InstId"),
                        },
                        vec![
                            Operand::BlockId(then_block.get_block_id()),
                            Operand::BlockId(end_block.get_block_id()),
                        ],
                    );
                    dfg_mut.append_operands(
                        match jump_id {
                            IRObj::InstId(id) => id,
                            _ => unreachable!("jump_id couldn't be non InstId"),
                        },
                        vec![Operand::BlockId(end_block.get_block_id())],
                    );
                });
            }

            // if the condition is a constant, we can directly decide whether to execute then_stmt
            IRObj::Const(value) => {
                if value != 0 {
                    let jump_then = insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::JUMP,
                        vec![],
                    ));

                    let then_block = Rc::new(BasicBlock::new(BasicBlockType::If));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&then_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&then_block));
                    });
                    self.then_stmt().parse();
                    let jump_end = insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::JUMP,
                        vec![],
                    ));

                    let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&end_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&end_block));
                    });

                    CONTEXT_STACK.with(|stack| {
                        let dfg = stack.borrow_mut().get_current_dfg();
                        let mut dfg_mut = dfg.borrow_mut();
                        dfg_mut.append_operands(
                            match jump_then {
                                IRObj::InstId(id) => id,
                                _ => unreachable!("jump_id couldn't be non InstId"),
                            },
                            vec![Operand::BlockId(then_block.get_block_id())],
                        );
                        dfg_mut.append_operands(
                            match jump_end {
                                IRObj::InstId(id) => id,
                                _ => unreachable!("jump_id couldn't be non InstId"),
                            },
                            vec![Operand::BlockId(end_block.get_block_id())],
                        );
                    });
                }
            }

            IRObj::Pointer {
                initialized: _,
                pointer_id: _,
            } => unreachable!("Pointer branch is unreachable in condition expression"),
            IRObj::None => {
                unreachable!("None branch is unreachable in condition expression")
            }
        }
    }

    fn parse_paired_condition(&self) {
        let result = self.condition().parse_var_exp();
        match result {
            IRObj::InstId(inst_id) => {
                let br_id = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::BR,
                    vec![
                        Operand::InstId(inst_id),
                        // we would add label(block id) here later
                    ],
                ));

                // create brand new block scope for then_stmt
                let then_block = Rc::new(BasicBlock::new(BasicBlockType::If));
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_block(Rc::clone(&then_block));
                    stack
                        .borrow_mut()
                        .get_current_func()
                        .push_basic_block(Rc::clone(&then_block));
                });
                self.then_stmt().parse();
                // add jump to the end block
                let then_to_end = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::JUMP,
                    vec![],
                ));

                // create brand new block scope for else_stmt
                let else_block = Rc::new(BasicBlock::new(BasicBlockType::If));
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_block(Rc::clone(&else_block));
                    stack
                        .borrow_mut()
                        .get_current_func()
                        .push_basic_block(Rc::clone(&else_block));
                });
                self.else_stmt().parse();
                let else_to_end = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::JUMP,
                    vec![],
                ));

                // block right after the condition statement
                let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_block(Rc::clone(&end_block));
                    stack
                        .borrow_mut()
                        .get_current_func()
                        .push_basic_block(Rc::clone(&end_block));
                });

                // add label for br inst
                CONTEXT_STACK.with(|stack| {
                    let dfg = stack.borrow_mut().get_current_dfg();
                    let mut dfg_mut = dfg.borrow_mut();
                    dfg_mut.append_operands(
                        match br_id {
                            IRObj::InstId(id) => id,
                            _ => unreachable!("br_id couldn't be non InstId"),
                        },
                        vec![
                            Operand::BlockId(then_block.get_block_id()),
                            Operand::BlockId(else_block.get_block_id()),
                        ],
                    );
                    dfg_mut.append_operands(
                        match then_to_end {
                            IRObj::InstId(id) => id,
                            _ => unreachable!("jump_id couldn't be non InstId"),
                        },
                        vec![Operand::BlockId(end_block.get_block_id())],
                    );
                    dfg_mut.append_operands(
                        match else_to_end {
                            IRObj::InstId(id) => id,
                            _ => unreachable!("jump_id couldn't be non InstId"),
                        },
                        vec![Operand::BlockId(end_block.get_block_id())],
                    );
                });
            }

            // if the condition is a constant, we can directly decide whether to execute then_stmt or else_stmt
            IRObj::Const(value) => {
                if value != 0 {
                    let jump_then = insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::JUMP,
                        vec![],
                    ));

                    let then_block = Rc::new(BasicBlock::new(BasicBlockType::If));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&then_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&then_block));
                    });
                    self.then_stmt().parse();
                    let jump_end = insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::JUMP,
                        vec![],
                    ));

                    let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&end_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&end_block));
                    });

                    CONTEXT_STACK.with(|stack| {
                        let dfg = stack.borrow_mut().get_current_dfg();
                        let mut dfg_mut = dfg.borrow_mut();
                        dfg_mut.append_operands(
                            match jump_then {
                                IRObj::InstId(id) => id,
                                _ => unreachable!("jump_id couldn't be non InstId"),
                            },
                            vec![Operand::BlockId(then_block.get_block_id())],
                        );
                        dfg_mut.append_operands(
                            match jump_end {
                                IRObj::InstId(id) => id,
                                _ => unreachable!("jump_id couldn't be non InstId"),
                            },
                            vec![Operand::BlockId(end_block.get_block_id())],
                        );
                    });
                } else {
                    let jump_else = insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::JUMP,
                        vec![],
                    ));

                    let else_block = Rc::new(BasicBlock::new(BasicBlockType::If));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&else_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&else_block));
                    });
                    self.else_stmt().parse();
                    let jump_end = insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::JUMP,
                        vec![],
                    ));

                    let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&end_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&end_block));
                    });

                    CONTEXT_STACK.with(|stack| {
                        let dfg = stack.borrow_mut().get_current_dfg();
                        let mut dfg_mut = dfg.borrow_mut();
                        dfg_mut.append_operands(
                            match jump_else {
                                IRObj::InstId(id) => id,
                                _ => unreachable!("jump_id couldn't be non InstId"),
                            },
                            vec![Operand::BlockId(else_block.get_block_id())],
                        );
                        dfg_mut.append_operands(
                            match jump_end {
                                IRObj::InstId(id) => id,
                                _ => unreachable!("jump_id couldn't be non InstId"),
                            },
                            vec![Operand::BlockId(end_block.get_block_id())],
                        );
                    });
                }
            }

            IRObj::Pointer {
                initialized: _,
                pointer_id: _,
            } => unreachable!("Pointer branch is unreachable in condition expression"),

            IRObj::None => {
                unreachable!("None branch is unreachable in condition expression")
            }
        }
    }
}

pub trait WhileStatement {
    fn condition(&self) -> &Exp;
    fn body_stmt(&self) -> &dyn Statement;

    fn parse_while_statement(&self) {
        // if the current basic block is not empty, we need to create a while_entry block
        // else we just reuse it as while_entry(change its type)
        let while_entry = if !CONTEXT_STACK.with(|stack| {
            stack
                .borrow()
                .get_current_basic_block()
                .inst_list
                .borrow()
                .is_empty()
        }) {
            let while_entry = Rc::new(BasicBlock::new(BasicBlockType::WhileEntry));
            CONTEXT_STACK.with(|stack| {
                stack.borrow_mut().enter_block(Rc::clone(&while_entry));
                stack
                    .borrow_mut()
                    .get_current_func()
                    .push_basic_block(Rc::clone(&while_entry));
            });
            while_entry
        } else {
            CONTEXT_STACK.with(|stack| {
                let current_block = stack.borrow().get_current_basic_block();
                let func = stack.borrow_mut().get_current_func();
                func.change_block_type(current_block.get_block_id(), BasicBlockType::WhileEntry);
                current_block
            })
        };

        // add while_entry id to current loop context
        CONTEXT_STACK.with(|stack| {
            stack
                .borrow_mut()
                .add_while_entry_to_current_loop(while_entry.get_block_id());
        });

        // parse as usual
        let result = self.condition().parse_var_exp();
        let entry_end_inst = match result {
            IRObj::InstId(inst_id) => {
                insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::BR,
                    vec![
                        Operand::InstId(inst_id),
                        // we would add label(block id) here later
                    ],
                ))
            }

            // if the condition is a constant, we can directly decide whether to execute body_stmt
            IRObj::Const(_) => insert_ir(InstData::new(
                BType::Void,
                IRObj::None,
                KoopaOpCode::JUMP,
                vec![],
            )),

            IRObj::Pointer {
                initialized: _,
                pointer_id: _,
            } => unreachable!("Pointer branch is unreachable in condition expression"),

            IRObj::None => {
                unreachable!("None branch is unreachable in condition expression")
            }
        };

        let while_body = Rc::new(BasicBlock::new(BasicBlockType::WhileBody));
        CONTEXT_STACK.with(|stack| {
            stack.borrow_mut().enter_block(Rc::clone(&while_body));
            stack
                .borrow_mut()
                .get_current_func()
                .push_basic_block(Rc::clone(&while_body));
        });

        // parse the body
        match result {
            IRObj::InstId(_) => {
                self.body_stmt().parse();
                let jump_to_entry = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::JUMP,
                    vec![],
                ));

                let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                CONTEXT_STACK.with(|stack| {
                    let mut stack_mut = stack.borrow_mut();

                    stack_mut.enter_block(Rc::clone(&end_block));
                    stack_mut
                        .get_current_func()
                        .push_basic_block(Rc::clone(&end_block));

                    // add end block to loop context
                    stack_mut.add_end_block_to_current_loop(end_block.get_block_id());
                });

                CONTEXT_STACK.with(|stack| {
                    {
                        let dfg = stack.borrow_mut().get_current_dfg();
                        let mut dfg_mut = dfg.borrow_mut();
                        let IRObj::InstId(inst_id) = entry_end_inst else {
                            unreachable!("entry_end_inst couldn't be non InstId")
                        };
                        let opcode = dfg_mut.get_inst(&inst_id).unwrap().opcode.clone();

                        // append label for while_entry
                        dfg_mut.append_operands(
                            inst_id,
                            match opcode {
                                KoopaOpCode::BR => vec![
                                    Operand::BlockId(while_body.get_block_id()),
                                    Operand::BlockId(end_block.get_block_id()),
                                ],
                                _ => unreachable!("entry_end_inst should be BR"),
                            },
                        );

                        dfg_mut.append_operands(
                            match jump_to_entry {
                                IRObj::InstId(id) => id,
                                _ => unreachable!("jump_id couldn't be non InstId"),
                            },
                            vec![Operand::BlockId(while_entry.get_block_id())], // const condition always jumps to body
                        );
                    }

                    // fill labels
                    stack.borrow().fill_labels_for_current_loop();
                });
            }

            IRObj::Const(value) => {
                if value != 0 {
                    self.body_stmt().parse();
                    let jump_to_entry = insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::JUMP,
                        vec![],
                    ));

                    // TODO: we still generate the end block even for constant true condition for now
                    let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                    CONTEXT_STACK.with(|stack| {
                        let mut stack_mut = stack.borrow_mut();
                        stack_mut.enter_block(Rc::clone(&end_block));
                        stack_mut
                            .get_current_func()
                            .push_basic_block(Rc::clone(&end_block));

                        // add end block to loop context
                        stack_mut.add_end_block_to_current_loop(end_block.get_block_id());
                    });

                    CONTEXT_STACK.with(|stack| {
                        {
                            let dfg = stack.borrow_mut().get_current_dfg();
                            let mut dfg_mut = dfg.borrow_mut();
                            let IRObj::InstId(inst_id) = entry_end_inst else {
                                unreachable!("entry_end_inst couldn't be non InstId")
                            };
                            let opcode = dfg_mut.get_inst(&inst_id).unwrap().opcode.clone();

                            // append label from while_entry to while_body
                            dfg_mut.append_operands(
                                inst_id,
                                match opcode {
                                    KoopaOpCode::JUMP => {
                                        vec![Operand::BlockId(while_body.get_block_id())]
                                    }
                                    _ => unreachable!("entry_end_inst should be JUMP"),
                                },
                            );

                            // append jump back to while_entry
                            dfg_mut.append_operands(
                                match jump_to_entry {
                                    IRObj::InstId(id) => id,
                                    _ => unreachable!("jump_id couldn't be non InstId"),
                                },
                                vec![Operand::BlockId(while_entry.get_block_id())], // const condition always jumps to body
                            );
                        }

                        // fill labels
                        stack.borrow().fill_labels_for_current_loop();
                    });
                } else {
                    // this case only while_entry exists, so we directly take while_body as end_block
                    CONTEXT_STACK.with(|stack| {
                        let stack = stack.borrow();
                        let dfg = stack.get_current_dfg();
                        let mut dfg_mut = dfg.borrow_mut();
                        let IRObj::InstId(inst_id) = entry_end_inst else {
                            unreachable!("entry_end_inst couldn't be non InstId")
                        };
                        let opcode = dfg_mut.get_inst(&inst_id).unwrap().opcode.clone();

                        // change while_body's type to normal
                        let func = stack.get_current_func();
                        func.change_block_type(while_body.get_block_id(), BasicBlockType::Normal);

                        // append label from while_entry to while_body(end_block)
                        dfg_mut.append_operands(
                            inst_id,
                            match opcode {
                                KoopaOpCode::JUMP => {
                                    vec![Operand::BlockId(while_body.get_block_id())]
                                }
                                _ => unreachable!("entry_end_inst should be JUMP"),
                            },
                        );
                    });
                }
            }

            IRObj::Pointer {
                initialized: _,
                pointer_id: _,
            } => {
                unreachable!("Pointer branch is unreachable in condition expression")
            }

            IRObj::None => {
                unreachable!("None branch is unreachable in condition expression")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum OpenStmt {
    SingleCond {
        condition: Exp,
        then_stmt: Box<Stmt>,
    },
    OpenCond {
        condition: Exp,
        then_stmt: Box<CloseStmt>, // this avoid the ambiguity in parsing
        else_stmt: Box<OpenStmt>,
    },
    OpenWhile {
        condition: Exp,
        body_stmt: Box<OpenStmt>, // this should be Statement rather than OpenStmt to allow both open and close while
    },
}

impl ConditionStatement for OpenStmt {
    fn condition(&self) -> &Exp {
        match self {
            OpenStmt::SingleCond { condition, .. } => condition,
            OpenStmt::OpenCond { condition, .. } => condition,
            OpenStmt::OpenWhile { condition, .. } => condition,
        }
    }

    fn then_stmt(&self) -> &dyn Statement {
        match self {
            OpenStmt::SingleCond { then_stmt, .. } => &**then_stmt,
            OpenStmt::OpenCond { then_stmt, .. } => &**then_stmt,
            OpenStmt::OpenWhile { body_stmt, .. } => &**body_stmt,
        }
    }

    fn else_stmt(&self) -> &dyn Statement {
        match self {
            OpenStmt::OpenCond { else_stmt, .. } => &**else_stmt,
            _ => panic!("Not a condition statement"),
        }
    }
}

impl WhileStatement for OpenStmt {
    fn condition(&self) -> &Exp {
        match self {
            OpenStmt::OpenWhile { condition, .. } => condition,
            _ => panic!("Not a while statement"),
        }
    }

    fn body_stmt(&self) -> &dyn Statement {
        match self {
            OpenStmt::OpenWhile { body_stmt, .. } => &**body_stmt,
            _ => panic!("Not a while statement"),
        }
    }
}

impl Statement for OpenStmt {
    fn parse(&self) {
        match self {
            OpenStmt::SingleCond { .. } => {
                self.parse_single_condition();
            }

            OpenStmt::OpenCond { .. } => {
                self.parse_paired_condition();
            }

            OpenStmt::OpenWhile { .. } => {
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_new_loop();
                });

                self.parse_while_statement();

                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().exit_current_loop();
                });
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum CloseStmt {
    SimpleStmt {
        simple_stmt: SimpleStmt,
    },
    CloseCond {
        condition: Exp,
        then_stmt: Box<CloseStmt>,
        else_stmt: Box<CloseStmt>,
    },
    CloseWhile {
        condition: Exp,
        body_stmt: Box<CloseStmt>,
    },
}

impl ConditionStatement for CloseStmt {
    fn condition(&self) -> &Exp {
        match self {
            CloseStmt::CloseCond { condition, .. } => condition,
            _ => panic!("Not a condition statement"),
        }
    }

    fn then_stmt(&self) -> &dyn Statement {
        match self {
            CloseStmt::CloseCond { then_stmt, .. } => &**then_stmt,
            _ => panic!("Not a condition statement"),
        }
    }

    fn else_stmt(&self) -> &dyn Statement {
        match self {
            CloseStmt::CloseCond { else_stmt, .. } => &**else_stmt,
            _ => panic!("Not a condition statement"),
        }
    }
}

impl WhileStatement for CloseStmt {
    fn condition(&self) -> &Exp {
        match self {
            CloseStmt::CloseWhile { condition, .. } => condition,
            _ => panic!("Not a while statement"),
        }
    }

    fn body_stmt(&self) -> &dyn Statement {
        match self {
            CloseStmt::CloseWhile { body_stmt, .. } => &**body_stmt,
            _ => panic!("Not a while statement"),
        }
    }
}

impl Statement for CloseStmt {
    fn parse(&self) {
        match self {
            CloseStmt::SimpleStmt { simple_stmt } => {
                simple_stmt.parse();
            }
            CloseStmt::CloseCond { .. } => {
                self.parse_paired_condition();
            }
            CloseStmt::CloseWhile { .. } => {
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_new_loop();
                });

                self.parse_while_statement();

                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().exit_current_loop();
                });
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SimpleStmt {
    RegularStmt { l_val: LVal, exp: Exp },
    RawExp { exp: Option<Exp> },
    Block { block: Box<Block> },
    Break,
    Continue,
    ReturnStmt { exp: Exp },
}

impl Statement for SimpleStmt {
    fn parse(&self) {
        match self {
            // normal assignment statement
            SimpleStmt::RegularStmt { l_val, exp } => {
                if CONTEXT_STACK
                    .with(|stack| stack.borrow().get_latest_const(l_val.ident.as_str()))
                    .is_some()
                {
                    panic!("Cannot assign to a constant variable");
                } else if CONTEXT_STACK
                    .with(|stack| stack.borrow().get_latest_pointer(l_val.ident.as_str()))
                    .is_none()
                {
                    panic!("Variable {} not declared", l_val.ident);
                }

                let pointer = CONTEXT_STACK
                    .with(|stack| stack.borrow().get_latest_pointer(l_val.ident.as_str()))
                    .unwrap();
                let result = exp.parse_var_exp();

                insert_ir(InstData::new(
                    BType::Void, // assuming integer type for simplicity
                    IRObj::None,
                    KoopaOpCode::STORE,
                    vec![
                        match result {
                            IRObj::InstId(id) => Operand::InstId(id),
                            IRObj::Const(value) => Operand::Const(value),
                            IRObj::Pointer {
                                initialized: _,
                                pointer_id: _,
                            } => unreachable!("Pointer branch is unreachable in STORE"),
                            IRObj::None => panic!("Cannot store void value"),
                        },
                        match pointer {
                            IRObj::Pointer {
                                initialized: _,
                                pointer_id,
                            } => Operand::Pointer(pointer_id),
                            _ => panic!("Expected a pointer for l_val {}", l_val.ident),
                        },
                    ],
                ));

                // set_pointer_initialized
                CONTEXT_STACK.with(|stack| {
                    stack
                        .borrow_mut()
                        .set_pointer_initialized(l_val.ident.as_str())
                });
            }

            // is it necessary?
            SimpleStmt::RawExp { exp } => {
                if let Some(e) = exp {
                    let _ = e.parse_var_exp();
                }
            }

            SimpleStmt::Block { block } => {
                CONTEXT_STACK.with(|stack| stack.borrow_mut().enter_scope());
                block.parse();
                CONTEXT_STACK.with(|stack| stack.borrow_mut().exit_scope());
            }

            SimpleStmt::Break => {
                if CONTEXT_STACK
                    .with(|stack| stack.borrow().get_current_inst_list().borrow().is_empty())
                {
                    CONTEXT_STACK.with(|stack| {
                        let block_id = stack.borrow().get_current_basic_block().get_block_id();
                        stack
                            .borrow()
                            .get_current_func()
                            .change_block_type(block_id, BasicBlockType::Break);
                    });
                } else {
                    let break_block = Rc::new(BasicBlock::new(BasicBlockType::Break));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&break_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&break_block));
                    });
                }

                // insert jump inst
                if let IRObj::InstId(break_inst) = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::JUMP,
                    vec![],
                )) {
                    // add current break inst to the loop context
                    CONTEXT_STACK.with(|stack| stack.borrow_mut().add_new_break(break_inst));
                }

                let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_block(Rc::clone(&end_block));
                    stack
                        .borrow_mut()
                        .get_current_func()
                        .push_basic_block(Rc::clone(&end_block));
                });
            }

            SimpleStmt::Continue => {
                if CONTEXT_STACK
                    .with(|stack| stack.borrow().get_current_inst_list().borrow().is_empty())
                {
                    CONTEXT_STACK.with(|stack| {
                        let block_id = stack.borrow().get_current_basic_block().get_block_id();
                        stack
                            .borrow()
                            .get_current_func()
                            .change_block_type(block_id, BasicBlockType::Continue);
                    });
                } else {
                    let continue_block = Rc::new(BasicBlock::new(BasicBlockType::Continue));
                    CONTEXT_STACK.with(|stack| {
                        stack.borrow_mut().enter_block(Rc::clone(&continue_block));
                        stack
                            .borrow_mut()
                            .get_current_func()
                            .push_basic_block(Rc::clone(&continue_block));
                    });
                }

                // insert jump inst
                if let IRObj::InstId(continue_inst) = insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::JUMP,
                    vec![],
                )) {
                    // add current continue inst to the loop context
                    CONTEXT_STACK.with(|stack| stack.borrow_mut().add_new_continue(continue_inst));
                }

                let end_block = Rc::new(BasicBlock::new(BasicBlockType::Normal));
                CONTEXT_STACK.with(|stack| {
                    stack.borrow_mut().enter_block(Rc::clone(&end_block));
                    stack
                        .borrow_mut()
                        .get_current_func()
                        .push_basic_block(Rc::clone(&end_block));
                });
            }

            SimpleStmt::ReturnStmt { exp } => {
                let result = exp.parse_var_exp();

                insert_ir(InstData::new(
                    BType::Void,
                    IRObj::None,
                    KoopaOpCode::RET,
                    vec![match result {
                        IRObj::InstId(id) => Operand::InstId(id),
                        IRObj::Const(value) => Operand::Const(value),
                        IRObj::Pointer {
                            initialized: _,
                            pointer_id: _,
                        } => unimplemented!(),
                        IRObj::None => Operand::None,
                    }],
                ));
            }
        }
    }
}
