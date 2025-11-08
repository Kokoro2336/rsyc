use crate::global::config::BType;
use crate::global::context::SC_CONTEXT_STACK;
use crate::ir::config::{IRObj, KoopaOpCode, IR_VAR_ID_ALLOCATOR};
use crate::ir::koopa::{insert_ir, InstData};
use crate::sc::ast::LVal;
use crate::sc::op::*;

pub trait Expression {
    fn parse_var_exp(&self) -> IRObj;

    fn parse_const_exp(&self) -> IRObj;
}

#[derive(Debug, Clone)]
pub enum Exp {
    LOrExp { lor_exp: Box<LOrExp> },
}

impl Expression for Exp {
    /// parse_unary_exp
    fn parse_var_exp(&self) -> IRObj {
        match self {
            Exp::LOrExp { lor_exp } => return lor_exp.parse_var_exp(),
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            Exp::LOrExp { lor_exp } => return lor_exp.parse_const_exp(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LOrExp {
    LAndExp {
        land_exp: Box<LAndExp>,
    },
    LOrExp {
        lor_exp: Box<LOrExp>,
        lor_op: LOrOp,
        land_exp: Box<LAndExp>,
    },
}

impl Expression for LOrExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            LOrExp::LAndExp { land_exp } => {
                return land_exp.parse_var_exp();
            }

            LOrExp::LOrExp {
                lor_exp,
                lor_op,
                land_exp,
            } => {
                let left = lor_exp.parse_var_exp();
                // perform short-circuit evaluation for logical OR
                if let IRObj::Const(v) = &left {
                    if *v != 0 {
                        return IRObj::Const(1);
                    }
                }

                let right = land_exp.parse_var_exp();
                if let IRObj::Const(v) = &right {
                    if *v != 0 {
                        return IRObj::Const(1);
                    }
                }

                let koopa_op = match lor_op {
                    LOrOp::Or => KoopaOpCode::OR,
                };

                let ir_obj = IRObj::IRVar(
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                );
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![
                        crate::ir::koopa::Operand::from_parse_result(left),
                        crate::ir::koopa::Operand::from_parse_result(right),
                    ],
                ));
                ir_obj
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            LOrExp::LAndExp { land_exp } => {
                return land_exp.parse_const_exp();
            }
            LOrExp::LOrExp {
                lor_exp,
                lor_op,
                land_exp,
            } => {
                let left = lor_exp.parse_const_exp();
                let right = land_exp.parse_const_exp();

                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match lor_op {
                            LOrOp::Or => {
                                if *l != 0 || *r != 0 {
                                    1
                                } else {
                                    0
                                }
                            }
                        };
                        IRObj::Const(res)
                    }
                    _ => panic!(
                        "Non-constant in const expression: left={:?}, right={:?}",
                        left, right
                    ),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum LAndExp {
    EqExp {
        eq_exp: Box<EqExp>,
    },
    LAndExp {
        land_exp: Box<LAndExp>,
        land_op: LAndOp,
        eq_exp: Box<EqExp>,
    },
}

impl Expression for LAndExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            LAndExp::EqExp { eq_exp } => eq_exp.parse_var_exp(),
            LAndExp::LAndExp {
                land_exp,
                land_op,
                eq_exp,
            } => {
                let left = land_exp.parse_var_exp();
                // perform short-circuit evaluation for logical AND
                if let IRObj::Const(v) = &left {
                    if *v == 0 {
                        return IRObj::Const(0);
                    }
                }

                let right = eq_exp.parse_var_exp();
                if let IRObj::Const(v) = &right {
                    if *v == 0 {
                        return IRObj::Const(0);
                    }
                }

                let koopa_op = match land_op {
                    LAndOp::And => KoopaOpCode::AND,
                };

                let ir_obj = IRObj::IRVar(
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                );
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![
                        crate::ir::koopa::Operand::from_parse_result(left),
                        crate::ir::koopa::Operand::from_parse_result(right),
                    ],
                ));
                ir_obj
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            LAndExp::EqExp { eq_exp } => {
                return eq_exp.parse_const_exp();
            }
            LAndExp::LAndExp {
                land_exp,
                land_op: _,
                eq_exp,
            } => {
                let left = land_exp.parse_const_exp();
                let right = eq_exp.parse_const_exp();
                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        IRObj::Const(if *l != 0 && *r != 0 { 1 } else { 0 })
                    }
                    _ => panic!(
                        "Non-constant in const expression: left={:?}, right={:?}",
                        left, right
                    ),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum EqExp {
    RelExp {
        rel_exp: Box<RelExp>,
    },
    EqExp {
        eq_exp: Box<EqExp>,
        eq_op: EqOp,
        rel_exp: Box<RelExp>,
    },
}

impl Expression for EqExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            EqExp::RelExp { rel_exp } => {
                return rel_exp.parse_var_exp();
            }
            EqExp::EqExp {
                eq_exp,
                eq_op,
                rel_exp,
            } => {
                let left = eq_exp.parse_var_exp();
                let right = rel_exp.parse_var_exp();

                let koopa_op = match eq_op {
                    EqOp::Eq => KoopaOpCode::EQ,
                    EqOp::Ne => KoopaOpCode::NE,
                };

                let ir_obj = IRObj::IRVar(
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                );
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![
                        crate::ir::koopa::Operand::from_parse_result(left),
                        crate::ir::koopa::Operand::from_parse_result(right),
                    ],
                ));
                ir_obj
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            EqExp::RelExp { rel_exp } => {
                return rel_exp.parse_const_exp();
            }
            EqExp::EqExp {
                eq_exp,
                eq_op,
                rel_exp,
            } => {
                let left = eq_exp.parse_const_exp();
                let right = rel_exp.parse_const_exp();
                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match eq_op {
                            EqOp::Eq => *l == *r,
                            EqOp::Ne => *l != *r,
                        };
                        IRObj::Const(if res { 1 } else { 0 })
                    }
                    _ => panic!(
                        "Non-constant in const expression: left={:?}, right={:?}",
                        left, right
                    ),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RelExp {
    AddExp {
        add_exp: Box<AddExp>,
    },
    RelExp {
        rel_exp: Box<RelExp>,
        rel_op: RelOp,
        add_exp: Box<AddExp>,
    },
}

impl Expression for RelExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            RelExp::AddExp { add_exp } => {
                return add_exp.parse_var_exp();
            }
            RelExp::RelExp {
                rel_exp,
                rel_op,
                add_exp,
            } => {
                let left = rel_exp.parse_var_exp();
                let right = add_exp.parse_var_exp();

                let koopa_op = match rel_op {
                    RelOp::Lt => KoopaOpCode::LT,
                    RelOp::Gt => KoopaOpCode::GT,
                    RelOp::Le => KoopaOpCode::LE,
                    RelOp::Ge => KoopaOpCode::GE,
                };

                let ir_obj = IRObj::IRVar(
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                );
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![
                        crate::ir::koopa::Operand::from_parse_result(left),
                        crate::ir::koopa::Operand::from_parse_result(right),
                    ],
                ));
                ir_obj
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            RelExp::AddExp { add_exp } => {
                return add_exp.parse_const_exp();
            }
            RelExp::RelExp {
                rel_exp,
                rel_op,
                add_exp,
            } => {
                let left = rel_exp.parse_const_exp();
                let right = add_exp.parse_const_exp();
                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match rel_op {
                            RelOp::Lt => *l < *r,
                            RelOp::Gt => *l > *r,
                            RelOp::Le => *l <= *r,
                            RelOp::Ge => *l >= *r,
                        };
                        IRObj::Const(if res { 1 } else { 0 })
                    }
                    _ => panic!(
                        "Non-constant in const expression: left={:?}, right={:?}",
                        left, right
                    ),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PrimaryExp {
        exp: Box<PrimaryExp>,
    },
    UnaryExp {
        unary_op: UnaryOp,
        unary_exp: Box<UnaryExp>,
    },
}

impl Expression for UnaryExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            // handle primary expression
            UnaryExp::PrimaryExp { exp } => {
                return exp.parse_var_exp();
            }

            // handle unary operation
            UnaryExp::UnaryExp {
                unary_op,
                unary_exp,
            } => {
                let parse_result = unary_exp.parse_var_exp();

                match unary_op {
                    UnaryOp::Plus => parse_result,
                    UnaryOp::Minus | UnaryOp::Not => {
                        let ir_obj = IRObj::IRVar(
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                        );
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            match unary_op {
                                UnaryOp::Minus => KoopaOpCode::SUB,
                                UnaryOp::Not => KoopaOpCode::EQ,
                                _ => unreachable!(),
                            },
                            vec![
                                crate::ir::koopa::Operand::Const(0),
                                crate::ir::koopa::Operand::from_parse_result(parse_result),
                            ],
                        ));
                        ir_obj
                    }
                }
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            UnaryExp::PrimaryExp { exp } => {
                return exp.parse_const_exp();
            }
            UnaryExp::UnaryExp {
                unary_op,
                unary_exp,
            } => {
                let inner = unary_exp.parse_const_exp();
                match inner {
                    IRObj::Const(v) => match unary_op {
                        UnaryOp::Plus => IRObj::Const(v),
                        UnaryOp::Minus => IRObj::Const(-v),
                        UnaryOp::Not => panic!("A const expression couldn't be a NOT expression"),
                    },
                    _ => panic!("Non-constant in const expression: {:?}", inner),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum MulExp {
    UnaryExp {
        unary_exp: Box<UnaryExp>,
    },
    MulExp {
        mul_exp: Box<MulExp>,
        mul_op: MulOp,
        unary_exp: Box<UnaryExp>,
    },
}

impl Expression for MulExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            MulExp::UnaryExp { unary_exp } => {
                return unary_exp.parse_var_exp();
            }
            MulExp::MulExp {
                mul_exp,
                mul_op,
                unary_exp,
            } => {
                let left = mul_exp.parse_var_exp();
                let right = unary_exp.parse_var_exp();

                let koopa_op = match mul_op {
                    MulOp::Mul => KoopaOpCode::MUL,
                    MulOp::Div => KoopaOpCode::DIV,
                    MulOp::Mod => KoopaOpCode::MOD,
                };

                let ir_obj = IRObj::IRVar(
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                );
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![
                        crate::ir::koopa::Operand::from_parse_result(left),
                        crate::ir::koopa::Operand::from_parse_result(right),
                    ],
                ));
                ir_obj
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            MulExp::UnaryExp { unary_exp } => {
                return unary_exp.parse_const_exp();
            }
            MulExp::MulExp {
                mul_exp,
                mul_op,
                unary_exp,
            } => {
                let left = mul_exp.parse_const_exp();
                let right = unary_exp.parse_const_exp();
                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match mul_op {
                            MulOp::Mul => *l * *r,
                            MulOp::Div => *l / *r,
                            MulOp::Mod => *l % *r,
                        };
                        IRObj::Const(res)
                    }
                    _ => panic!(
                        "Non-constant in const expression: left={:?}, right={:?}",
                        left, right
                    ),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum AddExp {
    MulExp {
        mul_exp: Box<MulExp>,
    },
    AddExp {
        add_exp: Box<AddExp>,
        add_op: AddOp,
        mul_exp: Box<MulExp>,
    },
}

impl Expression for AddExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            AddExp::MulExp { mul_exp } => {
                return mul_exp.parse_var_exp();
            }
            AddExp::AddExp {
                add_exp,
                add_op,
                mul_exp,
            } => {
                let left = add_exp.parse_var_exp();
                let right = mul_exp.parse_var_exp();

                let koopa_op = match add_op {
                    AddOp::Add => KoopaOpCode::ADD,
                    AddOp::Sub => KoopaOpCode::SUB,
                };

                let ir_obj = IRObj::IRVar(
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                );
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![
                        crate::ir::koopa::Operand::from_parse_result(left),
                        crate::ir::koopa::Operand::from_parse_result(right),
                    ],
                ));
                ir_obj
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            AddExp::MulExp { mul_exp } => {
                return mul_exp.parse_const_exp();
            }
            AddExp::AddExp {
                add_exp,
                add_op,
                mul_exp,
            } => {
                let left = add_exp.parse_const_exp();
                let right = mul_exp.parse_const_exp();
                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match add_op {
                            AddOp::Add => *l + *r,
                            AddOp::Sub => *l - *r,
                        };
                        IRObj::Const(res)
                    }
                    _ => panic!(
                        "Non-constant in const expression: left={:?}, right={:?}",
                        left, right
                    ),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Number { value: i32 },
    LVal { l_val: LVal },
    Exp { exp: Box<Exp> },
}

impl Expression for PrimaryExp {
    fn parse_var_exp(&self) -> IRObj {
        match self {
            PrimaryExp::Number { value } => IRObj::Const(*value),
            PrimaryExp::Exp { exp } => exp.parse_var_exp(),

            PrimaryExp::LVal { l_val } => {
                match SC_CONTEXT_STACK
                    .with(|stack| stack.borrow().find_highest_priority(&l_val.ident))
                {
                    Some(IRObj::Pointer {
                        initialized,
                        pointer_id,
                    }) => {
                        // this case the variables wasn't loaded before use
                        if !initialized {
                            panic!("Variable {} used before initialization", l_val.ident);
                        }

                        // if it's a variable stored in memory, load first and return inst_id.
                        let ir_obj = IRObj::IRVar(
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                        );
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            KoopaOpCode::LOAD,
                            vec![crate::ir::koopa::Operand::Pointer(pointer_id)],
                        ));
                        ir_obj
                    }
                    Some(IRObj::Const(value)) => IRObj::Const(value),
                    _ => {
                        panic!(
                            "LVal {} not found in var table, maybe the ident is not defined",
                            l_val
                        );
                    }
                }
            }
        }
    }

    fn parse_const_exp(&self) -> IRObj {
        match self {
            PrimaryExp::Number { value } => IRObj::Const(*value),
            PrimaryExp::Exp { exp } => exp.parse_const_exp(),

            PrimaryExp::LVal { l_val } => {
                if let Some(value) =
                    SC_CONTEXT_STACK.with(|stack| stack.borrow().get_latest_const(&l_val.ident))
                {
                    value
                } else {
                    panic!("LVal {} not found in const table, maybe the ident is for a variable or not defined", l_val);
                }
            }
        }
    }
}
