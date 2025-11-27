use crate::global::config::BType;
use crate::global::context::SC_CONTEXT_STACK;
use crate::ir::config::{KoopaOpCode, IR_VAR_ID_ALLOCATOR};
use crate::ir::koopa::{insert_ir, IRObj, InstData};
use crate::sc::ast::{LVal, ReturnVal};
use crate::sc::op::*;

pub trait Expression {
    fn parse_var_exp(&self) -> IRObj;
    fn parse_const_exp(&self) -> IRObj;
    fn pre_parse(&self) -> IRObj;
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

    fn pre_parse(&self) -> IRObj {
        match self {
            Exp::LOrExp { lor_exp } => return lor_exp.pre_parse(),
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
            LOrExp::LAndExp { land_exp } => land_exp.parse_var_exp(),

            LOrExp::LOrExp {
                lor_exp,
                lor_op,
                land_exp,
            } => {
                let left = lor_exp.parse_var_exp();
                // perform short-circuit evaluation for logical OR
                match &left {
                    IRObj::Const(v) if *v != 0 => {
                        println!(
                            "Short-circuiting logical OR: left operand is non-zero constant: {v}"
                        );
                        return IRObj::Const(1);
                    }
                    IRObj::ReturnVal {
                        return_val,
                        ir_var_id,
                        ..
                    } if matches!(*return_val, ReturnVal::AlwaysNonZero) => {
                        println!("Short-circuiting logical OR: left operand is always non-zero return value: {:#?}, ir_var_id: {:?}", return_val, ir_var_id);
                        return IRObj::Const(1);
                    }
                    IRObj::None => panic!("Cannot use void type as an operand"),
                    _ => {}
                }

                let right = land_exp.parse_var_exp();
                match &right {
                    IRObj::Const(v) if *v != 0 => return IRObj::Const(1),
                    IRObj::ReturnVal { return_val, .. }
                        if matches!(*return_val, ReturnVal::AlwaysNonZero) =>
                    {
                        return IRObj::Const(1)
                    }
                    IRObj::None => panic!("Cannot use void type as an operand"),
                    _ => {}
                }

                // constant folding
                match (left.clone(), right.clone()) {
                    (IRObj::Const(left_val), IRObj::Const(right_val)) => {
                        return IRObj::Const((left_val != 0 || right_val != 0).into());
                    }
                    (
                        IRObj::ReturnVal {
                            return_val: left_return_val,
                            ..
                        },
                        IRObj::ReturnVal {
                            return_val: right_return_val,
                            ..
                        },
                    ) => {
                        return IRObj::Const(
                            (matches!(left_return_val, ReturnVal::AlwaysNonZero)
                                || matches!(right_return_val, ReturnVal::AlwaysNonZero))
                            .into(),
                        );
                    }
                    _ => {}
                }

                let converted_left = match &left {
                    IRObj::ReturnVal { .. } | IRObj::IRVar(_) => {
                        let ir_obj = IRObj::IRVar((
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                            SC_CONTEXT_STACK.with(|stack| {
                                stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                            }),
                        ));
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            KoopaOpCode::NE,
                            vec![left.clone(), IRObj::Const(0)],
                        ));
                        ir_obj
                    }
                    _ => left,
                };

                let converted_right = match &right {
                    IRObj::ReturnVal { .. } | IRObj::IRVar(_) => {
                        let ir_obj = IRObj::IRVar((
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                            SC_CONTEXT_STACK.with(|stack| {
                                stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                            }),
                        ));
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            KoopaOpCode::NE,
                            vec![right.clone(), IRObj::Const(0)],
                        ));
                        ir_obj
                    }
                    _ => right,
                };

                let koopa_op = match lor_op {
                    LOrOp::Or => KoopaOpCode::OR,
                };

                let ir_obj = IRObj::IRVar((
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                    SC_CONTEXT_STACK
                        .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
                ));
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![converted_left, converted_right],
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

    fn pre_parse(&self) -> IRObj {
        match self {
            LOrExp::LAndExp { land_exp } => land_exp.pre_parse(),

            LOrExp::LOrExp {
                lor_exp,
                lor_op: _,
                land_exp,
            } => {
                let left = lor_exp.pre_parse();
                // perform short-circuit evaluation for logical OR
                match &left {
                    IRObj::Const(v) if *v != 0 => return IRObj::Const(1),
                    _ => {}
                }

                let right = land_exp.pre_parse();
                match &right {
                    IRObj::Const(v) if *v != 0 => return IRObj::Const(1),
                    _ => {}
                }

                // constant folding
                if let (IRObj::Const(left_val), IRObj::Const(right_val)) =
                    (left.clone(), right.clone())
                {
                    return IRObj::Const(if left_val != 0 || right_val != 0 {
                        1
                    } else {
                        0
                    });
                }

                IRObj::IRVar((0, 0))
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
                match &left {
                    IRObj::None => panic!("Cannot use void type as an operand"),
                    IRObj::Const(v) if *v == 0 => {
                        println!(
                            "Short-circuiting logical AND: left operand is zero constant: {v}"
                        );
                        return IRObj::Const(0);
                    }
                    IRObj::ReturnVal {
                        return_val,
                        ir_var_id,
                        ..
                    } if matches!(*return_val, ReturnVal::AlwaysZero) => {
                        println!("Short-circuiting logical AND: left operand is always zero return value: {:#?}, ir_var_id: {:?}", return_val, ir_var_id);
                        return IRObj::Const(0);
                    }
                    _ => {}
                }

                let right = eq_exp.parse_var_exp();
                match &right {
                    IRObj::None => panic!("Cannot use void type as an operand"),
                    IRObj::Const(v) if *v == 0 => return IRObj::Const(0),
                    IRObj::ReturnVal { return_val, .. }
                        if matches!(*return_val, ReturnVal::AlwaysZero) =>
                    {
                        return IRObj::Const(0)
                    }
                    _ => {}
                }

                match (left.clone(), right.clone()) {
                    (IRObj::Const(left_val), IRObj::Const(right_val)) => {
                        return IRObj::Const((left_val != 0 && right_val != 0).into());
                    }
                    (
                        IRObj::ReturnVal {
                            return_val: left_return_val,
                            ..
                        },
                        IRObj::ReturnVal {
                            return_val: right_return_val,
                            ..
                        },
                    ) => {
                        return IRObj::Const(
                            (matches!(left_return_val, ReturnVal::AlwaysNonZero)
                                && matches!(right_return_val, ReturnVal::AlwaysNonZero))
                            .into(),
                        );
                    }
                    _ => {}
                }

                let koopa_op = match land_op {
                    LAndOp::And => KoopaOpCode::AND,
                };

                let converted_left = match &left {
                    IRObj::ReturnVal { .. } | IRObj::IRVar(_) => {
                        let ir_obj = IRObj::IRVar((
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                            SC_CONTEXT_STACK.with(|stack| {
                                stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                            }),
                        ));
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            KoopaOpCode::NE,
                            vec![left.clone(), IRObj::Const(0)],
                        ));
                        ir_obj
                    }
                    _ => left,
                };

                let converted_right = match &right {
                    IRObj::ReturnVal { .. } | IRObj::IRVar(_) => {
                        let ir_obj = IRObj::IRVar((
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                            SC_CONTEXT_STACK.with(|stack| {
                                stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                            }),
                        ));
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            KoopaOpCode::NE,
                            vec![right.clone(), IRObj::Const(0)],
                        ));
                        ir_obj
                    }
                    _ => right,
                };

                let ir_obj = IRObj::IRVar((
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                    SC_CONTEXT_STACK
                        .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
                ));
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![converted_left, converted_right],
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

    fn pre_parse(&self) -> IRObj {
        match self {
            LAndExp::EqExp { eq_exp } => eq_exp.pre_parse(),
            LAndExp::LAndExp {
                land_exp,
                land_op: _,
                eq_exp,
            } => {
                let left = land_exp.pre_parse();
                // short-circuit for logical AND
                match &left {
                    IRObj::Const(v) if *v == 0 => return IRObj::Const(0),
                    _ => {}
                }

                let right = eq_exp.pre_parse();
                match &right {
                    IRObj::Const(v) if *v == 0 => return IRObj::Const(0),
                    _ => {}
                }

                // constant folding
                if let (IRObj::Const(l), IRObj::Const(r)) = (left.clone(), right.clone()) {
                    return IRObj::Const(if l != 0 && r != 0 { 1 } else { 0 });
                }

                IRObj::IRVar((0, 0))
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
            EqExp::RelExp { rel_exp } => rel_exp.parse_var_exp(),
            EqExp::EqExp {
                eq_exp,
                eq_op,
                rel_exp,
            } => {
                let left = eq_exp.parse_var_exp();
                if matches!(left, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }
                let right = rel_exp.parse_var_exp();
                if matches!(right, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }

                if let (IRObj::Const(left_val), IRObj::Const(right_val)) =
                    (left.clone(), right.clone())
                {
                    let res = match eq_op {
                        EqOp::Eq => left_val == right_val,
                        EqOp::Ne => left_val != right_val,
                    };
                    return IRObj::Const(if res { 1 } else { 0 });
                }

                let koopa_op = match eq_op {
                    EqOp::Eq => KoopaOpCode::EQ,
                    EqOp::Ne => KoopaOpCode::NE,
                };

                let ir_obj = IRObj::IRVar((
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                    SC_CONTEXT_STACK
                        .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
                ));
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![left, right],
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

    fn pre_parse(&self) -> IRObj {
        match self {
            EqExp::RelExp { rel_exp } => rel_exp.pre_parse(),
            EqExp::EqExp {
                eq_exp,
                eq_op,
                rel_exp,
            } => {
                let left = eq_exp.pre_parse();
                let right = rel_exp.pre_parse();

                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match eq_op {
                            EqOp::Eq => l == r,
                            EqOp::Ne => l != r,
                        };
                        IRObj::Const(if res { 1 } else { 0 })
                    }
                    _ => IRObj::IRVar((0, 0)),
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
            RelExp::AddExp { add_exp } => add_exp.parse_var_exp(),
            RelExp::RelExp {
                rel_exp,
                rel_op,
                add_exp,
            } => {
                let left = rel_exp.parse_var_exp();
                if matches!(left, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }
                let right = add_exp.parse_var_exp();
                if matches!(right, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }

                if let (IRObj::Const(left_val), IRObj::Const(right_val)) =
                    (left.clone(), right.clone())
                {
                    let res = match rel_op {
                        RelOp::Lt => left_val < right_val,
                        RelOp::Gt => left_val > right_val,
                        RelOp::Le => left_val <= right_val,
                        RelOp::Ge => left_val >= right_val,
                    };
                    return IRObj::Const(if res { 1 } else { 0 });
                }

                let koopa_op = match rel_op {
                    RelOp::Lt => KoopaOpCode::LT,
                    RelOp::Gt => KoopaOpCode::GT,
                    RelOp::Le => KoopaOpCode::LE,
                    RelOp::Ge => KoopaOpCode::GE,
                };

                let ir_obj = IRObj::IRVar((
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                    SC_CONTEXT_STACK
                        .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
                ));
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![left, right],
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

    fn pre_parse(&self) -> IRObj {
        match self {
            RelExp::AddExp { add_exp } => add_exp.pre_parse(),
            RelExp::RelExp {
                rel_exp,
                rel_op,
                add_exp,
            } => {
                let left = rel_exp.pre_parse();
                let right = add_exp.pre_parse();

                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match rel_op {
                            RelOp::Lt => l < r,
                            RelOp::Gt => l > r,
                            RelOp::Le => l <= r,
                            RelOp::Ge => l >= r,
                        };
                        IRObj::Const(if res { 1 } else { 0 })
                    }
                    _ => IRObj::IRVar((0, 0)),
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
    FunctionCall {
        ident: String,
        args: Vec<Exp>,
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
            UnaryExp::PrimaryExp { exp } => exp.parse_var_exp(),

            UnaryExp::FunctionCall { ident, args } => {
                let func_def =
                    SC_CONTEXT_STACK.with(|stack| stack.borrow().get_func_def(ident.clone()));
                let (params, func_type) = (func_def.params, func_def.func_type);

                if args.len() != params.len() {
                    panic!(
                        "Arguments number mismatch in function call: expected {}, got {}",
                        params.len(),
                        args.len()
                    );
                }

                if params
                    .iter()
                    .any(|param| !matches!(param.param_type, BType::Int))
                {
                    panic!("Argument type mismatch occurred");
                }

                let ir_args = args
                    .iter()
                    .map(|arg| arg.parse_var_exp())
                    .collect::<Vec<IRObj>>();

                let ir_obj = match func_type {
                    BType::Void => IRObj::None,
                    _ => IRObj::ReturnVal {
                        ir_var_id: IR_VAR_ID_ALLOCATOR
                            .with(|allocator| allocator.borrow_mut().alloc()),
                        inst_id: SC_CONTEXT_STACK.with(|stack| {
                            stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                        }),
                        return_val: SC_CONTEXT_STACK.with(|stack| {
                            stack
                                .borrow()
                                .get_func_def(ident.clone())
                                .return_val
                                .borrow()
                                .clone()
                                .unwrap()
                        }),
                    },
                };

                insert_ir(InstData::new(
                    match ir_obj {
                        IRObj::None => BType::Void,
                        _ => BType::Int,
                    },
                    ir_obj.clone(),
                    KoopaOpCode::CALL,
                    vec![IRObj::FuncSym(ident.clone()), IRObj::Args(ir_args)],
                ));
                ir_obj
            }

            // handle unary operation
            UnaryExp::UnaryExp {
                unary_op,
                unary_exp,
            } => {
                let parse_result = unary_exp.parse_var_exp();

                if let IRObj::Const(res) = parse_result {
                    return match unary_op {
                        UnaryOp::Plus => IRObj::Const(res),
                        UnaryOp::Minus => IRObj::Const(-res),
                        UnaryOp::Not => IRObj::Const(if res == 0 { 1 } else { 0 }),
                    };
                }

                match unary_op {
                    UnaryOp::Plus => parse_result,
                    UnaryOp::Minus | UnaryOp::Not => {
                        let ir_obj = IRObj::IRVar((
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                            SC_CONTEXT_STACK.with(|stack| {
                                stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                            }),
                        ));
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            match unary_op {
                                UnaryOp::Minus => KoopaOpCode::SUB,
                                UnaryOp::Not => KoopaOpCode::EQ,
                                _ => unreachable!(),
                            },
                            vec![IRObj::Const(0), parse_result],
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

            // const exp couldn't contain function calls
            UnaryExp::FunctionCall { ident, args } => {
                panic!(
                    "Function call {} with arguments {:?} cannot be evaluated as a constant expression",
                    ident, args
                );
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

    fn pre_parse(&self) -> IRObj {
        match self {
            UnaryExp::PrimaryExp { exp } => exp.pre_parse(),
            // TODO: this one isn't very accurate
            UnaryExp::FunctionCall { ident: _, args: _ } => IRObj::IRVar((0, 0)),
            UnaryExp::UnaryExp {
                unary_op,
                unary_exp,
            } => {
                let inner = unary_exp.pre_parse();
                match inner {
                    IRObj::Const(v) => match unary_op {
                        UnaryOp::Plus => IRObj::Const(v),
                        UnaryOp::Minus => IRObj::Const(-v),
                        UnaryOp::Not => IRObj::Const(if v == 0 { 1 } else { 0 }),
                    },
                    _ => IRObj::IRVar((0, 0)),
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
            MulExp::UnaryExp { unary_exp } => unary_exp.parse_var_exp(),
            MulExp::MulExp {
                mul_exp,
                mul_op,
                unary_exp,
            } => {
                let left = mul_exp.parse_var_exp();
                if matches!(left, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }
                let right = unary_exp.parse_var_exp();
                if matches!(right, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }

                if let (IRObj::Const(left_val), IRObj::Const(right_val)) =
                    (left.clone(), right.clone())
                {
                    let res = match mul_op {
                        MulOp::Mul => left_val * right_val,
                        MulOp::Div => left_val / right_val,
                        MulOp::Mod => left_val % right_val,
                    };
                    return IRObj::Const(res);
                }

                let koopa_op = match mul_op {
                    MulOp::Mul => KoopaOpCode::MUL,
                    MulOp::Div => KoopaOpCode::DIV,
                    MulOp::Mod => KoopaOpCode::MOD,
                };

                let ir_obj = IRObj::IRVar((
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                    SC_CONTEXT_STACK
                        .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
                ));
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![left, right],
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

    fn pre_parse(&self) -> IRObj {
        match self {
            MulExp::UnaryExp { unary_exp } => unary_exp.pre_parse(),
            MulExp::MulExp {
                mul_exp,
                mul_op,
                unary_exp,
            } => {
                let left = mul_exp.pre_parse();
                let right = unary_exp.pre_parse();
                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match mul_op {
                            MulOp::Mul => l * r,
                            MulOp::Div => l / r,
                            MulOp::Mod => l % r,
                        };
                        IRObj::Const(res)
                    }
                    _ => IRObj::IRVar((0, 0)),
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
                if matches!(left, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }
                let right = mul_exp.parse_var_exp();
                if matches!(right, IRObj::None) {
                    panic!("Cannot use void type as an operand")
                }

                if let (IRObj::Const(left_val), IRObj::Const(right_val)) =
                    (left.clone(), right.clone())
                {
                    let res = match add_op {
                        AddOp::Add => left_val + right_val,
                        AddOp::Sub => left_val - right_val,
                    };
                    return IRObj::Const(res);
                }

                let koopa_op = match add_op {
                    AddOp::Add => KoopaOpCode::ADD,
                    AddOp::Sub => KoopaOpCode::SUB,
                };

                let ir_obj = IRObj::IRVar((
                    IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                    SC_CONTEXT_STACK
                        .with(|stack| stack.borrow().get_current_dfg().borrow().get_next_inst_id()),
                ));
                insert_ir(InstData::new(
                    BType::Int,
                    ir_obj.clone(),
                    koopa_op,
                    vec![left, right],
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

    fn pre_parse(&self) -> IRObj {
        match self {
            AddExp::MulExp { mul_exp } => mul_exp.pre_parse(),
            AddExp::AddExp {
                add_exp,
                add_op,
                mul_exp,
            } => {
                let left = add_exp.pre_parse();
                let right = mul_exp.pre_parse();
                match (&left, &right) {
                    (IRObj::Const(l), IRObj::Const(r)) => {
                        let res = match add_op {
                            AddOp::Add => l + r,
                            AddOp::Sub => l - r,
                        };
                        IRObj::Const(res)
                    }
                    _ => IRObj::IRVar((0, 0)),
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
                    Some(IRObj::ScVar {
                        initialized,
                        sc_var_id,
                    }) => {
                        // this case the variables wasn't loaded before use
                        if !initialized {
                            panic!("Variable {} used before initialization", l_val.ident);
                        }

                        // if it's a variable stored in memory, load first and return inst_id.
                        let ir_obj = IRObj::IRVar((
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                            SC_CONTEXT_STACK.with(|stack| {
                                stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                            }),
                        ));
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            KoopaOpCode::LOAD,
                            vec![IRObj::ScVar {
                                initialized,
                                sc_var_id,
                            }],
                        ));
                        ir_obj
                    }

                    Some(IRObj::Const(value)) => IRObj::Const(value),

                    Some(IRObj::GlobalVar {
                        initialized,
                        global_var_id,
                        init_val,
                    }) => {
                        // globa var has been initialized before use
                        let ir_obj = IRObj::IRVar((
                            IR_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc()),
                            SC_CONTEXT_STACK.with(|stack| {
                                stack.borrow().get_current_dfg().borrow().get_next_inst_id()
                            }),
                        ));
                        insert_ir(InstData::new(
                            BType::Int,
                            ir_obj.clone(),
                            KoopaOpCode::LOAD,
                            vec![IRObj::GlobalVar {
                                initialized,
                                global_var_id,
                                init_val,
                            }],
                        ));
                        ir_obj
                    }

                    _ => {
                        panic!(
                            "LVal {l_val} not found in var table, maybe the ident is not defined"
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
                    panic!("LVal {l_val} not found in const table, maybe the ident is for a variable or not defined");
                }
            }
        }
    }

    fn pre_parse(&self) -> IRObj {
        match self {
            PrimaryExp::Number { value } => IRObj::Const(*value),
            PrimaryExp::Exp { exp } => exp.pre_parse(),
            PrimaryExp::LVal { l_val } => {
                if let Some(value) =
                    SC_CONTEXT_STACK.with(|stack| stack.borrow().get_latest_const(&l_val.ident))
                {
                    value
                } else {
                    IRObj::IRVar((0, 0))
                }
            }
        }
    }
}
