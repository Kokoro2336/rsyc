use crate::base::r#type::Type;
use crate::utils::{cast, cast_deref, is};
use crate::frontend::ast::*;

use std::cell::RefCell;
use std::rc::Rc;
use tool::fix;

/**
 * A module which provides a variety of utilities for parsing.
 */
use crate::frontend::ast::{BinaryOp, Literal, Node, Op, UnaryOp};

// early constant folding optimization
pub fn fold(node: &Box<dyn Node>) -> Box<dyn Node> {
    if is::<BinaryOp>(node) {
        let bin_op = cast::<BinaryOp>(&*node).unwrap();
        let lhs = fold(&bin_op.lhs);
        let rhs = fold(&bin_op.rhs);

        if is::<Literal>(&*lhs) && is::<Literal>(&*rhs) {
            let lhs_lit = cast::<Literal>(&*lhs).unwrap();
            let rhs_lit = cast::<Literal>(&*rhs).unwrap();

            match (lhs_lit, rhs_lit) {
                (Literal::Int(lhs_val), Literal::Int(rhs_val)) => {
                    let result = match bin_op.op {
                        Op::Add => lhs_val + rhs_val,
                        Op::Sub => lhs_val - rhs_val,
                        Op::Mul => lhs_val * rhs_val,
                        Op::Div => lhs_val / rhs_val,
                        Op::Mod => lhs_val % rhs_val,
                        _ => panic!("Unsupported operation for constant folding: {:?}", bin_op),
                    };
                    Box::new(Literal::Int(result))
                }
                (Literal::Float(lhs_val), Literal::Float(rhs_val)) => {
                    let result = match bin_op.op {
                        Op::Add => lhs_val + rhs_val,
                        Op::Sub => lhs_val - rhs_val,
                        Op::Mul => lhs_val * rhs_val,
                        Op::Div => lhs_val / rhs_val,
                        _ => panic!("Unsupported operation for constant folding: {:?}", bin_op),
                    };
                    Box::new(Literal::Float(result))
                }
                (Literal::Float(lhs_val), Literal::Int(rhs_val)) => {
                    let result = match bin_op.op {
                        Op::Add => lhs_val + *rhs_val as f32,
                        Op::Sub => lhs_val - *rhs_val as f32,
                        Op::Mul => lhs_val * *rhs_val as f32,
                        Op::Div => lhs_val / *rhs_val as f32,
                        _ => panic!("Unsupported operation for constant folding: {:?}", bin_op),
                    };
                    Box::new(Literal::Float(result))
                }
                (Literal::Int(lhs_val), Literal::Float(rhs_val)) => {
                    let result = match bin_op.op {
                        Op::Add => *lhs_val as f32 + rhs_val,
                        Op::Sub => *lhs_val as f32 - rhs_val,
                        Op::Mul => *lhs_val as f32 * rhs_val,
                        Op::Div => *lhs_val as f32 / rhs_val,
                        _ => panic!("Unsupported operation for constant folding: {:?}", bin_op),
                    };
                    Box::new(Literal::Float(result))
                }
            }
        } else {
            panic!("Non-constant folding operation: {:?}", node);
        }
    } else if is::<UnaryOp>(node) {
        let un_op = cast::<UnaryOp>(&*node).unwrap();
        let operand = fold(&un_op.operand);

        if is::<Literal>(&*operand) {
            let lit = cast::<Literal>(&*operand).unwrap();
            match lit {
                Literal::Int(val) => {
                    let result = match un_op.op {
                        Op::Plus => *val,
                        Op::Minus => -val,
                        _ => panic!(
                            "Unsupported unary operation for constant folding: {:?}",
                            un_op
                        ),
                    };
                    Box::new(Literal::Int(result))
                }
                Literal::Float(val) => {
                    let result = match un_op.op {
                        Op::Plus => *val,
                        Op::Minus => -val,
                        _ => panic!(
                            "Unsupported unary operation for constant folding: {:?}",
                            un_op
                        ),
                    };
                    Box::new(Literal::Float(result))
                }
            }
        } else {
            panic!("Non-constant folding unary operation: {:?}", node);
        }
    } else if is::<Literal>(node) {
        let lit_node = cast::<Literal>(&*node).unwrap();
        match lit_node {
            Literal::Int(val) => Box::new(Literal::Int(*val)),
            Literal::Float(val) => Box::new(Literal::Float(*val)),
        }
    } else {
        panic!("Unsupported node type for constant folding: {:?}", node);
    }
}

// 1. Unfold the RawDecls into separate declarations.
// 2. Flatten the array.
pub fn canonicalize(mut node: RawDecl) -> Vec<Box<dyn Node>> {
    let aggr_typ = node.typ.clone();
    let mut new_nodes: Vec<Box<dyn Node>> = vec![];

    for raw_decl in node.raw_decls.into_iter() {
        if raw_decl.const_exps.is_empty() {
            // Dispatch constant and non-constant here.
            new_nodes.push(Box::new(VarDecl {
                name: raw_decl.ident,
                typ: aggr_typ.clone(),
                mutable: node.mutable,
                init_value: raw_decl.init_val,
            }));
        } else {
            let size = raw_decl.const_exps.iter().product::<u32>();
            let const_exps = raw_decl.const_exps.clone();
            // Flatten & Dispatch
            if node.mutable {
                // We don't know whether the init_values is float or int, so we still wrap it with Node.
                new_nodes.push(Box::new(ConstArray {
                    name: raw_decl.ident,
                    typ: Type::Array {
                        base: Box::new(aggr_typ.clone()),
                        dims: raw_decl.const_exps,
                    },
                    init_values: flatten(
                        aggr_typ.clone(),
                        const_exps,
                        raw_decl
                            .init_val
                            .expect("ConstArray should have an initial value!"),
                    ),
                }));
            } else {
                new_nodes.push(Box::new(LocalArray {
                    name: raw_decl.ident,
                    typ: Type::Array {
                        base: Box::new(aggr_typ.clone()),
                        dims: raw_decl.const_exps,
                    },
                    init_values: if raw_decl.init_val.is_none() {
                        None
                    } else {
                        Some(flatten(
                            aggr_typ.clone(),
                            const_exps,
                            raw_decl.init_val.unwrap(),
                        ))
                    },
                }));
            }
        }
    }

    new_nodes
}

fn flatten<'a>(base_typ: Type, indices: Vec<u32>, node: Box<dyn Node>) -> Vec<Box<dyn Node>> {
    if !is::<ArrayInitVal>(&*node) {
        panic!("flatten can only process ArrayInitVal nodes");
    }
    let new_vals: RefCell<Vec<Box<dyn Node>>> = RefCell::new(vec![]);

    {
        // flatten origin array
        let rec = fix(|f, params: (Box<dyn Node>, u32)| -> u32 {
            let (val, depth) = params;
            let mut filled_size: u32 = 0;

            // find minimal depth of current array.
            if is::<ArrayInitVal>(&val) {
                if new_vals.borrow().len() as u32 % indices.last().unwrap() != 0 {
                    panic!("Array has insufficient initializers");
                }

                let array_init_val = *cast_deref::<ArrayInitVal>(val).unwrap();
                let vals = array_init_val.init_vals;
                let sub_filled_size = vals.into_iter().fold(filled_size, |filled_size, val| {
                    filled_size + f((val, depth + 1))
                });

                filled_size += sub_filled_size;

                let to_be_filled = indices[depth as usize..indices.len()]
                    .iter()
                    .fold(1, |acc, index| acc * (*index as usize))
                    as u32
                    - filled_size;

                // fill 0
                (0..to_be_filled).for_each(|_| {
                    new_vals.borrow_mut().push(match base_typ.clone() {
                        Type::Int => Box::new(Literal::Int(0)),
                        Type::Float => Box::new(Literal::Float(0.0)),
                        _ => unreachable!(
                            "Only Int and Float types are supported in array initialization"
                        ),
                    });
                });

                filled_size += to_be_filled;
            } else {
                new_vals.borrow_mut().push(val);
                filled_size += 1;
            }

            filled_size
        });

        rec((node, 0));
    }

    let expected_size = indices.iter().fold(1, |acc, index| acc * (*index as usize));
    if new_vals.borrow().len() != expected_size {
        panic!(
            "Array has insufficient initializers: expected {}, found {}",
            expected_size,
            new_vals.borrow().len()
        );
    }

    new_vals.into_inner()
}
