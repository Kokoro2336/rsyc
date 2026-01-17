/**
 * Semantic analysis.
 * Performs type inference, add implicit cast and checks for semantic errors.
 */
use crate::base::pass::{Pass, SymbolTable};
use crate::base::r#type::Type;
use crate::frontend::ast::*;
use crate::utils::{cast, cast_mut, is, replace, take};

pub struct Semantic<'a> {
    pub syms: SymbolTable<String, Type>,
    pub node: Option<&'a mut Box<dyn Node>>,
}

impl<'a> Semantic<'a> {
    pub fn new(node: &'a mut Box<dyn Node>) -> Semantic<'a> {
        Semantic {
            syms: SymbolTable::new(),
            node: Some(node),
        }
    }

    pub fn analyze(&mut self, node: &mut Box<dyn Node>) -> Type {
        // Exps
        if is::<BinaryOp>(node) {
            let bin_op = cast_mut::<BinaryOp>(node).unwrap();
            let lhs_type = self.analyze(&mut bin_op.lhs);
            let rhs_type = self.analyze(&mut bin_op.rhs);

            // And & Or
            if matches!(lhs_type, Type::Float) {
                bin_op.lhs = Box::new(BinaryOp {
                    typ: Type::Int,
                    lhs: take(&mut bin_op.lhs),
                    op: Op::Ne,
                    rhs: Box::new(Literal::Float(0.0)),
                });
            }

            if matches!(rhs_type, Type::Float) {
                bin_op.rhs = Box::new(BinaryOp {
                    typ: Type::Int,
                    lhs: take(&mut bin_op.rhs),
                    op: Op::Ne,
                    rhs: Box::new(Literal::Float(0.0)),
                });
            }

            // Implicit cast
            if matches!(lhs_type, Type::Int) && matches!(rhs_type, Type::Float) {
                bin_op.lhs = Box::new(UnaryOp {
                    typ: Type::Float,
                    op: Op::Cast(Type::Int, Type::Float),
                    operand: take(&mut bin_op.lhs),
                });
            } else if matches!(lhs_type, Type::Float) && matches!(rhs_type, Type::Int) {
                bin_op.rhs = Box::new(UnaryOp {
                    typ: Type::Float,
                    op: Op::Cast(Type::Int, Type::Float),
                    operand: take(&mut bin_op.rhs),
                });
            }

            // return type
            // if the operation doesn't allow float return type, directly return Int
            if bin_op.op.only_ret_int() {
                bin_op.typ = Type::Int;
                Type::Int
            } else if matches!(lhs_type, Type::Float) || matches!(rhs_type, Type::Float) {
                bin_op.typ = Type::Float;
                Type::Float
            } else {
                bin_op.typ = Type::Int;
                Type::Int
            }
        } else if is::<UnaryOp>(node) {
            let un_op = cast_mut::<UnaryOp>(node).unwrap();
            if matches!(un_op.op, Op::Cast(_, _)) {
                panic!("Cast op is impossible to occur before semantic analysis!");
            }
            let operand_type = self.analyze(&mut un_op.operand);

            // Insert Ne for float
            match un_op.op {
                Op::Plus => {
                    // remove the plus node
                    let operand = take(&mut un_op.operand);
                    replace(node, operand);
                    operand_type
                }
                Op::Minus => {
                    // Minus always returns the same type as operand
                    operand_type
                }
                Op::Not => {
                    // Insert Ne for float
                    if matches!(operand_type, Type::Float) {
                        un_op.operand = Box::new(BinaryOp {
                            typ: Type::Int,
                            lhs: take(&mut un_op.operand),
                            op: Op::Ne,
                            rhs: Box::new(Literal::Float(0.0)),
                        });
                    }
                    Type::Int
                }
                _ => unreachable!("Unexpected unary op: {:?}", un_op),
            }
        } else if is::<Literal>(node) {
            let lit = cast_mut::<Literal>(node).unwrap();
            match lit {
                Literal::Int(_) => Type::Int,
                Literal::Float(_) => Type::Float,
            }
        } else if is::<VarAccess>(node) {
            let var_access = cast_mut::<VarAccess>(node).unwrap();
            if let Some(var_type) = self.syms.get(&var_access.name) {
                var_access.typ = var_type.clone();
                var_type.clone()
            } else {
                panic!("Undefined variable: {}", var_access.name);
            }
        } else if is::<Call>(node) {
            let call = cast_mut::<Call>(node).unwrap();
            if let Some(func_type) = self.syms.get(&call.func_name) {
                call.typ = func_type.clone();
                func_type.clone()
            } else {
                panic!("Undefined function: {}", call.func_name);
            }
        } else if is::<ArrayAccess>(node) {
            let array_access = cast_mut::<ArrayAccess>(node).unwrap();
            let array_type = self.syms.get(&array_access.name).unwrap_or_else(|| {
                panic!("Undefined array variable: {}", array_access.name);
            });

            // infer the access's typ
            array_access.typ = if let Type::Array { base, dims } = array_type {
                let new_dims = dims.clone()[array_access.indices.len()..].to_vec();
                // if new_dims is empty, return base type; else decay it to pointer.
                if new_dims.is_empty() {
                    base.as_ref().clone()
                } else {
                    decay(Type::Array {
                        base: base.clone(),
                        dims: new_dims,
                    })
                }
            } else {
                panic!(
                    "Variable {} is not an array, cannot access with indices",
                    array_access.name
                );
            };
            array_access.typ.clone()

        // Declarations
        } else if is::<Function>(node) {
            let func = cast_mut::<Function>(node).unwrap();
            self.syms
                .insert(func.name.clone(), func.return_type.clone());
            self.analyze(&mut func.body);
            self.syms.exit_scope();
            Type::Void
        } else if is::<VarDecl>(node) {
            let var_decl = cast_mut::<VarDecl>(node).unwrap();
            self.syms
                .insert(var_decl.name.clone(), var_decl.typ.clone());
            if let Some(init_value) = &mut var_decl.init_value {
                let val_typ = self.analyze(init_value);
                // if val_typ does not match the decl's typ, insert implicit cast
                if val_typ != var_decl.typ {
                    var_decl.init_value = Some(Box::new(UnaryOp {
                        typ: var_decl.typ.clone(),
                        op: Op::Cast(val_typ, var_decl.typ.clone()),
                        operand: take(init_value),
                    }));
                }
            }
            Type::Void
        } else if is::<ConstArray>(node) {
            let const_array = cast_mut::<ConstArray>(node).unwrap();
            self.syms
                .insert(const_array.name.clone(), const_array.typ.clone());
            let base = match &const_array.typ {
                Type::Array { base, .. } => base.as_ref().clone(),
                _ => panic!("ConstArray must have array type!"),
            };

            for init_val in &mut const_array.init_values {
                let val_type = self.analyze(init_val);
                // We don't insert cast node for ConstArray, we directly modify the init_val node.
                if val_type != base {
                    let literal = cast_mut::<Literal>(init_val).unwrap();
                    match val_type {
                        // if val_typ == Type::Int and val_typ != base, then base must be Float
                        Type::Int => {
                            *literal = Literal::Float(literal.get_int() as f32);
                        }
                        Type::Float => {
                            *literal = Literal::Float(literal.get_float());
                        }
                        _ => unreachable!(
                            "ConstArray can only be initialized with Int or Float literals: {:?}",
                            val_type
                        ),
                    }
                }
            }
            Type::Void
        } else if is::<LocalArray>(node) {
            let local_array = cast_mut::<LocalArray>(node).unwrap();
            self.syms
                .insert(local_array.name.clone(), local_array.typ.clone());
            if let Some(init_values) = &mut local_array.init_values {
                for init_val in init_values {
                    let val_typ = self.analyze(init_val);
                    // since we don't know whether the init_values is float or int, we insert cast node here.
                    if val_typ != local_array.typ {
                        match val_typ {
                            Type::Int => {
                                *init_val = Box::new(UnaryOp {
                                    typ: local_array.typ.clone(),
                                    op: Op::Cast(Type::Int, local_array.typ.clone()),
                                    operand: take(init_val),
                                });
                            },
                            Type::Float => {
                                *init_val = Box::new(UnaryOp {
                                    typ: local_array.typ.clone(),
                                    op: Op::Cast(Type::Float, local_array.typ.clone()),
                                    operand: take(init_val),
                                });
                            },
                            _ => unreachable!(
                                "LocalArray can only be initialized with Int or Float literals: {:?}", val_typ
                            ),
                        }
                    }
                }
            }
            Type::Void

        // Statements
        } else if is::<Block>(node) {
            let block = cast_mut::<Block>(node).unwrap();
            self.syms.enter_scope();
            for stmt in &mut block.statements {
                self.analyze(stmt);
            }
            self.syms.exit_scope();
            Type::Void
        } else if is::<If>(node) {
            let if_stmt = cast_mut::<If>(node).unwrap();
            // Do implicit cast for cond if necessary.
            let cond_type = self.analyze(&mut if_stmt.condition);
            if matches!(cond_type, Type::Float) {
                if_stmt.condition = Box::new(BinaryOp {
                    typ: Type::Int,
                    lhs: take(&mut if_stmt.condition),
                    op: Op::Ne,
                    rhs: Box::new(Literal::Float(0.0)),
                });
            }

            self.analyze(&mut if_stmt.then_block);
            if let Some(else_branch) = &mut if_stmt.else_block {
                self.analyze(else_branch);
            }
            Type::Void
        } else if is::<While>(node) {
            let while_stmt = cast_mut::<While>(node).unwrap();
            let cond_type = self.analyze(&mut while_stmt.condition);
            if matches!(cond_type, Type::Float) {
                while_stmt.condition = Box::new(BinaryOp {
                    typ: Type::Int,
                    lhs: take(&mut while_stmt.condition),
                    op: Op::Ne,
                    rhs: Box::new(Literal::Float(0.0)),
                });
            }

            self.analyze(&mut while_stmt.body);
            Type::Void
        } else if is::<Return>(node) {
            let ret = cast_mut::<Return>(node).unwrap();
            if let Some(expr) = &mut ret.0 {
                self.analyze(expr);
            }
            Type::Void
        } else if is::<Assign>(node) {
            let assign = cast_mut::<Assign>(node).unwrap();
            let lhs_type = self.analyze(&mut assign.lhs);
            let rhs_type = self.analyze(&mut assign.rhs);
            // insert implicit cast if necessary
            if lhs_type != rhs_type {
                assign.rhs = Box::new(UnaryOp {
                    typ: lhs_type.clone(),
                    op: Op::Cast(rhs_type, lhs_type),
                    operand: take(&mut assign.rhs),
                });
            }

            Type::Void
        } else {
            Type::Void
        }
    }
}

impl<'a> Pass for Semantic<'a> {
    fn run(&mut self) {
        let node = self.node.take().unwrap();
        self.analyze(node);
    }
}

// Pointer -> Array
fn raise(typ: Type) -> Type {
    match typ {
        Type::Pointer { base } => match *base {
            Type::Array {
                base: arr_base,
                dims,
            } => Type::Array {
                base: arr_base,
                dims: {
                    let mut vec = vec![1];
                    vec.extend(dims);
                    vec
                },
            },
            _ => Type::Array {
                base,
                dims: vec![1],
            },
        },
        _ => panic!("Cannot raise non-pointer type: {:?}", typ),
    }
}

// Array -> Pointer
fn decay(typ: Type) -> Type {
    match typ {
        Type::Array { base, dims } => {
            if dims.len() == 0 {
                panic!("Cannot decay array with zero dimensions!");
            }
            if dims.len() == 1 {
                Type::Pointer { base }
            } else {
                Type::Pointer {
                    base: Box::new(Type::Array {
                        base,
                        dims: dims[1..].to_vec(),
                    }),
                }
            }
        }
        _ => panic!("Cannot decay non-array type: {:?}", typ),
    }
}
