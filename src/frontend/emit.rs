use crate::base::ir;
use crate::base::ir::*;
use crate::base::{Builder, BuilderContext, LoopInfo};
/**
 * Original IR generation.
 */
use crate::base::{Pass, SymbolTable, Type};
use crate::frontend::ast::*;
use crate::frontend::semantic::decay;
use crate::utils::cast;

use std::collections::HashMap;

pub struct Emit<'a> {
    root: &'a Box<dyn Node>,
    builder: Builder,
    program: Program,

    // This time, for the convenience of recongizing global vars, we store a separate table for them.
    globals: HashMap<String, GlobalId>,
    // symbol -> OpId(for Alloca)
    syms: SymbolTable<String, OpId>,
    // we store the idx of current function
    current_function: Option<usize>,
    // original name -> promoted name
    mangled: HashMap<String, String>,

    // counters
    counter: u32,
    // for naming string literals
    str_counter: u32,
}

impl<'a> Emit<'a> {
    pub fn new(builder: Builder, root: &'a Box<dyn Node>) -> Self {
        Self {
            builder,
            syms: SymbolTable::new(),
            globals: HashMap::new(),
            root,
            program: Program::new(),
            current_function: None,
            mangled: HashMap::new(),
            counter: 0,
            str_counter: 0,
        }
    }

    pub fn emit(&mut self, node: &Box<dyn Node>) -> Result<Option<OpId>, String> {
        let builder = &mut self.builder;

        if let Some(fn_decl) = cast::<FnDecl>(node) {
            // create new function
            self.program.funcs.push(Function::new(fn_decl.name.clone()));
            self.current_function = Some(self.program.funcs.len() - 1);
            let func = &mut self.program.funcs[self.current_function.unwrap()];

            // get and store arguments
            // shadow the ctx to use function's cfg/dfg
            // in other branches, the ctx is already function's ctx
            let ctx = &mut BuilderContext {
                cfg: Some(&mut func.cfg),
                dfg: Some(&mut func.dfg),
                globals: &mut self.program.globals,
            };
            //create new block for function
            let block_id = builder.create_new_block(ctx)?;
            builder.set_current_block(block_id);
            // enter function scope
            self.syms.enter_scope();

            for (i, arg) in fn_decl.params.iter().enumerate() {
                let get_arg = builder.create(
                    ctx,
                    ir::Op::new(
                        arg.1.clone(),
                        vec![Attr::Function(arg.0.clone()), Attr::Param(i as u32)],
                        OpData::GetArg,
                    ),
                )?;
                let alloca = builder.create(
                    ctx,
                    ir::Op::new(arg.1.clone(), vec![Attr::Param(i as u32)], OpData::Alloca),
                )?;
                builder.create(
                    ctx,
                    ir::Op::new(
                        Type::Void,
                        vec![],
                        OpData::Store {
                            addr: alloca,
                            value: get_arg,
                        },
                    ),
                )?;
            }

            // parse the function body
            self.emit(&fn_decl.body)?;
            // exit function scope
            self.syms.exit_scope();
            return Ok(None);
        } else if let Some(block) = cast::<Block>(node) {
            block
                .statements
                .iter()
                .try_for_each(|stmt| -> Result<(), String> {
                    self.emit(stmt)?;
                    Ok(())
                })?;
        } else if let Some(var_decl) = cast::<VarDecl>(node) {
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                &mut BuilderContext {
                    cfg: None,
                    dfg: None,
                    globals: &mut self.program.globals,
                }
            };

            // Global alloca
            if var_decl.is_global {
                // treat all the global vars as Array. Since use VarArray/ConstArray
                // needs extra handling in IR generation, we just use ConstArray here.
                let arr = ConstArray {
                    name: var_decl.name.clone(),
                    typ: var_decl.typ.clone(),
                    init_values: if let Some(init_val) = &var_decl.init_value {
                        // Theoretically, global var has already been constant folded or given an zero init in parsing phase.
                        vec![init_val.clone()]
                    } else {
                        // default init to zero
                        vec![Box::new(Literal::Int(0))]
                    },
                };

                let alloca = builder.create(
                    // use program.globals DFG
                    ctx,
                    ir::Op::new(
                        var_decl.typ.clone(),
                        vec![
                            Attr::Size(var_decl.typ.size_in_bytes()),
                            Attr::GlobalArray(arr),
                        ],
                        OpData::GlobalAlloca,
                    ),
                )?;
                // insert into globals table. If the var is global, then the symbol table must be at global scope.
                self.globals.insert(var_decl.name.clone(), GlobalId(alloca));
            } else {
                // Alloca
                let alloca = builder.create(
                    ctx,
                    ir::Op::new(
                        var_decl.typ.clone(),
                        vec![Attr::Size(var_decl.typ.size_in_bytes())],
                        OpData::Alloca,
                    ),
                )?;
                // insert into symbol table
                self.syms.insert(var_decl.name.clone(), alloca);
                // if has init value, create store
                if let Some(init_val) = &var_decl.init_value {
                    let op_id = self.emit(init_val)?;
                    // Re-acquire ctx after emit
                    let ctx = if let Some(func_idx) = self.current_function {
                        let func = &mut self.program.funcs[func_idx];
                        &mut BuilderContext {
                            cfg: Some(&mut func.cfg),
                            dfg: Some(&mut func.dfg),
                            globals: &mut self.program.globals,
                        }
                    } else {
                        panic!("Local variable init outside function");
                    };

                    builder.create(
                        ctx,
                        ir::Op::new(
                            Type::Void,
                            vec![],
                            OpData::Store {
                                addr: alloca,
                                value: op_id.unwrap(),
                            },
                        ),
                    )?;
                }
                // If no init value, do nothing (the value is undefined)
            }
            return Ok(None);
        } else if let Some(var_array) = cast::<VarArray>(node) {
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                &mut BuilderContext {
                    cfg: None,
                    dfg: None,
                    globals: &mut self.program.globals,
                }
            };

            if var_array.is_global {
                // Global alloca
                // use ConstArray to represent all the global array, but it doesn't mean .
                let array = ConstArray {
                    name: var_array.name.clone(),
                    typ: var_array.typ.clone(),
                    init_values: if let Some(init_values) = &var_array.init_values {
                        init_values.clone()
                    } else {
                        // default init to zero
                        let len = match &var_array.typ {
                            Type::Array { dims, .. } => dims.iter().product::<u32>(),
                            _ => {
                                return Err("VarArray.typ is not Array".to_string());
                            }
                        };
                        vec![Box::new(Literal::Int(0)); len as usize]
                    },
                };

                let alloca = builder.create(
                    // use program.globals DFG
                    ctx,
                    ir::Op::new(
                        var_array.typ.clone(),
                        vec![
                            Attr::Size(var_array.typ.size_in_bytes()),
                            Attr::GlobalArray(array),
                        ],
                        OpData::GlobalAlloca,
                    ),
                )?;
                // insert into globals table. If the var is global, then the symbol table must because at global scope.
                self.globals
                    .insert(var_array.name.clone(), GlobalId(alloca));
            } else {
                let total_size = var_array.typ.size_in_bytes();
                let alloca = builder.create(
                    ctx,
                    ir::Op::new(
                        var_array.typ.clone(),
                        vec![Attr::Size(total_size)],
                        OpData::Alloca,
                    ),
                )?;
                // create another layer of indirection
                let ptr_typ = decay(var_array.typ.clone());
                let ptr_size = ptr_typ.size_in_bytes();
                let ptr_addr = builder.create(
                    ctx,
                    ir::Op::new(ptr_typ.clone(), vec![Attr::Size(ptr_size)], OpData::Alloca),
                )?;
                builder.create(
                    ctx,
                    ir::Op::new(
                        Type::Void,
                        vec![],
                        OpData::Store {
                            addr: ptr_addr,
                            value: alloca,
                        },
                    ),
                )?;
                // insert the pointer of the array into symbol table
                self.syms.insert(var_array.name.clone(), ptr_addr);
                // if has init values, create stores
                if let Some(init_values) = &var_array.init_values {
                    let base_size = match &var_array.typ {
                        Type::Array { base, .. } => base.size_in_bytes(),
                        _ => {
                            return Err("VarArray.typ is not Array".to_string());
                        }
                    };
                    let base_addr = builder.create(
                        ctx,
                        ir::Op::new(ptr_typ.clone(), vec![], OpData::Load { addr: ptr_addr }),
                    )?;
                    for (idx, init_val) in init_values.iter().enumerate() {
                        let op_id = self.emit(init_val)?;
                        // Re-acquire ctx
                        let ctx = if let Some(func_idx) = self.current_function {
                            let func = &mut self.program.funcs[func_idx];
                            &mut BuilderContext {
                                cfg: Some(&mut func.cfg),
                                dfg: Some(&mut func.dfg),
                                globals: &mut self.program.globals,
                            }
                        } else {
                            panic!("Local array init outside function");
                        };

                        // evaluate the address
                        let offset = builder.create(
                            ctx,
                            ir::Op::new(
                                Type::Int,
                                vec![Attr::Int((idx as u32 * base_size) as i32)],
                                OpData::Int,
                            ),
                        )?;
                        let addr = builder.create(
                            ctx,
                            ir::Op::new(
                                ptr_typ.clone(),
                                vec![],
                                OpData::AddI {
                                    lhs: base_addr,
                                    rhs: offset,
                                },
                            ),
                        )?;
                        // store
                        builder.create(
                            ctx,
                            ir::Op::new(
                                Type::Void,
                                vec![],
                                OpData::Store {
                                    addr,
                                    value: op_id.unwrap(),
                                },
                            ),
                        )?;
                    }
                    // TODO: when the trailing zeroes reach some kind of limit, we add a loop to init them.
                }
            }
        } else if let Some(const_array) = cast::<ConstArray>(node) {
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                &mut BuilderContext {
                    cfg: None,
                    dfg: None,
                    globals: &mut self.program.globals,
                }
            };

            let name = if let Some(current_func) = self.current_function {
                // promote the const array to global
                let func_name = self.program.funcs[current_func].name;
                // generate mangled name
                let mangled_name = format!(
                    "__const_{}_{}_{}",
                    func_name, const_array.name, self.counter
                );
                // update the counter
                self.counter += 1;
                // insert into mangled table
                self.mangled
                    .insert(const_array.name.clone(), mangled_name.clone());
                mangled_name
            } else {
                const_array.name.clone()
            };

            // Global alloca
            let alloca = builder.create(
                // use program.globals DFG
                ctx,
                ir::Op::new(
                    const_array.typ.clone(),
                    vec![
                        Attr::Size(const_array.typ.size_in_bytes()),
                        Attr::GlobalArray(const_array.clone()),
                    ],
                    OpData::GlobalAlloca,
                ),
            )?;
            // insert into globals table. If the var is global, then the symbol table must because at global scope.
            self.globals.insert(name, GlobalId(alloca));
        } else if let Some(ret) = cast::<Return>(node) {
            if let Some(ret_val) = &ret.0 {
                let op_id = self.emit(ret_val)?;
                let ctx = if let Some(func_idx) = self.current_function {
                    let func = &mut self.program.funcs[func_idx];
                    &mut BuilderContext {
                        cfg: Some(&mut func.cfg),
                        dfg: Some(&mut func.dfg),
                        globals: &mut self.program.globals,
                    }
                } else {
                    panic!("Return outside function");
                };

                builder.create(
                    ctx,
                    ir::Op::new(
                        Type::Void,
                        vec![],
                        OpData::Ret {
                            value: Some(op_id.unwrap()),
                        },
                    ),
                )?;
            } else {
                let ctx = if let Some(func_idx) = self.current_function {
                    let func = &mut self.program.funcs[func_idx];
                    &mut BuilderContext {
                        cfg: Some(&mut func.cfg),
                        dfg: Some(&mut func.dfg),
                        globals: &mut self.program.globals,
                    }
                } else {
                    panic!("Return outside function");
                };

                builder.create(
                    ctx,
                    ir::Op::new(Type::Void, vec![], OpData::Ret { value: None }),
                )?;
            }
        } else if let Some(if_stmt) = cast::<If>(node) {
            // evaluate the codition
            let cond_op = self.emit(&if_stmt.condition)?;
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                panic!("If statement outside function");
            };

            // create blocks
            let then_block = builder.create_new_block(ctx)?;
            let else_block = if if_stmt.else_block.is_some() {
                Some(builder.create_new_block(ctx)?)
            } else {
                None
            };
            let end_block = builder.create_new_block(ctx)?;

            // create branch instructions
            let mut attrs = vec![Attr::Branch {
                then_bb: then_block,
                else_bb: else_block,
            }];
            builder.create(
                ctx,
                ir::Op::new(
                    Type::Void,
                    attrs,
                    OpData::Br {
                        cond: cond_op.unwrap(),
                    },
                ),
            )?;

            // then block_id
            builder.set_current_block(then_block);
            self.emit(&if_stmt.then_block)?;
            
            // Re-acquire ctx
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                panic!("If statement outside function");
            };

            // Add jump to end_block if not terminated
            builder.create(
                ctx,
                ir::Op::new(Type::Void, vec![Attr::Jump(end_block)], OpData::Jump),
            )?;

            // else block_id
            if let Some(else_blk) = else_block {
                builder.set_current_block(else_blk);
                self.emit(&if_stmt.else_block.as_ref().unwrap())?;
                
                // Re-acquire ctx
                let ctx = if let Some(func_idx) = self.current_function {
                    let func = &mut self.program.funcs[func_idx];
                    &mut BuilderContext {
                        cfg: Some(&mut func.cfg),
                        dfg: Some(&mut func.dfg),
                        globals: &mut self.program.globals,
                    }
                } else {
                    panic!("If statement outside function");
                };

                // Add jump to end_block if not terminated
                builder.create(
                    ctx,
                    ir::Op::new(Type::Void, vec![Attr::Jump(end_block)], OpData::Jump),
                )?;
            }

            // set current block to end_block
            builder.set_current_block(end_block);
        } else if let Some(while_stmt) = cast::<While>(node) {
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                panic!("While statement outside function");
            };

            // create blocks
            let while_entry = builder.create_new_block(ctx)?;
            let while_body = builder.create_new_block(ctx)?;
            let while_end = builder.create_new_block(ctx)?;

            // jump to while_entry
            builder.create(
                ctx,
                ir::Op::new(Type::Void, vec![Attr::Jump(while_entry)], OpData::Jump),
            )?;

            // while_entry block
            builder.set_current_block(while_entry);
            let cond_op = self.emit(&while_stmt.condition)?;

             // Re-acquire ctx
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                panic!("While statement outside function");
            };

            // create branch instruction
            builder.create(
                ctx,
                ir::Op::new(
                    Type::Void,
                    vec![Attr::Branch {
                        then_bb: while_body,
                        else_bb: Some(while_end),
                    }],
                    OpData::Br {
                        cond: cond_op.unwrap(),
                    },
                ),
            )?;

            // while_body block
            builder.set_current_block(while_body);
            // push loop info
            builder.loop_stack.push(LoopInfo {
                while_entry: Some(while_entry),
                end_block: Some(while_end),
            });
            self.emit(&while_stmt.body)?;
            // pop loop info
            builder.loop_stack.pop();

             // Re-acquire ctx
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                panic!("While statement outside function");
            };

            // jump back to while_entry
            builder.create(
                ctx,
                ir::Op::new(Type::Void, vec![Attr::Jump(while_entry)], OpData::Jump),
            )?;

            // set current block to while_end
            builder.set_current_block(while_end);
        } else if let Some(break_stmt) = cast::<Break>(node) {
            let loop_info = builder
                .loop_stack
                .last()
                .ok_or("Break statement not inside a loop")?;
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                return Err("Break statement not inside a function".to_string());
            };

            builder.create(
                ctx,
                ir::Op::new(
                    Type::Void,
                    vec![Attr::Jump(loop_info.end_block.unwrap())],
                    OpData::Jump,
                ),
            )?;
        } else if let Some(continue_stmt) = cast::<Continue>(node) {
            let loop_info = builder
                .loop_stack
                .last()
                .ok_or("Continue statement not inside a loop")?;
            
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                return Err("Continue statement not inside a function".to_string());
            };

            builder.create(
                ctx,
                ir::Op::new(
                    Type::Void,
                    vec![Attr::Jump(loop_info.while_entry.unwrap())],
                    OpData::Jump,
                ),
            )?;
        } else if let Some(assign_stmt) = cast::<Assign>(node) {
        } else if let Some(var_access) = cast::<VarAccess>(node) {
        } else if let Some(array_access) = cast::<ArrayAccess>(node) {
            // emit indices first
            let mut index_ops = vec![];
            for index in &array_access.indices {
                let op = self.emit(index)?;
                index_ops.push(op.unwrap());
            }

            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                panic!("Array access outside function");
            };

            // find whether the array is global or local
            let array_name = &array_access.name;
            // the type has been decayed in semantic phase
            let ptr_typ = decay(array_access.typ.clone());

            let ptr_addr = if let Some(global_id) = self.globals.get(array_name) {
                // global array
                builder.create(
                    ctx,
                    ir::Op::new(ptr_typ.clone(), vec![], OpData::GetGlobal(*global_id)),
                )?
            } else if let Some(mangled_name) = self.mangled.get(array_name) {
                // promoted local const array
                let global_id = self.globals.get(mangled_name).ok_or(format!(
                    "ArrayAccess: promoted const array {} not found in globals",
                    mangled_name
                ))?;
                builder.create(
                    ctx,
                    ir::Op::new(ptr_typ.clone(), vec![], OpData::GetGlobal(*global_id)),
                )?
            } else {
                // normal local var array
                let ptr_addr = self.syms.get(array_name).ok_or(format!(
                    "ArrayAccess: local array {} not found in syms",
                    array_name
                ))?;
                builder.create(
                    ctx,
                    ir::Op::new(ptr_typ.clone(), vec![], OpData::Load { addr: *ptr_addr }),
                )?
            };

            // get the base type from the pointer address
            // this must be a pointer type
            let base_type = ctx.dfg.as_ref().unwrap().get(ptr_addr).unwrap().unwrap().typ.clone();
            
            // dereference the pointer type
            let mut curr_ty = match base_type {
                Type::Pointer { base } => *base,
                _ => return Err(format!("ArrayAccess: base type is not pointer: {:?}", base_type)),
            };

            let mut curr_addr = ptr_addr;

            for index_op in index_ops {
                // calculate stride
                let stride = curr_ty.size_in_bytes();
                let stride_op = builder.create(
                    ctx,
                    ir::Op::new(Type::Int, vec![Attr::Int(stride as i32)], OpData::Int),
                )?;

                // calculate offset
                let offset = builder.create(
                    ctx,
                    ir::Op::new(
                        Type::Int,
                        vec![],
                        OpData::MulI {
                            lhs: index_op,
                            rhs: stride_op,
                        },
                    ),
                )?;

                // update address
                // the new address points to the element (or sub-array)
                // so the type of new address should be Pointer(curr_ty)
                // But wait, curr_ty changes after indexing.
                // If curr_ty is [10 x [20 x int]], and we index it, we get a pointer to [20 x int].
                // The new address type is [20 x int]*.

                // update curr_ty for next iteration
                curr_ty = match curr_ty {
                    Type::Array { base, dims } => {
                        if dims.len() == 1 {
                            *base
                        } else {
                            Type::Array {
                                base,
                                dims: dims[1..].to_vec(),
                            }
                        }
                    }
                    Type::Pointer { base } => *base, // should not happen for standard arrays but for safety
                    _ => curr_ty, // e.g. Int, if we are indexing into a pointer to int?
                };

                let new_ptr_typ = Type::Pointer {
                    base: Box::new(curr_ty.clone()),
                };

                curr_addr = builder.create(
                    ctx,
                    ir::Op::new(
                        new_ptr_typ,
                        vec![],
                        OpData::AddI {
                            lhs: curr_addr,
                            rhs: offset,
                        },
                    ),
                )?;
            }

            // decide whether to load or return address
            match array_access.typ {
                Type::Int | Type::Float | Type::Char => {
                     let value = builder.create(
                        ctx,
                        ir::Op::new(array_access.typ.clone(), vec![], OpData::Load { addr: curr_addr }),
                    )?;
                    return Ok(Some(value));
                }
                _ => {
                    // return address for array/pointer types
                    return Ok(Some(curr_addr));
                }
            }

        } else if let Some(call) = cast::<Call>(node) {
            // evaluate arguments
            let mut arg_ops: Vec<OpId> = vec![];
            for arg in &call.args {
                let arg_op = self.emit(arg)?;
                arg_ops.push(arg_op.unwrap());
            }

            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                panic!("Call outside function");
            };

            // create call op_id
            let call_op = builder.create(
                ctx,
                ir::Op::new(
                    call.typ.clone(),
                    vec![Attr::Function(call.func_name.clone())],
                    OpData::Call { args: arg_ops },
                ),
            )?;
            return Ok(Some(call_op));
        } else if let Some(binary_op) = cast::<BinaryOp>(node) {
        } else if let Some(unary_op) = cast::<UnaryOp>(node) {
        } else if let Some(literal) = cast::<Literal>(node) {
            let ctx = if let Some(func_idx) = self.current_function {
                let func = &mut self.program.funcs[func_idx];
                &mut BuilderContext {
                    cfg: Some(&mut func.cfg),
                    dfg: Some(&mut func.dfg),
                    globals: &mut self.program.globals,
                }
            } else {
                &mut BuilderContext {
                    cfg: None,
                    dfg: None,
                    globals: &mut self.program.globals,
                }
            };

            match literal {
                Literal::Int(val) => {
                    let op_id = builder.create(
                        ctx,
                        ir::Op::new(Type::Int, vec![Attr::Int(*val)], OpData::Int),
                    )?;
                    return Ok(Some(op_id));
                }
                Literal::Float(val) => {
                    let op_id = builder.create(
                        ctx,
                        ir::Op::new(Type::Float, vec![Attr::Float(*val)], OpData::Float),
                    )?;
                    return Ok(Some(op_id));
                }
                Literal::String(string) => {
                    // create a global u8 ConstArray
                    let char_array = ConstArray {
                        // disposable, don't care about the name
                        name: "".to_string(),
                        typ: Type::Array {
                            base: Box::new(Type::Char),
                            dims: vec![(string.len() + 1) as u32],
                        },
                        init_values: string
                            .chars()
                            .map(|c| Box::new(Literal::Int(c as i32)) as Box<dyn Node>)
                            // This chain adds the null terminator
                            .chain(std::iter::once(Box::new(Literal::Int(0)) as Box<dyn Node>))
                            .collect(),
                    };
                    // update str_counter
                    self.str_counter += 1;
                    // create global alloca
                    let typ = char_array.typ.clone();
                    let global_alloca = builder.create(
                        // use program.globals DFG
                        ctx,
                        ir::Op::new(
                            char_array.typ.clone(),
                            vec![
                                Attr::Size(char_array.typ.size_in_bytes()),
                                Attr::GlobalArray(char_array),
                            ],
                            OpData::GlobalAlloca,
                        ),
                    )?;
                    // get the pointer right now
                    let ptr_typ = decay(typ);
                    // return the pointer OpId
                    let ptr_addr = builder.create(
                        ctx,
                        ir::Op::new(
                            ptr_typ.clone(),
                            vec![],
                            OpData::GetGlobal(GlobalId(global_alloca)),
                        ),
                    )?;
                    return Ok(Some(ptr_addr));
                }
            }
        }
        // ignore empty
        Ok(None)
    }
}

impl Pass<Program> for Emit<'_> {
    fn run(&mut self) -> Result<Program, String> {
        let root = self.root;
        self.emit(root)?;
        Ok(std::mem::take(&mut self.program))
    }
}
