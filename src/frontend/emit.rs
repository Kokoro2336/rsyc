use crate::base::ir::*;
use crate::base::Builder;
/**
 * Original IR generation.
 */
use crate::base::{Pass, SymbolTable, Type};
use crate::frontend::ast::*;
use crate::utils::cast;

pub struct Emit<'a> {
    builder: Builder,
    // This syms table is for storing the mapping from variable names to OpId
    globals: SymbolTable<String, OpId>,
    syms: SymbolTable<String, OpId>,
    root: &'a Box<dyn Node>,
    program: Program,
    // we store the idx of current function
    current_function: Option<usize>,
}

impl<'a> Emit<'a> {
    pub fn new(builder: Builder, root: &'a Box<dyn Node>) -> Self {
        Self {
            builder,
            globals: SymbolTable::new(),
            syms: SymbolTable::new(),
            root,
            program: Program::new(),
            current_function: None,
        }
    }

    pub fn emit(&mut self, node: &Box<dyn Node>) -> Result<(), String> {
        let builder = &mut self.builder;
        if let Some(fn_decl) = cast::<FnDecl>(node) {
            // create new function
            self.program.funcs.push(Function::new());
            self.current_function = Some(self.program.funcs.len() - 1);
            let func = &mut self.program.funcs[self.current_function.unwrap()];
            //create new block for function
            builder.current_block = Some(builder.create_new_block(func)?);

            // get and store arguments
            for (i, arg) in fn_decl.params.iter().enumerate() {
                let get_arg = builder.create(
                    func,
                    arg.1.clone(),
                    vec![Attr::Function(arg.0.clone()), Attr::Param(i as u32)],
                    OpData::GetArg,
                )?;
                let alloca = builder.create(
                    func,
                    arg.1.clone(),
                    vec![Attr::Param(i as u32)],
                    OpData::Alloca,
                )?;
                builder.create(
                    func,
                    Type::Void,
                    vec![],
                    OpData::Store {
                        addr: alloca,
                        value: get_arg,
                    },
                )?;
            }

            // parse the function body
            self.emit(&fn_decl.body)?;
        } else if let Some(block) = cast::<Block>(node) {
            block
                .statements
                .iter()
                .try_for_each(|stmt| self.emit(stmt))?;
        } else if let Some(var_decl) = cast::<VarDecl>(node) {
        } else if let Some(var_array) = cast::<VarArray>(node) {
        } else if let Some(const_array) = cast::<ConstArray>(node) {
        } else if let Some(ret) = cast::<Return>(node) {
        } else if let Some(if_stmt) = cast::<If>(node) {
        } else if let Some(while_stmt) = cast::<While>(node) {
        } else if let Some(break_stmt) = cast::<Break>(node) {
        } else if let Some(continue_stmt) = cast::<Continue>(node) {
        } else if let Some(assign_stmt) = cast::<Assign>(node) {
        } else if let Some(var_access) = cast::<VarAccess>(node) {
        } else if let Some(array_access) = cast::<ArrayAccess>(node) {
        } else if let Some(call) = cast::<Call>(node) {
        } else if let Some(binary_op) = cast::<BinaryOp>(node) {
        } else if let Some(unary_op) = cast::<UnaryOp>(node) {
        } else if let Some(literal) = cast::<Literal>(node) {
            match *literal {
                Literal::Int(val) => {}
                Literal::Float(val) => {}
                Literal::String(ref val) => {}
            }
        }
        // ignore empty
        Ok(())
    }
}

impl Pass<Program> for Emit<'_> {
    fn run(&mut self) -> Result<Program, String> {
        let root = self.root;
        self.emit(root)?;
        Ok(std::mem::take(&mut self.program))
    }
}
