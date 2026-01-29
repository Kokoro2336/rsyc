use crate::base::Builder;
/**
 * Original IR generation.
 */
use crate::base::Pass;
use crate::frontend::ast::*;
use crate::utils::cast;

pub struct Emit<'a> {
    builder: Builder<'a>,
    root: &'a Box<dyn Node>,
}

impl<'a> Emit<'a> {
    pub fn new(builder: Builder<'a>, root: &'a Box<dyn Node>) -> Self {
        Self { builder, root }
    }

    pub fn emit(&mut self, node: &Box<dyn Node>) -> Result<(), String> {
        let builder = &mut self.builder;
        if let Some(fn_decl) = cast::<FnDecl>(node) {
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
            }
        }
        // ignore empty
        Ok(())
    }
}

impl Pass<()> for Emit<'_> {
    fn run(&mut self) -> Result<(), String> {
        let root = self.root;
        self.emit(root)
    }
}
