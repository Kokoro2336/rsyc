use std::any::Any;

use crate::base::config::Type;

// We can't impl Clone for dyn Node, because Clone return self, and self it's unknown at compile time.
pub trait Node: Any + std::fmt::Debug {
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug)]
pub struct CompUnit {
    nodes: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct Function {
    name: String,
    params: Vec<(String, Type)>,
    return_type: Type,
    body: Box<dyn Node>,
}

#[derive(Debug)]
pub struct Break();
#[derive(Debug)]
pub struct Continue();
#[derive(Debug)]
pub struct Return(Option<Box<dyn Node>>);

#[derive(Debug)]
pub struct Block {
    statements: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct Assign {
    lhs: Box<dyn Node>,
    rhs: Box<dyn Node>,
}

#[derive(Debug)]
pub struct If {
    condition: Box<dyn Node>,
    then_block: Box<dyn Node>,
    else_block: Option<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct While {
    condition: Box<dyn Node>,
    body: Box<dyn Node>,
}

#[derive(Debug)]
pub struct BinaryOp {
    lhs: Box<dyn Node>,
    op: Op,
    rhs: Box<dyn Node>,
}

#[derive(Debug)]
pub struct UnaryOp {
    op: Op,
    operand: Box<dyn Node>,
}

#[derive(Debug)]
pub struct Call {
    func_name: String,
    args: Vec<Box<dyn Node>>,
}

// Var
#[derive(Debug)]
pub struct VarDecl {
    name: String,
    typ: Type,
    mutable: bool,
    init_value: Option<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct VarAccess {
    name: String,
}

// Array
#[derive(Debug)]
pub struct ConstArray {
    name: String,
    typ: Type,
    init_values: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct LocalArray {
    name: String,
    typ: Type,
    init_values: Option<Vec<Box<dyn Node>>>,
}

#[derive(Debug)]
pub struct ArrayAccess {
    name: String,
    indices: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct ArrayAssign {
    name: String,
    indices: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct Empty();

#[derive(Debug)]
pub struct Int(i32);
#[derive(Debug)]
pub struct Float(f32);

#[derive(Debug)]
pub enum Op {
    Plus,
    Minus,
    Not,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

macro_rules! impl_node {
    ($($t:ty),*) => {
        $(
            impl Node for $t {
                fn as_any(&self) -> &dyn Any {
                    self
                }
            }
        )*
    };
}

impl_node!(
    CompUnit, Function, Block, Break, Continue, Return, BinaryOp, UnaryOp, Call, 
    VarDecl, VarAccess, ConstArray, LocalArray, ArrayAccess, ArrayAssign, Empty,
    Assign, If, While, Int, Float, DeclAggr, ArrayInitVal
);

// Raw struct for parsing
// Original defined declaration structures
#[derive(Debug)]
pub struct DeclAggr {
  pub typ: Type,
  pub mutable: bool,
  pub raw_decls: Vec<RawDecl>,
}

#[derive(Debug)]
pub struct RawDecl {
  pub ident: String,
  pub const_exps: Vec<Box<dyn Node>>,
  pub init_val: Option<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct ArrayInitVal {
  pub init_vals: Vec<Box<dyn Node>>,
}
