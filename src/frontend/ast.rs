use std::any::Any;

use crate::base::config::Type;

// We can't impl Clone for dyn Node, because Clone return self, and self it's unknown at compile time.
pub trait Node: Any + std::fmt::Debug {
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Box<dyn Node>,
}

#[derive(Debug)]
pub struct Break();
#[derive(Debug)]
pub struct Continue();
#[derive(Debug)]
pub struct Return(pub Option<Box<dyn Node>>);

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct Assign {
    pub lhs: Box<dyn Node>,
    pub rhs: Box<dyn Node>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Box<dyn Node>,
    pub then_block: Box<dyn Node>,
    pub else_block: Option<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct While {
    pub condition: Box<dyn Node>,
    pub body: Box<dyn Node>,
}

#[derive(Debug)]
pub struct BinaryOp {
    pub lhs: Box<dyn Node>,
    pub op: Op,
    pub rhs: Box<dyn Node>,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub op: Op,
    pub operand: Box<dyn Node>,
}

#[derive(Debug)]
pub struct Call {
    pub func_name: String,
    pub args: Vec<Box<dyn Node>>,
}

// Var
#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub typ: Type,
    pub mutable: bool,
    pub init_value: Option<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct VarAccess {
    pub name: String,
}

// Array
#[derive(Debug)]
pub struct ConstArray {
    pub name: String,
    pub typ: Type,
    pub init_values: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct LocalArray {
    pub name: String,
    pub typ: Type,
    pub init_values: Option<Vec<Box<dyn Node>>>,
}

#[derive(Debug)]
pub struct ArrayAccess {
    pub name: String,
    pub indices: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct ArrayAssign {
    pub name: String,
    pub indices: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct Empty();

#[derive(Debug)]
pub struct Int(pub i32);
#[derive(Debug)]
pub struct Float(pub f32);

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
    Function,
    Block,
    Break,
    Continue,
    Return,
    BinaryOp,
    UnaryOp,
    Call,
    VarDecl,
    VarAccess,
    ConstArray,
    LocalArray,
    ArrayAccess,
    ArrayAssign,
    Empty,
    Assign,
    If,
    While,
    Int,
    Float,
    DeclAggr,
    RawDecl,
    RawDef,
    ArrayInitVal
);

// Raw struct for parsing
// Original defined declaration structures
#[derive(Debug)]
pub struct DeclAggr {
    pub decls: Vec<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct RawDecl {
    pub typ: Type,
    pub mutable: bool,
    pub raw_decls: Vec<RawDef>,
}

#[derive(Debug)]
pub struct RawDef {
    pub ident: String,
    pub const_exps: Vec<u32>,
    pub init_val: Option<Box<dyn Node>>,
}

#[derive(Debug)]
pub struct ArrayInitVal {
    pub init_vals: Vec<Box<dyn Node>>,
}
