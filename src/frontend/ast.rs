use std::any::Any;

use crate::base::r#type::Type;
use crate::debug::graph::GraphNode;

// We can't impl Clone for dyn Node, because Clone return self, and self it's unknown at compile time.
pub trait Node: Any + std::fmt::Debug + GraphNode {
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug)]
pub struct FnDecl {
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
    pub typ: Type,
    pub lhs: Box<dyn Node>,
    pub op: Op,
    pub rhs: Box<dyn Node>,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub typ: Type,
    pub op: Op,
    pub operand: Box<dyn Node>,
}

#[derive(Debug)]
pub struct Call {
    pub typ: Type,
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
    pub typ: Type,
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
    pub typ: Type,
}

#[derive(Debug)]
pub struct Empty();

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
}

impl Literal {
    pub fn get_int(&self) -> i32 {
        if let Literal::Int(val) = self {
            *val
        } else {
            panic!("Literal is not Int");
        }
    }
    pub fn get_float(&self) -> f32 {
        if let Literal::Float(val) = self {
            *val
        } else {
            panic!("Literal is not Float");
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Op {
    // unary
    Plus,
    Minus,
    Not,
    // special op which only occurs in type casting
    Cast(Type, Type),

    // binary
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

impl Op {
    // Check if the operation only returns int type
    pub fn only_ret_int(&self) -> bool {
        matches!(
            self,
            Op::And | Op::Or | Op::Lt | Op::Gt | Op::Le | Op::Ge | Op::Eq | Op::Ne
        )
    }
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
    FnDecl,
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
    Empty,
    Assign,
    If,
    While,
    Literal,
    DeclAggr,
    RawDecl,
    RawDef,
    ArrayInitVal
);

// Raw struct passed through parsing phase
// Processed declaration aggregation
#[derive(Debug)]
pub struct DeclAggr {
    pub decls: Vec<Box<dyn Node>>,
}

// Original declaration aggregation
#[derive(Debug)]
pub struct RawDecl {
    pub typ: Type,
    pub mutable: bool,
    pub raw_decls: Vec<RawDef>,
}

// Original signle declaration
#[derive(Debug)]
pub struct RawDef {
    pub ident: String,
    pub const_exps: Vec<u32>,
    pub init_val: Option<Box<dyn Node>>,
}

// Original array initialization values
#[derive(Debug)]
pub struct ArrayInitVal {
    pub init_vals: Vec<Box<dyn Node>>,
}