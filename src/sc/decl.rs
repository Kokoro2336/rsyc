use crate::global::config::{eval_typ, BType};
use crate::global::context::SC_CONTEXT_STACK;
use crate::ir::config::{KoopaOpCode, IR_VAR_ID_ALLOCATOR, SC_VAR_ID_ALLOCATOR};
use crate::ir::koopa::{insert_ir, IRObj, InstData};
use crate::sc::exp::{Exp, Expression};

use std::cell::RefCell;
use std::cmp::min;
use std::ops::Deref;
use std::vec::Vec;
use tool::prelude::*;

pub trait Declaration {
    fn parse(&self) -> IRObj;
    fn parse_global(&self) -> IRObj {
        IRObj::None
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    ConstDecl { const_decl: ConstDecl },
    VarDecl { var_decl: VarDecl },
}

impl Decl {
    pub fn parse(&self) {
        match self {
            Decl::ConstDecl { const_decl } => {
                const_decl.parse();
            }
            Decl::VarDecl { var_decl } => {
                var_decl.parse();
            }
        }
    }

    pub fn parse_global(&self) -> Vec<IRObj> {
        match self {
            Decl::ConstDecl { const_decl } => const_decl.parse_global(),
            Decl::VarDecl { var_decl } => var_decl.parse_global(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

impl ConstDecl {
    fn parse(&self) {
        emit_cur_decl(Decl::ConstDecl {
            const_decl: self.clone(),
        });
        for const_def in &self.const_defs {
            let result = const_def.parse();
            SC_CONTEXT_STACK.with(|stack| {
                stack
                    .borrow_mut()
                    .insert_const(const_def.ident.clone(), result)
            });
        }
    }

    fn parse_global(&self) -> Vec<IRObj> {
        emit_cur_decl(Decl::ConstDecl {
            const_decl: self.clone(),
        });
        self.const_defs
            .iter()
            .map(|const_def| {
                let result = const_def.parse_global();

                SC_CONTEXT_STACK.with(|stack| {
                    stack
                        .borrow_mut()
                        .insert_global_sym(const_def.ident.clone(), result.clone())
                });
                result
            })
            .collect::<Vec<IRObj>>()
    }
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub ident: String,
    pub const_exps: Vec<ConstExp>,
    pub const_init_val: RefCell<ConstInitVal>,
}

impl FillArray<ConstInitVal> for ConstDef {
    fn get_ident(&self) -> String {
        self.ident.clone()
    }

    fn get_const_exps(&self) -> Vec<ConstExp> {
        self.const_exps.clone()
    }

    fn get_vals(&self) -> ConstInitVal {
        match *self.const_init_val.borrow() {
            ConstInitVal::ConstExp(_) => {
                panic!("ConstDef's init val must be ConstArrayExp when filling array")
            }
            ConstInitVal::ConstArrayExp(ref vals) => self.const_init_val.borrow().clone(),
            ConstInitVal::ZeroPlaceHolder => {
                panic!("ConstDef's init val must be ConstArrayExp when filling array")
            }
        }
    }

    fn get_zero_place_holder(&self) -> ConstInitVal {
        ConstInitVal::ZeroPlaceHolder
    }

    fn unwrap_array(&self, val: &ConstInitVal) -> Vec<ConstInitVal> {
        match val {
            ConstInitVal::ConstArrayExp(vals) => vals.clone(),
            _ => unreachable!("Cannot unwrap non-array ConstInitVal"),
        }
    }

    fn wrap_array(&self, vals: Vec<ConstInitVal>) -> ConstInitVal {
        ConstInitVal::ConstArrayExp(vals)
    }

    fn is_array(&self, val: &ConstInitVal) -> bool {
        matches!(val, ConstInitVal::ConstArrayExp(_))
    }

    fn is_exp(&self, val: &ConstInitVal) -> bool {
        matches!(val, ConstInitVal::ConstExp(_))
    }

    fn is_zero_place_holder(&self, val: &ConstInitVal) -> bool {
        matches!(val, ConstInitVal::ZeroPlaceHolder)
    }
}

impl Declaration for ConstDef {
    fn parse(&self) -> IRObj {
        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_sc_var(self.ident.as_str()))
            .is_some()
        {
            panic!(
                "Cannot declare constant {} with the same name as a variable",
                self.ident
            );
        }

        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_const(self.ident.as_str()))
            .is_some()
        {
            panic!("Constant {} already declared", self.ident);
        }

        let old_typ = get_cur_type();
        let indexes = self
            .const_exps
            .iter()
            .map(|const_exp| match const_exp.parse() {
                IRObj::Const(c) => c as u32,
                _ => unreachable!("ConstExp must parse to Const"),
            })
            .collect::<Vec<u32>>();

        if !self.const_exps.is_empty() {
            *self.const_init_val.borrow_mut() = self.fill_array();
        }
        let typ = eval_typ(get_cur_decl_type(), &indexes, 0);
        emit_cur_type(typ.clone());

        let mut result = self.const_init_val.borrow().parse();

        emit_cur_type(old_typ);

        match &mut result {
            IRObj::Const(_) => result,
            IRObj::Array { ident, typ, .. } => {
                *ident = self.ident.clone();
                *typ = BType::Pointer {
                    typ: Box::new(typ.clone()),
                };
                result
            }
            _ => unreachable!(
                "Global constant {} must be initialized with a constant or array",
                self.ident
            ),
        }
    }

    fn parse_global(&self) -> IRObj {
        let old_typ = get_cur_type();
        let indexes = self
            .const_exps
            .iter()
            .map(|const_exp| match const_exp.parse() {
                IRObj::Const(c) => c as u32,
                _ => unreachable!("ConstExp must parse to Const"),
            })
            .collect::<Vec<u32>>();

        let typ = eval_typ(get_cur_decl_type(), &indexes, 0);
        emit_cur_type(typ.clone());

        if !self.const_exps.is_empty() {
            *self.const_init_val.borrow_mut() = self.fill_array();
        }
        let mut result = self.const_init_val.borrow().parse();

        emit_cur_type(old_typ);

        match &mut result {
            IRObj::Const(_) => result,
            IRObj::Array { ident, typ, .. } => {
                *ident = self.ident.clone();
                *typ = BType::Pointer {
                    typ: Box::new(typ.clone()),
                };
                result
            }
            _ => unreachable!(
                "Global constant {} must be initialized with a constant or array",
                self.ident
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstArrayExp(Vec<ConstInitVal>),
    ZeroPlaceHolder,
}

impl IntoIterator for ConstInitVal {
    type Item = ConstInitVal;
    type IntoIter = std::vec::IntoIter<ConstInitVal>;

    fn into_iter(self) -> Self::IntoIter {
        vec![self].into_iter()
    }
}

impl Declaration for ConstInitVal {
    fn parse(&self) -> IRObj {
        match self {
            ConstInitVal::ConstExp(const_exp) => const_exp.parse(),
            ConstInitVal::ConstArrayExp(const_initvals) => {
                let init_results = const_initvals
                    .iter()
                    .map(|const_initval| const_initval.parse())
                    .collect::<Vec<IRObj>>();

                IRObj::Array {
                    ident: "".to_string(),
                    typ: BType::Void,
                    dimensions: vec![],
                    vals: Some(init_results),
                }
            }
            ConstInitVal::ZeroPlaceHolder => IRObj::Const(0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstExp {
    pub exp: Box<Exp>,
}

impl Declaration for ConstExp {
    fn parse(&self) -> IRObj {
        self.exp.parse_const_exp()
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub b_type: BType,
    pub var_defs: Vec<VarDef>,
}

impl VarDecl {
    fn parse(&self) {
        emit_cur_decl(Decl::VarDecl {
            var_decl: self.clone(),
        });
        for var_def in &self.var_defs {
            let result = var_def.parse();
            // insert sc_var into sc_var table for parsing first.
            SC_CONTEXT_STACK.with(|stack| {
                stack
                    .borrow_mut()
                    .insert_sc_var(var_def.ident.clone(), result)
            });
        }
    }

    fn parse_global(&self) -> Vec<IRObj> {
        emit_cur_decl(Decl::VarDecl {
            var_decl: self.clone(),
        });
        self.var_defs
            .iter()
            .map(|var_def| {
                let result = var_def.parse_global();

                // insert sc_var into sc_var table for parsing first.
                SC_CONTEXT_STACK.with(|stack| {
                    stack
                        .borrow_mut()
                        .insert_global_sym(var_def.ident.clone(), result.clone())
                });

                result
            })
            .collect::<Vec<IRObj>>()
    }
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: String,
    pub const_exps: Vec<ConstExp>,
    pub init_val: RefCell<Option<InitVal>>,
}

impl FillArray<InitVal> for VarDef {
    fn get_ident(&self) -> String {
        self.ident.clone()
    }

    fn get_const_exps(&self) -> Vec<ConstExp> {
        self.const_exps.clone()
    }

    fn get_vals(&self) -> InitVal {
        if self.const_exps.is_empty() {
            panic!("You couldn't call this function when the init_val is not an array");
        }

        match self.init_val.borrow().as_ref() {
            Some(inner) => match inner {
                InitVal::ArrayExp(_) => inner.clone(),
                _ => unreachable!("VarDef's init val must be ArrayExp when filling array"),
            },
            // {} empty array init
            None => InitVal::ArrayExp(vec![]),
        }
    }

    fn get_zero_place_holder(&self) -> InitVal {
        InitVal::ZeroPlaceHolder
    }

    fn unwrap_array(&self, val: &InitVal) -> Vec<InitVal> {
        match val {
            InitVal::ArrayExp(vals) => vals.clone(),
            _ => unreachable!("Cannot unwrap non-array InitVal"),
        }
    }

    fn wrap_array(&self, vals: Vec<InitVal>) -> InitVal {
        InitVal::ArrayExp(vals)
    }

    fn is_array(&self, val: &InitVal) -> bool {
        matches!(val, InitVal::ArrayExp(_))
    }

    fn is_exp(&self, val: &InitVal) -> bool {
        matches!(val, InitVal::Exp(_))
    }

    fn is_zero_place_holder(&self, val: &InitVal) -> bool {
        matches!(val, InitVal::ZeroPlaceHolder)
    }
}

impl Declaration for VarDef {
    fn parse(&self) -> IRObj {
        // semantic check
        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_const(self.ident.as_str()))
            .is_some()
        {
            panic!(
                "Cannot declare variable {} with the same name as a constant",
                self.ident
            );
        }

        if SC_CONTEXT_STACK
            .with(|stack| stack.borrow().get_current_sc_var(self.ident.as_str()))
            .is_some()
        {
            panic!("Variable {} already declared", self.ident);
        }

        if !self.const_exps.is_empty() {
            *self.init_val.borrow_mut() = Some(self.fill_array());
        }

        let indexes = self
            .const_exps
            .iter()
            .map(|const_exp| match const_exp.parse() {
                IRObj::Const(c) => c as u32,
                _ => unreachable!("ConstExp must parse to Const"),
            })
            .collect::<Vec<u32>>();

        let old_typ = get_cur_type();
        let typ = eval_typ(get_cur_decl_type(), &indexes, 0);
        emit_cur_type(typ.clone());

        let var_id = SC_VAR_ID_ALLOCATOR.with(|allocator| allocator.borrow_mut().alloc());
        let var = if self.const_exps.is_empty() {
            IRObj::ScVar {
                typ: BType::Pointer {
                    typ: Box::new(typ.clone()),
                },
                initialized: self.init_val.borrow().is_some(),
                sc_var_id: var_id, // placeholder, will be replaced
            }
        } else {
            IRObj::Array {
                ident: self.ident.clone(),
                typ: BType::Pointer {
                    typ: Box::new(typ.clone()),
                },
                dimensions: indexes.clone(),
                // to be filled later
                vals: None,
            }
        };

        // whatever the init_val is, we need to allocate space for the variable
        insert_ir(InstData::new(
            BType::Pointer {
                typ: Box::new(typ.clone()),
            },
            var.clone(),
            KoopaOpCode::ALLOC,
            vec![IRObj::BType(typ)],
        ));

        let old_obj = get_cur_obj();
        emit_cur_obj(var.clone());

        if let Some(init_val) = self.init_val.borrow().as_ref() {
            init_val.parse();
        };

        emit_cur_obj(old_obj);
        emit_cur_type(old_typ);
        var
    }

    fn parse_global(&self) -> IRObj {
        if SC_CONTEXT_STACK.with(|stack| {
            let global_sym = stack.borrow().get_global_sym(&self.ident);
            global_sym.is_some() && !matches!(Some(IRObj::None), global_sym)
        }) {
            panic!("Global variable {} already declared", self.ident);
        }

        let indexes = self
            .const_exps
            .iter()
            .map(|const_exp| match const_exp.parse() {
                IRObj::Const(c) => c as u32,
                _ => unreachable!("ConstExp must parse to Const"),
            })
            .collect::<Vec<u32>>();

        let old_typ = get_cur_type();
        let typ = eval_typ(get_cur_decl_type(), &indexes, 0);
        emit_cur_type(typ.clone());

        if !self.const_exps.is_empty() {
            *self.init_val.borrow_mut() = Some(self.fill_array());
        }

        let old_const_only = is_const_only();
        set_const_only(true);

        let mut parse_result = match self.init_val.borrow().as_ref() {
            Some(init_val) => init_val.parse(),
            None => IRObj::Const(0),
        };

        set_const_only(old_const_only);

        let global_const_to_var = fix(|f, obj: &IRObj| -> IRObj {
            match obj {
                IRObj::Const(c) => IRObj::GlobalVar {
                    initialized: self.init_val.borrow().is_some(),
                    global_var_id: "".to_string(),
                    init_val: *c,
                },
                IRObj::Array {
                    ident,
                    typ,
                    dimensions,
                    vals,
                } => IRObj::Array {
                    ident: ident.clone(),
                    typ: typ.clone(),
                    dimensions: dimensions.clone(),
                    vals: Some(
                        vals.as_ref()
                            .expect("Global Array must be initialized first before converting!")
                            .iter()
                            .map(|val| f(val))
                            .collect::<Vec<IRObj>>(),
                    ),
                },
                _ => unreachable!("Global variable init val must be constant or array"),
            }
        });

        emit_cur_type(old_typ);

        match &mut parse_result {
            IRObj::Const(c) => IRObj::GlobalVar {
                initialized: self.init_val.borrow().is_some(),
                global_var_id: self.ident.clone(),
                init_val: *c,
            },
            IRObj::Array { ident, typ, .. } => {
                *ident = self.ident.clone();
                *typ = BType::Pointer {
                    typ: Box::new(typ.clone()),
                };
                global_const_to_var(&parse_result)
            }
            _ => unreachable!(
                "Global variable {} must be initialized with a constant or array",
                &self.ident
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InitVal {
    Exp(Box<Exp>),
    ArrayExp(Vec<InitVal>),
    ZeroPlaceHolder, // fill array with zero
}

impl IntoIterator for InitVal {
    type Item = InitVal;
    type IntoIter = std::vec::IntoIter<InitVal>;

    fn into_iter(self) -> Self::IntoIter {
        vec![self].into_iter()
    }
}

impl Declaration for InitVal {
    fn parse(&self) -> IRObj {
        match self {
            InitVal::Exp(exp) => {
                if is_const_only() {
                    return exp.parse_const_exp();
                }

                let parse_result = exp.parse_var_exp();
                // we don't need to store temp var to var_table here for it'll be removed soon after STORE
                if let IRObj::Const(_) | IRObj::IRVar { .. } | IRObj::ReturnVal { .. } =
                    parse_result
                {
                    insert_ir(InstData::new(
                        BType::Void,
                        IRObj::None,
                        KoopaOpCode::STORE,
                        vec![
                            match parse_result {
                                IRObj::IRVar { .. } | IRObj::Const(_) | IRObj::ReturnVal { .. } => {
                                    parse_result.clone()
                                }
                                _ => unreachable!(),
                            },
                            // the target might be IRVar, ScVar or an Array for now
                            get_cur_obj(),
                        ],
                    ));
                }
                parse_result
            }

            InitVal::ArrayExp(exps) => {
                let init_results = exps
                    .iter()
                    .enumerate()
                    .map(|(idx, init_val)| {
                        if is_const_only() {
                            return init_val.parse();
                        }

                        let old_typ = get_cur_type();
                        let typ = match old_typ.clone() {
                            BType::Array { typ, .. } => typ.clone(),
                            _ => {
                                unreachable!(
                                    "Array init val must have array type in decl context: {:#?}",
                                    old_typ
                                );
                            }
                        };
                        let wrapped_typ = BType::Pointer {
                            typ: Box::new(*typ.clone()),
                        };

                        let ir_obj = IRObj::new_ir_var(wrapped_typ.clone());
                        let old_obj = get_cur_obj();

                        insert_ir(InstData::new(
                            {
                                emit_cur_type(*typ.clone());
                                wrapped_typ.clone()
                            },
                            ir_obj.clone(),
                            KoopaOpCode::GETELEMPTR,
                            vec![old_obj.clone(), IRObj::Const(idx as i32)],
                        ));

                        emit_cur_obj(ir_obj.clone());
                        let result = init_val.parse();

                        // retrieve the old obj and typ
                        emit_cur_obj(old_obj);
                        emit_cur_type(old_typ);

                        result
                    })
                    .collect::<Vec<IRObj>>();

                IRObj::Array {
                    ident: "".to_string(),
                    typ: BType::Void,
                    dimensions: vec![],
                    vals: Some(init_results),
                }
            }
            InitVal::ZeroPlaceHolder => IRObj::Const(0),
        }
    }
}

pub trait FillArray<T: Clone + IntoIterator> {
    fn get_ident(&self) -> String;
    fn get_const_exps(&self) -> Vec<ConstExp>;
    fn get_vals(&self) -> T;
    fn get_zero_place_holder(&self) -> T;

    fn unwrap_array(&self, val: &T) -> Vec<T>;
    fn wrap_array(&self, vals: Vec<T>) -> T;

    fn is_array(&self, val: &T) -> bool;
    fn is_exp(&self, val: &T) -> bool;
    fn is_zero_place_holder(&self, val: &T) -> bool;

    fn fill_array(&self) -> T {
        let indexes = self
            .get_const_exps()
            .iter()
            .map(|const_exp| match const_exp.parse() {
                IRObj::Const(c) => c as u32,
                _ => unreachable!("ConstExp must parse to Const"),
            })
            .collect::<Vec<u32>>();
        let new_vals = RefCell::new(vec![]);

        // flatten origin array
        let rec = fix(|f, params: (T, u32)| -> u32 {
            let (val, depth) = params;
            let mut filled_size: u32 = 0;

            // find minimal depth of current array.
            if self.is_array(&val) {
                if new_vals.borrow().len() as u32 % indexes.last().unwrap() != 0 {
                    panic!("Array has insufficient initializers");
                }

                let vals = self.unwrap_array(&val);
                let sub_filled_size = vals.iter().fold(filled_size, |filled_size, val| {
                    filled_size + f((val.clone(), depth + 1))
                });

                filled_size += sub_filled_size;

                let to_be_filled = indexes[depth as usize..indexes.len()]
                    .iter()
                    .fold(1, |acc, index| acc * (*index as usize))
                    as u32
                    - filled_size;

                // fill 0
                (0..to_be_filled).for_each(|_| {
                    new_vals.borrow_mut().push(self.get_zero_place_holder());
                });

                filled_size += to_be_filled;
            } else if self.is_exp(&val) {
                new_vals.borrow_mut().push(val.clone());
                filled_size += 1;
            } else if self.is_zero_place_holder(&val) {
                new_vals.borrow_mut().push(self.get_zero_place_holder());
                filled_size += 1;
            } else {
                unreachable!("Array must contain only Array, Exp or ZeroPlaceHolder")
            };

            filled_size
        });

        rec((self.get_vals(), 0));
        let expected_size = indexes.iter().fold(1, |acc, index| acc * (*index as usize));
        if new_vals.borrow().len() != expected_size {
            panic!(
                "Array has insufficient initializers: expected {}, found {}",
                expected_size,
                new_vals.borrow().len()
            );
        }

        indexes
            .iter()
            .rev()
            .fold(new_vals.clone().borrow().clone(), |mut acc, &size| {
                acc = acc
                    .chunks(size as usize)
                    .map(|chunk| self.wrap_array(chunk.to_vec()))
                    .collect::<Vec<T>>();

                acc
            })
            .remove(0)
    }
}

pub struct DeclContext {
    // arr being processsed currently
    pub cur_obj: IRObj,
    // current type
    pub cur_type: BType,
    // current decl type
    pub cur_decl: Option<Decl>,
    // const only
    pub const_only: bool,
}

impl DeclContext {
    pub fn new() -> Self {
        DeclContext {
            cur_obj: IRObj::None,
            cur_type: BType::Unknown,
            cur_decl: None,
            const_only: false,
        }
    }

    pub fn emit_cur_decl(&mut self, decl: Decl) {
        self.cur_decl = Some(decl);
    }

    pub fn get_cur_decl(&self) -> Option<Decl> {
        self.cur_decl.clone()
    }

    pub fn emit_cur_obj(&mut self, obj: IRObj) {
        self.cur_obj = obj;
    }

    pub fn get_cur_obj(&self) -> IRObj {
        self.cur_obj.clone()
    }

    pub fn emit_cur_type(&mut self, typ: BType) {
        self.cur_type = typ;
    }

    pub fn get_cur_type(&self) -> BType {
        self.cur_type.clone()
    }

    pub fn set_const_only(&mut self, const_only: bool) {
        self.const_only = const_only;
    }

    pub fn is_const_only(&self) -> bool {
        self.const_only
    }
}

thread_local! {
    pub static DECL_CONTEXT: RefCell<DeclContext> = RefCell::new(DeclContext::new());
}

pub fn emit_cur_obj(obj: IRObj) {
    DECL_CONTEXT.with(|cxt| cxt.borrow_mut().emit_cur_obj(obj));
}

pub fn get_cur_obj() -> IRObj {
    DECL_CONTEXT.with(|cxt| cxt.borrow().get_cur_obj())
}

pub fn emit_cur_type(typ: BType) {
    DECL_CONTEXT.with(|cxt| cxt.borrow_mut().emit_cur_type(typ));
}

pub fn get_cur_type() -> BType {
    DECL_CONTEXT.with(|cxt| cxt.borrow().get_cur_type())
}

pub fn emit_cur_decl(decl: Decl) {
    DECL_CONTEXT.with(|cxt| cxt.borrow_mut().emit_cur_decl(decl));
}

pub fn get_cur_decl() -> Decl {
    DECL_CONTEXT
        .with(|cxt| cxt.borrow().get_cur_decl())
        .expect("No current declaration in context")
}

pub fn set_const_only(const_only: bool) {
    DECL_CONTEXT.with(|cxt| cxt.borrow_mut().set_const_only(const_only));
}

pub fn is_const_only() -> bool {
    DECL_CONTEXT.with(|cxt| cxt.borrow().is_const_only())
}

pub fn get_cur_decl_type() -> BType {
    match get_cur_decl() {
        Decl::ConstDecl { const_decl } => const_decl.b_type,
        Decl::VarDecl { var_decl } => var_decl.b_type,
    }
}
