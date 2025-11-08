use crate::ir::koopa::Func;
use crate::global::config::BType;
use crate::asm::config::{RVRegCode, RVOperandType, STK_FRM_BASE_LENGTH};

use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug, Clone)]
/// sp, fp, size of frame
pub struct StackFrame {
    // current sp's offset relative to initiative sp
    sp_offset: u32,
    // current fp's offset relative to initiative fp
    fp_offset: u32,
    // total size of stack frame
    size: u32,
    // offset relative to current sp.
    cur_offset: u32,
    // variable map in this stack frame
    var_map: HashMap<String, (u32, u32)>, // variable name to (offset, size) in stack frame
}

#[derive(Debug)]
pub struct StackFrameManager {
    frames: Vec<StackFrame>,
}

impl StackFrameManager {
    pub fn new() -> Self {
        Self { frames: vec![] }
    }

    pub fn prologue(&mut self, func: &Func) {
        // iterate inst_map, calculate stack frame size. as koopa ir is in SSA form,
        // each inst is only assigned once, so we can simply sum up the size of all as long as the inst have return value.
        let dfg = func.dfg.borrow();
        let origin_size = dfg
            .inst_map
            .iter()
            .fold(0, |acc, (_, inst)| acc + inst.typ.size_in_bytes());

        // actually manager doesn't know the initial value of sp and fp,
        // so we use positive offset to represent them for convenience.
        let size =
            ((origin_size + (STK_FRM_BASE_LENGTH - 1)) / STK_FRM_BASE_LENGTH) * STK_FRM_BASE_LENGTH;

        let (sp_offset, fp_offset) = if let Some(stack_frame) = self.frames.last() {
            (stack_frame.sp_offset + size, stack_frame.sp_offset)
        } else {
            (0, 0)
        };

        self.frames.push(StackFrame {
            sp_offset,
            fp_offset,
            size,
            cur_offset: 0,
            var_map: HashMap::new(),
        });
    }

    pub fn get_sp_offset(&self) -> u32 {
        self.frames.last().map_or(0, |frame| frame.sp_offset)
    }

    pub fn get_fp_offset(&self) -> u32 {
        self.frames.last().map_or(0, |frame| frame.fp_offset)
    }

    pub fn get_size(&self) -> u32 {
        self.frames.last().map_or(0, |frame| frame.size)
    }

    pub fn epilogue(&mut self) {
        self.frames.pop();
    }

    pub fn is_callee(&self) -> bool {
        self.frames.len() > 1
    }

    // @return (offset, size)
    pub fn alloc_var(&mut self, name: String, typ: BType) -> (u32, u32) {
        let frame = self.frames.last_mut().unwrap();

        let offset = frame.cur_offset;
        let size = match typ {
            BType::Int => 4,
            BType::Void => 0,
        };

        frame.var_map.insert(name, (offset, size));
        frame.cur_offset += size;

        (offset, size)
    }

    /// this would return RVOperandType with eventual offset in stack frame
    pub fn alloc_named_var_wrapped(&mut self, name: String, typ: BType) -> RVOperandType {
        let (offset, _) = self.alloc_var(name, typ);
        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::SP,
        }
    }

    /// often for temporary variables without name
    pub fn alloc_anonymous_var_wrapped(&mut self, typ: BType) -> RVOperandType {
        let (offset, _) = self.alloc_var("".to_string(), typ);
        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::SP,
        }
    }

    pub fn get_named_var_wrapped(&self, name: String) -> RVOperandType {
        let frame = self.frames.last().unwrap();
        let (offset, _) = frame.var_map.get(&name).cloned().unwrap_or_else(|| panic!(
            "Variable {} not found in current stack frame!",
            name
        ));

        RVOperandType::MemWithReg {
            offset,
            reg: RVRegCode::SP,
        }
    }
}

thread_local! {
    pub static STK_FRM_MANAGER: RefCell<StackFrameManager> = RefCell::new(StackFrameManager::new());
}
