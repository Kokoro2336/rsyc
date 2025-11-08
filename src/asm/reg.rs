use crate::asm::config::{RVRegCode, RVOperandType, REG_IDLE};

use std::cell::RefCell;

pub struct RVRegAllocator {
    pub map: [u32; 32], // each item means the inst id that occupies this register.
}

impl RVRegAllocator {
    pub fn new() -> Self {
        Self {
            map: [REG_IDLE; 32],
        }
    }

    pub fn get_reg_occupation(&self, reg: RVRegCode) -> u32 {
        self.map[reg as usize]
    }

    /// temp regs: t0-t6, a0-a7
    pub fn find_free_reg(&self) -> Option<RVRegCode> {
        for (i, &inst_id) in self.map.iter().enumerate() {
            if inst_id == REG_IDLE
                && (i >= RVRegCode::T0 as usize && i <= RVRegCode::T2 as usize
                    || i >= RVRegCode::T3 as usize && i <= RVRegCode::T6 as usize
                    || i >= RVRegCode::A0 as usize && i <= RVRegCode::A7 as usize)
            {
                return Some(unsafe { std::mem::transmute(i as u8) });
            }
        }
        // TODO: maybe we need to have mem allocation strategy in the future
        None
    }

    pub fn occupy_reg(&mut self, reg: RVRegCode, inst_id: u32) {
        self.map[reg as usize] = inst_id;
    }

    pub fn find_and_occupy_temp_reg(&mut self, inst_id: u32) -> RVOperandType {
        if let Some(reg) = self.find_free_reg() {
            self.occupy_reg(reg, inst_id);
            RVOperandType::Temp(reg)
        } else {
            RVOperandType::None
        }
    }

    pub fn find_and_occupy_perm_reg(&mut self, inst_id: u32) -> RVOperandType {
        if let Some(reg) = self.find_free_reg() {
            self.occupy_reg(reg, inst_id);
            RVOperandType::Perm(reg)
        } else {
            RVOperandType::None
        }
    }

    pub fn free_reg(&mut self, reg: RVRegCode) {
        self.map[reg as usize] = REG_IDLE;
    }

    pub fn from_idx(idx: usize) -> RVRegCode {
        assert!(idx < 32);
        unsafe { std::mem::transmute::<u8, RVRegCode>(idx as u8) }
    }
}

thread_local! {
    /// this array records the occupation status of each register.
    /// each item means the inst id that occupies this register.
    pub static RVREG_ALLOCATOR: RefCell<RVRegAllocator> = RefCell::new(RVRegAllocator::new());
}
