use crate::asm::config::{RVOperandType, RVRegCode, REG_IDLE};

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
    pub fn find_free_temp_reg(&self) -> Option<RVRegCode> {
        for (i, &inst_id) in self.map.iter().enumerate() {
            if inst_id == REG_IDLE
                && (i >= RVRegCode::T0 as usize && i <= RVRegCode::T2 as usize
                    || i >= RVRegCode::T3 as usize && i <= RVRegCode::T6 as usize
                    // we should place a0-a7 at the lowest priority
                    || i >= RVRegCode::A0 as usize && i <= RVRegCode::A7 as usize)
            {
                return Some(unsafe { std::mem::transmute(i as u8) });
            }
        }
        // TODO: maybe we need to have mem allocation strategy in the future
        None
    }

    // find temp regs occupied by the specific inst
    pub fn find_inst_occupied_perm_reg(&self, inst_id: u32) -> Vec<RVOperandType> {
        self.map
            .iter()
            .enumerate()
            .filter(|(i, &inst_occupied)| {
                (*i >= RVRegCode::T0 as usize && *i <= RVRegCode::T2 as usize
                    || *i >= RVRegCode::T3 as usize && *i <= RVRegCode::T6 as usize
                    || *i >= RVRegCode::A0 as usize && *i <= RVRegCode::A7 as usize)
                    && inst_id == inst_occupied
            })
            .collect::<Vec<(usize, &u32)>>()
            .iter()
            .map(|(i, _)| RVOperandType::Perm(RVRegAllocator::from_idx(*i)))
            .collect::<Vec<RVOperandType>>()
    }

    pub fn find_and_occupy_param_reg(&mut self, inst_id: u32) -> RVOperandType {
        if let Some(reg) = self.find_free_param_reg() {
            self.occupy_reg(reg, inst_id);
            // param reg don't need to be perm,
            // cause they would be restore soon after function call,
            // which is all executed in one inst: KoopaOpCode::CALL
            RVOperandType::Temp(reg)
        } else {
            RVOperandType::None
        }
    }

    pub fn find_free_param_reg(&mut self) -> Option<RVRegCode> {
        for (i, &inst_id) in self.map.iter().enumerate() {
            if inst_id == REG_IDLE && i >= RVRegCode::A0 as usize && i <= RVRegCode::A7 as usize {
                return Some(unsafe { std::mem::transmute(i as u8) });
            }
        }

        None
    }

    pub fn occupy_reg(&mut self, reg: RVRegCode, inst_id: u32) {
        self.map[reg as usize] = inst_id;
    }

    pub fn find_and_occupy_temp_reg(&mut self, inst_id: u32) -> RVOperandType {
        if let Some(reg) = self.find_free_temp_reg() {
            self.occupy_reg(reg, inst_id);
            RVOperandType::Temp(reg)
        } else {
            RVOperandType::None
        }
    }

    pub fn find_and_occupy_perm_reg(&mut self, inst_id: u32) -> RVOperandType {
        if let Some(reg) = self.find_free_temp_reg() {
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

    pub fn param_reg_from_idx(idx: usize) -> RVRegCode {
        assert!(idx < 8);
        unsafe { std::mem::transmute::<u8, RVRegCode>((RVRegCode::A0 as u8) + (idx as u8)) }
    }
}

thread_local! {
    /// this array records the occupation status of each register.
    /// each item means the inst id that occupies this register.
    pub static RVREG_ALLOCATOR: RefCell<RVRegAllocator> = RefCell::new(RVRegAllocator::new());
}
