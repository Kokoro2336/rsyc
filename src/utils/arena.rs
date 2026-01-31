use crate::base::ir::Program;
/**
 * Simple arena allocator for indexed storage of values.
 * Faster retrieval by index compared to HashMap.
 */
use crate::base::Pass;
use crate::debug::error;

pub trait Arena<T> {
    fn remove(&mut self, idx: usize) -> Result<usize, String>;
    fn gc(&mut self) -> Result<Vec<ArenaItem<T>>, String>;
}

pub enum ArenaItem<T> {
    // This is for storing actual data.
    Data(T),
    // This is for marking transported data's new index in the new arena.
    NewIndex(usize),
    // This is for slot whose former data has been permanently destroyed.
    None,
}

impl<T> ArenaItem<T> {
    pub fn replace(&mut self, new_index: usize) -> Self {
        std::mem::replace(self, ArenaItem::NewIndex(new_index))
    }
}

pub struct IndexedArena<T> {
    pub storage: Vec<ArenaItem<T>>,
}

impl<T> IndexedArena<T> {
    pub fn new() -> Self {
        Self {
            storage: vec![ArenaItem::None],
        }
    }

    pub fn alloc(&mut self, data: T) -> Result<usize, &str> {
        let index = self.storage.len();
        self.storage.push(ArenaItem::Data(data));
        Ok(index)
    }

    pub fn get(&self, idx: usize) -> Result<Option<&T>, &str> {
        if idx >= self.storage.len() {
            return Err("IndexedArena insert: index out of bounds");
        }
        // match None: the arena is empty
        // match Some(AreanaItem::Data): the arena has data at idx
        // match Some(AreanaItem::NewIndex): the arena has transported data at idx, invalid!
        // match Some(AreanaItem::None): the arena has deleted data at idx, invalid!
        if matches!(
            self.storage.get(idx),
            Some(ArenaItem::None) | Some(ArenaItem::NewIndex(_))
        ) {
            return Err("IndexedArena get: index points to None or NewIndex");
        }
        match self.storage.get(idx) {
            Some(ArenaItem::Data(node)) => Ok(Some(node)),
            _ => Ok(None),
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Result<Option<&mut T>, &str> {
        if idx >= self.storage.len() {
            return Err("IndexedArena insert: index out of bounds");
        }
        if matches!(
            self.storage.get(idx),
            Some(ArenaItem::None) | Some(ArenaItem::NewIndex(_))
        ) {
            return Err("IndexedArena get_mut: index points to None or NewIndex");
        }
        match self.storage.get_mut(idx) {
            Some(ArenaItem::Data(node)) => Ok(Some(node)),
            _ => Ok(None),
        }
    }
}
