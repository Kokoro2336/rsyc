/**
 * Simple arena allocator for indexed storage of values.
 * Faster retrieval by index compared to HashMap.
 */
use crate::base::pass::Pass;
use crate::log::error;
use crate::base::op::Program;

pub struct Compaction<'a> {
    at: Option<usize>,
    arena: &'a mut Program,
}

impl<'a> Compaction<'a> {
    pub fn new(arena: &'a mut Program) -> Self {
        Self { at: None, arena }
    }
}

impl<'a> Pass for Compaction<'a> {
    fn run(&mut self) {
        // run arena's own gargage collecting and rewriting
        // self.arena.gc();
        // TODO: run rewriting of other structures excluding dfg.
    }
}

pub trait CompactTrait {
    fn rewrite_indices(&mut self, old_storage: &Vec<ArenaItem<Self>>)
    where
        Self: Sized;
}

pub struct ArenaNode<T: CompactTrait> {
    prev: Option<usize>,
    data: T,
    next: Option<usize>,
}

pub enum ArenaItem<T: CompactTrait> {
    // This is for storing actual data.
    Data(ArenaNode<T>),
    // This is for marking transported data's new index in the new arena.
    NewIndex(usize),
    // This is for slot whose former data has been permanently destroyed.
    None,
}

impl<T: CompactTrait> ArenaItem<T> {
    pub fn data(&self) -> Option<&T> {
        match self {
            ArenaItem::Data(node) => Some(&node.data),
            _ => None,
        }
    }
}

impl<T: CompactTrait> ArenaItem<T> {
    pub fn replace(&mut self, new_index: usize) -> Self {
        std::mem::replace(self, ArenaItem::NewIndex(new_index))
    }
}

pub struct IndexedArena<T: CompactTrait> {
    head: Option<usize>,
    storage: Vec<ArenaItem<T>>,
}

impl<T: CompactTrait> IndexedArena<T> {
    pub fn new() -> Self {
        Self {
            head: None,
            storage: vec![ArenaItem::None],
        }
    }

    pub fn get(&self, idx: usize) -> Option<&ArenaNode<T>> {
        match self.storage.get(idx) {
            Some(ArenaItem::Data(node)) => Some(node),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut ArenaNode<T>> {
        match self.storage.get_mut(idx) {
            Some(ArenaItem::Data(node)) => Some(node),
            _ => None,
        }
    }

    // We can pass builder's at to idx.
    pub fn insert(&mut self, idx: usize, data: T) -> usize {
        if idx >= self.storage.len() {
            error!("IndexedArena insert: index out of bounds");
        }
        // match None: the arena is empty
        // match Some(AreanaItem::Data): the arena has data at idx
        // match Some(AreanaItem::NewIndex): the arena has transported data at idx, invalid!
        // match Some(AreanaItem::None): the arena has deleted data at idx, invalid!
        if matches!(
            self.storage.get(idx),
            Some(ArenaItem::None) | Some(ArenaItem::NewIndex(_))
        ) {
            error!("IndexedArena insert: cannot insert at deleted or transported index");
        }
        let index = self.storage.len();
        let mut prev_idx = None;

        if let Some(node) = self.get(idx) {
            prev_idx = node.prev;
        }

        // insert the new node first
        let new_node = ArenaNode {
            prev: prev_idx,
            data,
            next: Some(idx),
        };
        self.storage.push(ArenaItem::Data(new_node));

        // update the surrounding nodes
        if let Some(prev) = prev_idx {
            if let Some(node) = self.get_mut(prev) {
                node.next = Some(index);
            }
        } else {
            self.head = Some(index);
        }

        if let Some(node) = self.get_mut(idx) {
            node.prev = Some(index);
        }

        index
    }

    pub fn erase(&mut self, idx: usize) -> usize {
        let (prev, next) = if let Some(node) = self.get(idx) {
            (node.prev, node.next)
        } else {
            error!("IndexedArena erase: index not found");
        };

        // update the surrounding nodes
        if let Some(prev) = prev {
            if let Some(prev_node) = self.get_mut(prev) {
                prev_node.next = next;
            }
        } else {
            self.head = next;
        }

        if let Some(next) = next {
            if let Some(next_node) = self.get_mut(next) {
                next_node.prev = prev;
            }
        }

        // mark this slot as deleted
        self.storage[idx] = ArenaItem::None;
        idx
    }

    pub fn gc(&mut self) -> Vec<ArenaItem<T>> {
        let mut new_arena: Vec<ArenaItem<T>> = vec![];
        let mut at = self.head;
        if at.is_none() {
            return new_arena;
        }

        // Transport
        while let Some(idx) = at {
            // check if the slot is occupied by data
            if self.get(idx).is_some() {
                let new_idx = new_arena.len();
                let data = self.storage[idx].replace(new_idx);
                let next = match data {
                    ArenaItem::Data(ref node) => node.next,
                    _ => None,
                };
                new_arena.push(data);
                at = next;
            }
        }

        // rewrite idx
        for item in new_arena.iter_mut() {
            // item can't be any other variant than Data here
            if let ArenaItem::Data(node) = item {
                node.prev = match node.prev {
                    Some(old_idx) => match self.storage[old_idx] {
                        ArenaItem::NewIndex(new_idx) => Some(new_idx),
                        _ => None,
                    },
                    None => None,
                };
                // perform T's own rewriting
                node.data.rewrite_indices(&self.storage);
                node.next = match node.next {
                    Some(old_idx) => match self.storage[old_idx] {
                        ArenaItem::NewIndex(new_idx) => Some(new_idx),
                        _ => None,
                    },
                    None => None,
                };
            }
        }

        // return old storage for further processing in compaction passes
        std::mem::replace(&mut self.storage, new_arena)
    }
}
