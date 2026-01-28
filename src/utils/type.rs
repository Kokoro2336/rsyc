use crate::frontend::ast::Node;
use std::any::Any;

pub fn cast<T: Any>(input: &dyn Any) -> Option<&T> {
    input.downcast_ref::<T>()
}

pub fn cast_mut<T: Any>(input: &mut dyn Any) -> Option<&mut T> {
    input.downcast_mut::<T>()
}

pub fn cast_deref<T: Any>(input: Box<dyn Any>) -> Option<Box<T>> {
    if input.is::<T>() {
        Some(input.downcast::<T>().ok().unwrap())
    } else {
        None
    }
}

pub fn is<T: Any>(input: &dyn Any) -> bool {
    input.is::<T>()
}

pub fn replace(input: &mut Box<dyn Node>, new: Box<dyn Node>) -> Box<dyn Node> {
    std::mem::replace(input, new)
}

pub fn take(input: &mut Box<dyn Node>) -> Box<dyn Node> {
    std::mem::take(input)
}
