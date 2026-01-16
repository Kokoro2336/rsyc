use std::any::Any;

pub trait Pass<T: Any> {
    fn run(&mut self);
    fn take(self) -> T;
}
