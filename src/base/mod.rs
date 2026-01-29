pub use crate::base::r#type::Type;

mod r#type;
mod builder;
mod context;
mod pass;
pub use crate::base::builder::*;
pub use crate::base::context::*;
pub use crate::base::pass::*;

mod bb;
mod op;
pub mod ir {
    pub use crate::base::bb::*;
    pub use crate::base::op::*;
}
