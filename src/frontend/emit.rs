use crate::base::Builder;
/**
 * Original IR generation.
 */
use crate::base::Pass;

pub struct Emit {
    builder: Builder,
}

impl Emit {
    pub fn new(builder: Builder) -> Self {
        Self { builder }
    }
}

impl Pass<()> for Emit {
    fn run(&mut self) -> Result<(), String> {
        todo!()
    }
}
