use std::collections::HashMap;

use crate::{grammar::parser::Node, runtime::types::OpacaValue};

#[derive(Debug, Clone)]
pub struct Module {
    imports: Vec<Node>,
    exports: HashMap<String, u32>,
    symbols: HashMap<(String, u32), OpacaValue>,
}

impl Module {
    pub fn new(
        imports: Vec<Node>,
        exports: HashMap<String, u32>,
        symbols: HashMap<(String, u32), OpacaValue>,
    ) -> Module {
        Module {
            imports: imports,
            exports: exports,
            symbols: symbols,
        }
    }
}
