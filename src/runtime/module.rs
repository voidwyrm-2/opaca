use std::collections::{HashMap, HashSet};

use crate::{common::OpacaError, runtime::types::OpacaValue};

#[derive(Debug, Clone)]
pub struct Module {
    exports: HashSet<(String, u32)>,
    symbols: HashMap<(String, u32), OpacaValue>,
}

impl Module {
    pub fn new(
        exports: HashSet<(String, u32)>,
        symbols: HashMap<(String, u32), OpacaValue>,
    ) -> Module {
        Module {
            exports: exports,
            symbols: symbols,
        }
    }

    pub fn get_symbol_unchecked(&self, sig: &(String, u32)) -> Result<&OpacaValue, OpacaError> {
        if let Some(value) = self.symbols.get(&sig) {
            Ok(value)
        } else {
            Err(OpacaError::from(format!(
                "Symbol {}/{} does not exist",
                sig.0, sig.1
            )))
        }
    }

    pub fn get_symbol(&self, sig: &(String, u32)) -> Result<&OpacaValue, OpacaError> {
        let value = self.get_symbol_unchecked(&sig)?;

        if self.exports.contains(&sig) {
            Ok(value)
        } else {
            Err(OpacaError::from(format!(
                "Symbol {}/{} is not accessible",
                sig.0, sig.1
            )))
        }
    }
}
