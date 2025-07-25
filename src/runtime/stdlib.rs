use std::collections::{HashMap, HashSet};

use crate::{
    common::OpacaError, runtime::module::Module, runtime::types::OpacaFunction,
    runtime::types::OpacaValue,
};

#[macro_export]
macro_rules! insert_v {
    ($m:expr, $name:expr, $value:expr) => {
        $m.insert(($name.to_string(), 0), $value)
    };
}

#[macro_export]
macro_rules! insert_f {
    ($m:expr, $sig:expr, $func:expr) => {
        $m.insert(
            ($sig.0.to_string(), $sig.1),
            OpacaValue::Function(OpacaFunction::Builtin($func)),
        )
    };
}

#[macro_export]
macro_rules! create_module {
    ($(addf $fsig:expr, $fun:expr)*) => {
        {
            let mut m: HashMap<(String, u32), OpacaValue> = HashMap::new();

            $(insert_f!(m, $fsig, $fun);)*

            m
        }
    };
}

fn get_io() -> Result<Module, OpacaError> {
    let symbols = create_module! {
        addf ("println", 1), io_println_1
    };

    let mut exports: HashSet<(String, u32)> = HashSet::new();

    for k in &symbols {
        exports.insert(k.0.clone());
    }

    Ok(Module::new(exports, symbols))
}

fn io_println_1(args: Vec<OpacaValue>, module: &Module) -> Result<OpacaValue, OpacaError> {
    println!("hello!");
    Ok(OpacaValue::Nothing)
}

pub fn get_std_module() -> Result<Module, OpacaError> {
    let modules = [((String::from("Io"), 0), OpacaValue::Module(get_io()?))];

    let symbols: HashMap<(String, u32), OpacaValue> = HashMap::from(modules);

    let mut exports: HashSet<(String, u32)> = HashSet::new();

    for k in &symbols {
        exports.insert(k.0.clone());
    }

    Ok(Module::new(exports, symbols))
}
