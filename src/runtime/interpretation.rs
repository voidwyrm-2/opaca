use std::collections::{HashMap, HashSet};

use crate::{
    common::OpacaError,
    grammar::parser::Node,
    runtime::{
        module::Module,
        stdlib::get_std_module,
        types::{OpacaFunction, OpacaValue},
    },
    token_opacaerr,
};

pub fn resolve_import(
    path: &Node,
    already_imported: &mut HashSet<String>,
) -> Result<(String, Module), OpacaError> {
    if let Node::Use { path, .. } = path {
        let full_path = path
            .iter()
            .map(|t| t.get_typ().get_string().clone())
            .collect::<Vec<String>>()
            .join(".");

        if already_imported.contains(&full_path) {
            return Err(token_opacaerr!(
                path[0],
                "'{}' has already been imported",
                full_path
            ));
        }

        let mut name = String::new();
        let mut cur = Module::new(HashSet::new(), HashMap::new());

        let mut start = 0;

        match path[0].get_typ().get_string().as_str() {
            "std" => {
                name = String::from("std");
                cur = get_std_module()?;
                start = 1;
            }
            _ => (),
        }

        for i in start..path.len() {
            let name = path[i].get_typ().get_string();
            cur = match cur.get_symbol(&(name.clone(), 0)) {
                Ok(v) => match v {
                    OpacaValue::Module(m) => m.clone(),
                    _ => {
                        return Err(token_opacaerr!(
                            path[i],
                            "Cannot directly import symbols that aren't modules",
                        ));
                    }
                },
                Err(e) => return Err(token_opacaerr!(path[i], "{}", e)),
            }
        }

        Ok((name, cur))
    } else {
        panic!("interpretation::resolve_import input must be a Node::Use")
    }
}

pub fn eval_module(module: Node) -> Result<(String, Module), OpacaError> {
    if let Node::Module { name, contents } = module {
        let mut exports: HashSet<(String, u32)> = HashSet::new();
        let mut symbols: HashMap<(String, u32), OpacaValue> = HashMap::new();
        let mut already_imported: HashSet<String> = HashSet::new();

        for node in &contents {
            match node {
                Node::Use { .. } => {
                    let (name, value) = resolve_import(node, &mut already_imported)?;

                    if symbols.contains_key(&(name.clone(), 0)) {
                        return Err(OpacaError::from(format!(
                            "Symbol {}/0 does not exist",
                            name
                        )));
                    }

                    symbols.insert((name, 0), OpacaValue::Module(value));
                }
                Node::Exports { symbols, .. } => {
                    if exports.len() > 0 {
                        return Err(OpacaError::from(format!(
                            "A module cannot contain more than one 'exports' statement",
                        )));
                    }

                    exports = symbols
                        .iter()
                        .map(|s| (s.0.get_typ().get_string().clone(), s.1))
                        .collect();
                }
                Node::Let { name, expr, .. } => {}
                Node::Function {
                    name, params, body, ..
                } => {}
                _ => panic!(
                    "unexpected branch for\n{}\nin interpretation::eval_module",
                    node
                ),
            }
        }

        Ok((
            name.get_typ().get_string().clone(),
            Module::new(exports, symbols),
        ))
    } else {
        panic!("interpretation::eval_module input must be a Node::Module")
    }
}

pub struct Interpreter<'a> {
    module: &'a Module,
}

fn eval_expr(module: &Module, expr: Node) -> Result<OpacaValue, OpacaError> {
    todo!("implement expression evaluation")
}

fn eval_fun(
    module: &Module,
    nodes: Vec<Node>,
    args: Vec<OpacaValue>,
) -> Result<OpacaValue, OpacaError> {
    todo!("implement function execution")
}

pub fn call_fun(
    module: &Module,
    fun: OpacaFunction,
    args: Vec<OpacaValue>,
) -> Result<OpacaValue, OpacaError> {
    match fun {
        OpacaFunction::Builtin(f) => f(args, module),
        OpacaFunction::Composite(nodes) => eval_fun(module, nodes.1, args),
    }
}
