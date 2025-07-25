use std::fmt::Display;

use crate::{
    common::OpacaError,
    grammar::{lexer::TokenType, parser::Node},
    runtime::module::Module,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpacaType {
    Nothing,
    Bool,
    Int,
    Float,
    String,
    List,
    Function,
    Module,
}

impl Display for OpacaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum OpacaValue {
    Nothing,
    Bool(bool),
    Int(u32),
    Float(f32),
    List(Vec<OpacaValue>),
    Function(OpacaFunction),
    Module(Module),
}

impl OpacaValue {
    pub fn kind(&self) -> OpacaType {
        match self {
            Self::Nothing => OpacaType::Nothing,
            Self::Bool(_) => OpacaType::Bool,
            Self::Int(_) => OpacaType::Int,
            Self::Float(_) => OpacaType::Float,
            Self::List(_) => OpacaType::List,
            Self::Function(_) => OpacaType::Function,
            Self::Module(_) => OpacaType::Module,
        }
    }

    pub fn op(&self, op: &TokenType, other: OpacaValue) -> Result<OpacaValue, OpacaError> {
        match self {
            OpacaValue::Nothing => todo!("add nothing_op"),
            OpacaValue::Bool(_) => todo!("add bool_op"),
            OpacaValue::Int(v) => int_op(*v, op, other),
            OpacaValue::Float(_) => todo!("add float_op"),
            OpacaValue::List(_) => todo!("add list_op"),
            OpacaValue::Function(_) => todo!("add function_op"),
            OpacaValue::Module(_) => todo!("add module_op"),
        }
    }
}

//pub fn get_symbol(&self, )

fn int_op(left: u32, op: &TokenType, right: OpacaValue) -> Result<OpacaValue, OpacaError> {
    todo!("implement int operations")
}

#[derive(Debug, Clone)]
pub enum OpacaFunction {
    Composite((u32, Vec<Node>)),
    Builtin(fn(args: Vec<OpacaValue>, module: &Module) -> Result<OpacaValue, OpacaError>),
}
