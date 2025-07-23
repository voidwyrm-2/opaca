use std::{
    error::Error,
    fmt::{self, Debug, Display},
};

pub struct OpacaError {
    msg: String,
}

impl From<&str> for OpacaError {
    fn from(value: &str) -> Self {
        OpacaError {
            msg: value.to_string(),
        }
    }
}

impl From<String> for OpacaError {
    fn from(value: String) -> Self {
        OpacaError { msg: value }
    }
}

impl From<fmt::Error> for OpacaError {
    fn from(value: fmt::Error) -> Self {
        OpacaError::from(format!("{}", value))
    }
}

impl Debug for OpacaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Display for OpacaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for OpacaError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

pub enum Pair<T, U> {
    A(T),
    B(U),
}

pub fn try_index<T>(vec: &Vec<T>, index: usize) -> Option<&T> {
    if index < vec.len() {
        Some(&vec[index])
    } else {
        None
    }
}

pub fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}
