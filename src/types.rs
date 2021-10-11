use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::env::Env;

#[derive(Clone, Debug)]
pub enum MalType {
    Nil,
    Bool(bool),
    Int(i32),
    Float(f64),
    Str(String),
    Sym(String),
    List(Rc<Vec<MalType>>, Rc<MalType>),
    Vector(Rc<Vec<MalType>>, Rc<MalType>),
    Hash(Rc<HashMap<String, MalType>>, Rc<MalType>),
    Func(fn(MalArgs) -> MalRet, Rc<MalType>),
    MalFunc {
        ast: Rc<MalType>,
        env: Env,
        params: Rc<MalType>,
        is_macro: bool,
        meta: Rc<MalType>,
    },
    Atom(Rc<RefCell<MalType>>),
}

#[derive(Debug)]
pub enum MalErr {
    ErrValue(MalType),
    ErrorMsg(String),
    OsErr(String),
}

pub type MalArgs = Vec<MalType>;
pub type MalRet = Result<MalType, MalErr>;

#[inline]
pub fn error_msg(s: &str) -> MalErr {
    MalErr::ErrorMsg(s.into())
}

#[inline]
pub fn error_string(s: String) -> MalErr {
    MalErr::ErrorMsg(s)
}

impl PartialEq for MalType {
    fn eq(&self, other: &MalType) -> bool {
        match (self, other) {
            (MalType::Nil, MalType::Nil) => true,
            (MalType::Bool(ref a), MalType::Bool(ref b)) => a == b,
            (MalType::Int(ref a), MalType::Int(ref b)) => a == b,
            (MalType::Str(ref a), MalType::Str(ref b)) => a == b,
            (MalType::Sym(ref a), MalType::Sym(ref b)) => a == b,
            (MalType::List(ref a, _), MalType::List(ref b, _))
            | (MalType::Vector(ref a, _), MalType::Vector(ref b, _))
            | (MalType::List(ref a, _), MalType::Vector(ref b, _))
            | (MalType::Vector(ref a, _), MalType::List(ref b, _)) => a == b,
            (MalType::Hash(ref a, _), MalType::Hash(ref b, _)) => a == b,
            (MalType::MalFunc { .. }, MalType::MalFunc { .. }) => false,
            _ => false,
        }
    }
}

impl MalType {
    pub fn list(v: Vec<MalType>) -> MalType {
        MalType::List(Rc::new(v), Rc::new(MalType::Nil))
    }

    pub fn atom(v: MalType) -> MalType {
        MalType::Atom(Rc::new(RefCell::new(v)))
    }

    pub fn func(f: fn(args: MalArgs) -> MalRet) -> MalType {
        MalType::Func(f, Rc::new(MalType::Nil))
    }
}
