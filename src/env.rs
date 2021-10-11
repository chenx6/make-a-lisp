use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::core::*;
use crate::types::MalType;

#[derive(Debug)]
pub struct EnvStruct {
    data: RefCell<HashMap<String, MalType>>,
    pub outer: Option<Env>,
}

#[derive(Clone, Debug)]
pub struct Env(Rc<EnvStruct>);

impl Env {
    pub fn init() -> Env {
        let mut env: HashMap<String, MalType> = HashMap::new();
        env.insert("+".into(), MalType::func(add));
        env.insert("-".into(), MalType::func(sub));
        env.insert("*".into(), MalType::func(mul));
        env.insert("/".into(), MalType::func(div));
        env.insert("=".into(), MalType::func(equal));
        env.insert("<".into(), MalType::func(less));
        env.insert("<=".into(), MalType::func(le));
        env.insert(">".into(), MalType::func(great));
        env.insert(">=".into(), MalType::func(ge));
        env.insert("list".into(), MalType::func(list));
        env.insert("list?".into(), MalType::func(list_q));
        env.insert("empty?".into(), MalType::func(empty_q));
        env.insert("count".into(), MalType::func(count_q));
        env.insert("read-string".into(), MalType::func(read_string));
        env.insert("slurp".into(), MalType::func(slurp));
        env.insert("str".into(), MalType::func(_str));
        env.insert("prn".into(), MalType::func(prn));
        env.insert("atom".into(), MalType::func(atom));
        env.insert("atom?".into(), MalType::func(atom_q));
        env.insert("deref".into(), MalType::func(deref));
        env.insert("reset!".into(), MalType::func(reset_b));
        env.insert("concat".into(), MalType::func(concat));
        env.insert("cons".into(), MalType::func(cons));
        Env(Rc::new(EnvStruct {
            data: RefCell::new(env),
            outer: None,
        }))
    }

    pub fn new(outer: Option<Env>) -> Env {
        Env(Rc::new(EnvStruct {
            data: RefCell::new(HashMap::new()),
            outer,
        }))
    }

    pub fn bind(outer: Option<Env>, binds: MalType, exprs: MalType) -> Env {
        let mut env = Self::new(outer);
        if let (MalType::List(bl, _), MalType::List(el, _)) = (binds, exprs) {
            bl.iter().zip(el.iter()).for_each(|(b, e)| {
                env.set(b.clone(), e.clone());
            })
        }
        env
    }

    fn find(&self, key: &MalType) -> Option<Env> {
        let key_s = match key {
            MalType::Sym(s) => s,
            _ => return None,
        };
        match (
            self.0.data.borrow().contains_key(key_s),
            self.0.outer.clone(),
        ) {
            (true, _) => Some(self.clone()),
            (false, Some(o)) => o.find(key),
            _ => None,
        }
    }

    /// Get symbol from current environment
    pub fn get(&self, key: &MalType) -> Option<MalType> {
        let key_s = match key {
            MalType::Sym(s) => s,
            _ => return None,
        };
        match self.find(key) {
            Some(e) => Some(e.0.data.borrow().get(key_s).unwrap().clone()),
            None => None,
        }
    }

    /// Set a new symbol
    pub fn set(&mut self, key: MalType, value: MalType) -> Option<MalType> {
        match key {
            MalType::Sym(s) => {
                self.0.data.borrow_mut().insert(s, value.clone()).or(Some(value))
            },
            _ => None,
        }
    }

    pub fn outer(&self) -> Option<Env> {
        self.0.outer.clone()
    }
}
