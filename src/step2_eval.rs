use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

mod core;
mod printer;
mod reader;
mod types;

use types::MalType;

struct EnvStruct {
    data: RefCell<HashMap<String, MalType>>,
    pub outer: Option<Env>,
}

#[derive(Clone)]
struct Env(Rc<EnvStruct>);

impl Env {
    fn new(data: RefCell<HashMap<String, MalType>>, outer: Option<Env>) -> Env {
        Env(Rc::new(EnvStruct { data, outer }))
    }
    fn get(self, sym: &String) -> Option<MalType> {
        match self.0.data.borrow().get(sym) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }
}

fn READ() -> Option<MalType> {
    let mut buf = String::new();
    let _ = std::io::stdin().read_line(&mut buf);
    reader::read_str(&buf)
}

fn EVAL(val: &MalType, env: &Env) -> Option<MalType> {
    match val {
        MalType::List(l, _) if l.len() == 0 => Some(val.clone()),
        MalType::List(_, _) => {
            match eval_ast(val, env) {
                // Take the first item of the evaluated list and call it as function
                // Using the rest of the evaluated list as its arguments.
                Some(MalType::List(v, _)) => match v[0] {
                    MalType::Func(f, _) => match f(v[1..].to_vec()) {
                        Ok(v) => Some(v),
                        Err(_) => None,
                    },
                    _ => None,
                },
                Some(_) | None => None,
            }
        }
        _ => eval_ast(val, env),
    }
}

fn eval_ast(ast: &MalType, env: &Env) -> Option<MalType> {
    match ast {
        MalType::Sym(s) => env.clone().get(s), // JUST CLONE IT!
        MalType::List(l, _) => {
            // Calling EVAL on each of the members of the list
            let eval_lst: Vec<_> = l.iter().filter_map(|v| EVAL(v, env)).collect();
            Some(MalType::List(Rc::new(eval_lst), Rc::new(MalType::Nil)))
        }
        _ => Some(ast.clone()),
    }
}

fn PRINT(val: &MalType) {
    printer::pr_str(val);
}

fn rep(env: &Env) {
    let val = match READ() {
        Some(v) => v,
        None => return,
    };
    let val = EVAL(&val, env);
    match val {
        Some(v) => PRINT(&v),
        None => println!("Eval error!"),
    }
}

fn main() {
    // Init environment
    let mut env: HashMap<String, MalType> = HashMap::new();
    env.insert(
        "+".into(),
        types::MalType::Func(core::add, Rc::new(MalType::Nil)),
    );
    env.insert(
        "-".into(),
        types::MalType::Func(core::sub, Rc::new(MalType::Nil)),
    );
    env.insert(
        "*".into(),
        types::MalType::Func(core::mul, Rc::new(MalType::Nil)),
    );
    env.insert(
        "/".into(),
        types::MalType::Func(core::div, Rc::new(MalType::Nil)),
    );
    let env = RefCell::new(env);
    let env = Env::new(env, None);
    // REPL
    loop {
        print!("\nuser> ");
        let _ = std::io::stdout().flush();
        rep(&env);
    }
}
