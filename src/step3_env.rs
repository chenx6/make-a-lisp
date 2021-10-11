use std::{io::Write, rc::Rc};

mod core;
mod env;
mod printer;
mod reader;
mod types;

use types::MalType;

fn READ() -> Option<MalType> {
    let mut buf = String::new();
    let _ = std::io::stdin().read_line(&mut buf);
    reader::read_str(&buf)
}

fn apply(val: &MalType, env: &mut env::Env) -> Option<MalType> {
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
        Some(_) | None => Some(val.clone()),
    }
}

fn EVAL(val: &MalType, env: &mut env::Env) -> Option<MalType> {
    match val {
        MalType::List(l, _) if l.len() == 0 => Some(val.clone()),
        MalType::List(uneval_lst, _) => match &uneval_lst[0] {
            // Switch on the first element of the list:
            MalType::Sym(s) => match s.as_str() {
                "def!" => {
                    if uneval_lst.len() != 3 {
                        return None;
                    }
                    let evaled = match apply(&uneval_lst[2], env) {
                        Some(v) => v,
                        None => return None,
                    };
                    env.set(uneval_lst[1].clone(), evaled.clone());
                    Some(evaled)
                }
                "let*" => {
                    // Create a new environment
                    // Take the second element of the binding list, call EVAL using the new "let*" environment
                    // then call set on the "let*" environment
                    if uneval_lst.len() < 3 {
                        return None;
                    }
                    let mut new_env = env::Env::new(Some(env.clone()));
                    let bind_lst = match &uneval_lst[1] {
                        MalType::List(l, _) => l,
                        _ => return None,
                    };
                    for i in 0..bind_lst.len() / 2 {
                        if let Some(v) = EVAL(&bind_lst[i + 1], &mut new_env) {
                            new_env.set(bind_lst[i].clone(), v);
                        }
                    }
                    eval_ast(&uneval_lst[2], &mut new_env)
                }
                _ => apply(val, env),
            },
            _ => eval_ast(val, env),
        },
        _ => eval_ast(val, env),
    }
}

fn eval_ast(ast: &MalType, env: &mut env::Env) -> Option<MalType> {
    match ast {
        MalType::Sym(_) => env.clone().get(ast), // JUST CLONE IT!
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

fn rep(env: &mut env::Env) {
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
    let mut repl_env = env::Env::new(None);
    repl_env.set(
        MalType::Sym("+".into()),
        MalType::Func(core::add, Rc::new(MalType::Nil)),
    );
    // REPL
    loop {
        print!("\nuser> ");
        let _ = std::io::stdout().flush();
        rep(&mut repl_env);
    }
}
