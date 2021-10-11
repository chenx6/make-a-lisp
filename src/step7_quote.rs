use std::{io::Write, rc::Rc, vec};

mod core;
mod env;
mod printer;
mod reader;
mod types;

use reader::read_str;
use types::{error_msg, MalArgs, MalErr, MalType};

fn READ() -> Option<MalType> {
    let mut buf = String::new();
    let _ = std::io::stdin().read_line(&mut buf);
    reader::read_str(&buf)
}

fn apply(f: &MalType, args: MalArgs) -> Result<MalType, MalErr> {
    match f {
        MalType::Func(f, _) => f(args),
        MalType::MalFunc {
            ast: closure_ast,
            env: closure_env,
            params: closure_params,
            ..
        } => {
            let ast = (&*closure_ast.clone()).clone();
            let closure_params = &*closure_params.clone();
            let mut env = env::Env::bind(
                Some(closure_env.clone()),
                closure_params.clone(),
                MalType::list(args),
            );
            EVAL(&ast, &mut env)
        }
        _ => Err(error_msg("apply first argument is not function")),
    }
}

pub fn swap_b(args: MalArgs) -> Result<MalType, MalErr> {
    if args.len() < 2 {
        return Err(error_msg("swap! args.len() < 2"));
    }
    // (swap! 'atom 'func 'remain_args)
    match (&args[0], &args[1]) {
        (MalType::Atom(a), f) => {
            let a = &**a;
            let mut fargs = vec![a.borrow().clone()];
            if args.len() > 2 {
                fargs.append(&mut args[2..].to_vec());
            }
            let new_value = apply(f, fargs)?;
            *a.borrow_mut() = new_value.clone();
            Ok(new_value)
        }
        _ => Err(error_msg("swap! first argument is not atom")),
    }
}

/// Check if `ast` is a list and starts with `cmp_s` sym
fn starts_with(ast: &MalType, cmp_s: &str) -> bool {
    match ast {
        MalType::List(l, _) if l.len() > 0 => match l[0] {
            MalType::Sym(ref s) if s == cmp_s => true,
            _ => false,
        },
        _ => false,
    }
}

fn quasiquote_iter(elts: &MalArgs) -> MalType {
    let mut acc = MalType::list(vec![]);
    for elt in elts.iter().rev() {
        if let MalType::List(v, _) = elt {
            if starts_with(elt, "splice-unquote") && v.len() > 1 {
                acc = MalType::list(vec![MalType::Sym("concat".into()), v[1].clone(), acc]);
                continue;
            }
        }
        acc = MalType::list(vec![MalType::Sym("cons".into()), quasiquote(&elt), acc]);
    }
    return acc;
}

fn quasiquote(ast: &MalType) -> MalType {
    match ast {
        MalType::List(v, _) => {
            if starts_with(ast, "unquote") {
                return v[1].clone();
            }
            return quasiquote_iter(&v);
        }
        MalType::Hash(_, _) | MalType::Sym(_) => {
            return MalType::list(vec![MalType::Sym("quote".into()), ast.clone()])
        }
        _ => ast.clone(),
    }
}

pub fn EVAL(val: &MalType, env: &mut env::Env) -> Result<MalType, MalErr> {
    let mut ast = val.clone();
    let mut env = env.clone();

    loop {
        match ast {
            MalType::List(ref l, _) if l.len() == 0 => {
                return Ok(ast);
            }
            MalType::List(ref uneval_lst, _) => match &uneval_lst[0] {
                // Switch on the first element of the list:
                MalType::Sym(s) if s == "def!" => {
                    if uneval_lst.len() != 3 {
                        return Err(error_msg("def! len() != 3"));
                    }
                    let evaled = EVAL(&uneval_lst[2], &mut env)?;
                    env.set(uneval_lst[1].clone(), evaled.clone());
                    return Ok(evaled);
                }
                MalType::Sym(s) if s == "let*" => {
                    // Create a new environment
                    // Take the second element of the binding list, call EVAL using the new "let*" environment
                    // then call set on the "let*" environment
                    if uneval_lst.len() < 3 {
                        return Err(error_msg("let* len() != 3"));
                    }
                    let mut new_env = env::Env::new(Some(env.clone()));
                    let bind_lst = match &uneval_lst[1] {
                        MalType::List(l, _) => l,
                        _ => return Err(error_msg("let* argument[1] type error")),
                    };
                    for i in (0..bind_lst.len()).filter(|v| v & 1 == 0) {
                        if let Ok(v) = EVAL(&bind_lst[i + 1], &mut new_env) {
                            new_env.set(bind_lst[i].clone(), v);
                        }
                    }
                    env = new_env;
                    ast = uneval_lst[2].clone();
                    continue;
                }
                MalType::Sym(s) if s == "do" => {
                    // Evaluate all the elements of the list
                    match eval_ast(
                        &MalType::list(uneval_lst[1..uneval_lst.len() - 1].to_vec()),
                        &mut env,
                    ) {
                        Ok(MalType::List(_, _)) => {
                            ast = uneval_lst.last().unwrap_or(&MalType::Nil).clone();
                        }
                        _ => (),
                    };
                    continue;
                }
                MalType::Sym(s) if s == "if" => {
                    // Evaluate the first parameter (second element).
                    // If the result (condition) is anything other than nil or false
                    //   then evaluate the second parameter (third element of the list) and return the result.
                    match EVAL(&uneval_lst[1], &mut env)? {
                        MalType::Nil | MalType::Bool(false) => {
                            if uneval_lst.len() != 4 {
                                ast = MalType::Nil;
                            } else {
                                ast = uneval_lst[3].clone();
                            }
                        }
                        _ => ast = uneval_lst[2].clone(),
                    }
                    continue;
                }
                MalType::Sym(s) if s == "fn*" => {
                    // Return a new function closure
                    // Create a new environment
                    return Ok(MalType::MalFunc {
                        ast: Rc::new(uneval_lst[2].clone()),
                        env: env.clone(),
                        params: Rc::new(uneval_lst[1].clone()),
                        is_macro: false,
                        meta: Rc::new(MalType::Nil),
                    });
                }
                MalType::Sym(s) if s == "eval" => {
                    let arg_ast = &uneval_lst[1];
                    if let Ok(a) = EVAL(arg_ast, &mut env) {
                        ast = a;
                    }
                    continue;
                }
                MalType::Sym(s) if s == "quote" => return Ok(uneval_lst[1].clone()),
                MalType::Sym(s) if s == "quasiquote" => {
                    ast = quasiquote(&uneval_lst[1]);
                    continue;
                }
                // _default_
                _ => {
                    let f = eval_ast(&ast, &mut env);
                    // Set ast to the ast attribute of f
                    // Generate a new environment using the env and params attributes of f
                    // Set env to the new environment
                    // Continue at the beginning
                    if let Ok(MalType::List(l, _)) = f {
                        match l.get(0) {
                            Some(MalType::MalFunc {
                                ast: closure_ast,
                                env: closure_env,
                                params: closure_params,
                                ..
                            }) => {
                                ast = (&*closure_ast.clone()).clone();
                                let args = if l.len() > 1 {
                                    MalType::list(l[1..].to_vec())
                                } else {
                                    MalType::list(vec![])
                                };
                                let closure_params = &*closure_params.clone();
                                env = env::Env::bind(
                                    Some(closure_env.clone()),
                                    closure_params.clone(),
                                    args,
                                );
                            }
                            Some(MalType::Func(_, _)) => {
                                return apply(&l[0], l[1..].to_vec());
                            }
                            _ => {
                                return Err(error_msg("_default_ first argument is not function"));
                            }
                        }
                        continue;
                    } else {
                        return Err(error_msg("_default_ resolve error"));
                    }
                }
            },
            _ => {
                return eval_ast(&ast, &mut env);
            }
        };
    }
}

fn eval_ast(ast: &MalType, env: &mut env::Env) -> Result<MalType, MalErr> {
    match ast {
        MalType::Sym(_) => match env.clone().get(ast) {
            Some(v) => Ok(v),
            None => Err(error_msg("eval_ast cannot get sym")),
        }, // JUST CLONE IT!
        MalType::List(l, _) => {
            // Calling EVAL on each of the members of the list
            let eval_lst: Vec<_> = l.iter().filter_map(|v| EVAL(v, env).ok()).collect();
            Ok(MalType::list(eval_lst))
        }
        _ => Ok(ast.clone()),
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
        Ok(v) => PRINT(&v),
        Err(e) => println!("Eval error!, {:?}", e),
    }
}

fn main() {
    // Init environment
    let mut repl_env = env::Env::init();
    repl_env.set(MalType::Sym("swap!".into()), MalType::func(swap_b));
    // Argument
    let args: Vec<_> = std::env::args().map(MalType::Str).collect();
    repl_env.set(MalType::Sym("*ARGV*".into()), MalType::list(args));
    // Functions
    let f = match read_str(
        &"(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"nil)\")))))"
            .to_string(),
    ) {
        Some(v) => v,
        None => return,
    };
    let _ = EVAL(&f, &mut repl_env);
    // REPL
    loop {
        print!("user> ");
        let _ = std::io::stdout().flush();
        rep(&mut repl_env);
    }
}
