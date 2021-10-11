use ::core::panic;
use std::{io::Write, rc::Rc};

mod core;
mod env;
mod printer;
mod reader;
mod types;

use types::{MalArgs, MalType};

fn READ() -> Option<MalType> {
    let mut buf = String::new();
    let _ = std::io::stdin().read_line(&mut buf);
    reader::read_str(&buf)
}

fn apply(f: &MalType, args: MalArgs) -> Option<MalType> {
    match f {
        MalType::Func(f, _) => match f(args) {
            Ok(r) => Some(r),
            Err(_) => None,
        },
        _ => None,
    }
    
}

fn EVAL(val: &MalType, env: &mut env::Env) -> Option<MalType> {
    let ret_value: Option<MalType>;
    let mut ast = val.clone();
    let mut env = env.clone();

    loop {
        match ast {
            MalType::List(ref l, _) if l.len() == 0 => {
                ret_value = Some(ast);
                break;
            }
            MalType::List(ref uneval_lst, _) => match &uneval_lst[0] {
                // Switch on the first element of the list:
                MalType::Sym(s) if s == "def!" => {
                    if uneval_lst.len() != 3 {
                        return None;
                    }
                    let evaled = match EVAL(&uneval_lst[2], &mut env) {
                        Some(v) => v,
                        None => return None,
                    };
                    env.set(uneval_lst[1].clone(), evaled.clone());
                    ret_value = Some(evaled);
                    break;
                }
                MalType::Sym(s) if s == "let*" => {
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
                    for i in 0..=bind_lst.len() / 2 {
                        if let Some(v) = EVAL(&bind_lst[i + 1], &mut new_env) {
                            new_env.set(bind_lst[i].clone(), v);
                        }
                    }
                    env = new_env;
                    ast = uneval_lst[2].clone();
                    continue;
                }
                MalType::Sym(s) if s == "do" => {
                    match eval_ast(
                        &MalType::List(
                            Rc::new(uneval_lst[1..uneval_lst.len() - 1].to_vec()),
                            Rc::new(MalType::Nil),
                        ),
                        &mut env,
                    ) {
                        Some(MalType::List(_, _)) => {
                            ast = uneval_lst.last().unwrap_or(&MalType::Nil).clone();
                            continue;
                        }
                        _ => {
                            continue;
                        }
                    }
                }
                MalType::Sym(s) if s == "if" => {
                    match EVAL(&uneval_lst[1], &mut env) {
                        Some(MalType::Nil) | Some(MalType::Bool(false)) | None => {
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
                    ret_value = Some(MalType::MalFunc {
                        ast: Rc::new(uneval_lst[2].clone()),
                        env: env.clone(),
                        params: Rc::new(uneval_lst[1].clone()),
                        is_macro: false,
                        meta: Rc::new(MalType::Nil),
                    });
                    break;
                }
                MalType::Sym(s) if s == "eval" => {
                    todo!();
                }
                // _default_
                _ => {
                    let f = eval_ast(&ast, &mut env);
                    // Set ast to the ast attribute of f
                    // Generate a new environment using the env and params attributes of f
                    // Set env to the new environment
                    // Continue at the beginning
                    if let Some(MalType::List(l, _)) = f {
                        match l.get(0) {
                            Some(MalType::MalFunc {
                                ast: closure_ast,
                                env: closure_env,
                                params: closure_params,
                                ..
                            }) if l.len() > 1 => {
                                ast = (&*closure_ast.clone()).clone();
                                let args =
                                    MalType::List(Rc::new(l[1..].to_vec()), Rc::new(MalType::Nil));
                                let closure_params = &*closure_params.clone();
                                env = env::Env::bind(
                                    Some(closure_env.clone()),
                                    closure_params.clone(),
                                    args,
                                );
                            }
                            Some(MalType::Func(_, _)) => {
                                ret_value = apply(&l[0], l[1..].to_vec());
                                break;
                            },
                            _ => (),
                        }
                        continue;
                    } else {
                        panic!("WDNMD");
                    }
                }
            },
            _ => {
                ret_value = eval_ast(&ast, &mut env);
                break;
            }
        };
    }
    ret_value
}

fn eval_ast(ast: &MalType, env: &mut env::Env) -> Option<MalType> {
    match ast {
        MalType::Sym(_) => env.clone().get(ast), // JUST CLONE IT!
        MalType::List(l, _) => {
            // Calling EVAL on each of the members of the list
            let eval_lst: Vec<_> = l.iter().filter_map(|v| EVAL(v, env)).collect();
            let value = MalType::List(Rc::new(eval_lst.clone()), Rc::new(MalType::Nil));
            Some(value)
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
    let mut repl_env = env::Env::init();
    // REPL
    loop {
        print!("\nuser> ");
        let _ = std::io::stdout().flush();
        rep(&mut repl_env);
    }
}
