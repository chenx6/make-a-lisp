use std::rc::Rc;

use crate::{
    printer::pr_str,
    reader::read_str,
    types::{error_msg, error_string, MalArgs, MalErr, MalRet, MalType},
};

pub fn int_operation(args: MalArgs, op: fn(&i32, &i32) -> MalType) -> MalRet {
    if args.len() < 2 {
        return Err(error_msg("int_operation args.len() != 2"));
    }
    match (&args[0], &args[1]) {
        (MalType::Int(a1), MalType::Int(a2)) => Ok(op(a1, a2)),
        _ => Err(error_msg("int_operation argument type is not int")),
    }
}

pub fn add(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Int(a + b))
}

pub fn sub(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Int(a - b))
}

pub fn mul(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Int(a * b))
}

pub fn div(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Int(a / b))
}

pub fn equal(args: MalArgs) -> MalRet {
    Ok(MalType::Bool(args[0] == args[1]))
}

pub fn less(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Bool(a < b))
}

pub fn le(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Bool(a <= b))
}

pub fn great(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Bool(a > b))
}

pub fn ge(args: MalArgs) -> MalRet {
    int_operation(args, |a, b| MalType::Bool(a >= b))
}

pub fn prn(args: MalArgs) -> MalRet {
    if args.len() < 1 {
        return Err(MalErr::ErrValue(MalType::Nil));
    }
    pr_str(&args[0]);
    Ok(MalType::Nil)
}

pub fn list(args: MalArgs) -> MalRet {
    Ok(MalType::List(Rc::new(args), Rc::new(MalType::Nil)))
}

pub fn list_q(args: MalArgs) -> MalRet {
    if args.len() == 1 {
        if let MalType::List(_, _) = &args[0] {
            Ok(MalType::Bool(true))
        } else {
            Ok(MalType::Bool(false))
        }
    } else {
        Ok(MalType::Bool(false))
    }
}

pub fn empty_q(args: MalArgs) -> MalRet {
    match list_q(args.clone()) {
        Ok(MalType::Bool(true)) => match &args[0] {
            MalType::List(l, _) => Ok(MalType::Bool(l.len() == 0)),
            _ => Ok(MalType::Bool(false)),
        },
        _ => Ok(MalType::Bool(false)),
    }
}

pub fn count_q(args: MalArgs) -> MalRet {
    match list_q(args.clone()) {
        Ok(MalType::Bool(true)) => match &args[0] {
            MalType::List(l, _) => Ok(MalType::Int(l.len() as i32)),
            _ => Ok(MalType::Bool(false)),
        },
        _ => Ok(MalType::Bool(false)),
    }
}

pub fn read_string(args: MalArgs) -> MalRet {
    if args.len() != 1 {
        return Err(error_msg("read-string args.len() != 1"));
    }
    if let MalType::Str(s) = &args[0] {
        match read_str(s) {
            Some(r) => Ok(r),
            None => Err(error_msg("read-string read_str error")),
        }
    } else {
        Err(error_msg("read-string first argument is not str"))
    }
}

pub fn slurp(args: MalArgs) -> MalRet {
    if args.len() != 1 {
        return Err(error_msg("slurp args.len() != 1"));
    }
    if let MalType::Str(n) = &args[0] {
        match std::fs::read_to_string(n) {
            Ok(r) => Ok(MalType::Str(r)),
            Err(e) => Err(error_string(format!("slurp {}", e))),
        }
    } else {
        Err(error_msg("slurp first argument is not str"))
    }
}

pub fn _str(args: MalArgs) -> MalRet {
    let mut it = args.iter();
    let mut final_s = String::new();
    while let Some(MalType::Str(s)) = it.next() {
        final_s += s;
    }
    Ok(MalType::Str(final_s))
}

pub fn atom(args: MalArgs) -> MalRet {
    if args.len() != 1 {
        return Err(error_msg("atom args.len() != 1"));
    }
    Ok(MalType::atom(args[0].clone()))
}

pub fn atom_q(args: MalArgs) -> MalRet {
    if args.len() != 1 {
        return Err(error_msg("atom? args.len() != 1"));
    }
    match args[0] {
        MalType::Atom(_) => Ok(MalType::Bool(true)),
        _ => Ok(MalType::Bool(false)),
    }
}

pub fn deref(args: MalArgs) -> MalRet {
    if args.len() != 1 {
        return Err(error_msg("deref args.len() != 1"));
    }
    match &args[0] {
        MalType::Atom(a) => Ok(a.borrow().clone()),
        _ => Err(error_msg("deref first argument is not atom")),
    }
}

pub fn reset_b(args: MalArgs) -> MalRet {
    if args.len() != 2 {
        return Err(error_msg("reset! args.len() != 2"));
    }
    match (&args[0], &args[1]) {
        (MalType::Atom(a), t) => {
            *a.borrow_mut() = t.clone();
            Ok(t.clone())
        }
        _ => Err(error_msg("reset! first argument is not atom")),
    }
}

pub fn cons(args: MalArgs) -> MalRet {
    if args.len() != 2 {
        return Err(error_msg("cons args.len() != 2"));
    }
    match (&args[0], &args[1]) {
        (v, MalType::List(l, _)) => {
            let mut new_l = vec![v.clone()];
            new_l.extend_from_slice(l);
            Ok(MalType::list(new_l))
        }
        _ => Err(error_msg("cons argument type error")),
    }
}

pub fn concat(args: MalArgs) -> MalRet {
    let mut new_l = Vec::new();
    for arg in args.iter() {
        match arg {
            MalType::List(l, _) => new_l.extend_from_slice(l),
            _ => return Err(error_msg("concat type error")),
        }
    }
    Ok(MalType::list(new_l))
}

pub fn nth(args: MalArgs) -> MalRet {
    if args.len() != 2 {
        return Err(error_msg("nth args.len() != 2"));
    }
    match (&args[0], &args[1]) {
        (MalType::List(l, _), MalType::Int(i)) => match l.get(*i as usize) {
            Some(v) => Ok(v.clone()),
            None => Err(error_msg("nth index error")),
        },
        _ => Err(error_msg("nth argument type error")),
    }
}

pub fn first(args: MalArgs) -> MalRet {
    if args.len() != 1 {
        return Err(error_msg("first args.len() != 1"));
    }
    match &args[0] {
        MalType::List(l, _) if l.len() > 0 => Ok(l[0].clone()),
        MalType::List(l, _) if l.len() == 0 => Ok(MalType::Nil),
        MalType::Nil => Ok(MalType::Nil),
        _ => return Err(error_msg("first not a list")),
    }
}
