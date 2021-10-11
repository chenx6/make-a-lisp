use std::io::Write;

use crate::types::MalType;

pub fn pr_str(val: &MalType) {
    println!("{}", fmt_str(val));
}

fn fmt_str(val: &MalType) -> String {
    match val {
        MalType::Bool(b) => match b {
            true => format!("true"),
            false => format!("false"),
        },
        MalType::Nil => format!("nil"),
        MalType::Sym(s) => format!("{}", s),
        MalType::Int(i) => format!("{}", i),
        MalType::List(l, _) => fmt_seq(l),
        MalType::MalFunc {..} => format!("#<function>"),
        _ => format!("{:?}", val),
    }
}

fn fmt_seq(seq: &Vec<MalType>) -> String {
    let v: Vec<_> = seq.iter().map(|v| fmt_str(v)).collect();
    format!("({})", v.join(" "))
}
