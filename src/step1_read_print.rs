use std::io::Write;

mod core;
mod env;
mod printer;
mod reader;
mod types;

fn READ() -> String {
    let mut buf = String::new();
    let _ = std::io::stdin().read_line(&mut buf);
    buf
}

fn EVAL(input: String) -> Option<types::MalType> {
    reader::read_str(&input)
}

fn PRINT(val: &types::MalType) {
    printer::pr_str(val);
}

fn rep() {
    let buf = READ();
    let val = EVAL(buf);
    match val {
        Some(v) => PRINT(&v),
        None => println!("Eval error!"),
    }
}

fn main() {
    loop {
        print!("\nuser> ");
        std::io::stdout().flush();
        rep();
    }
}
