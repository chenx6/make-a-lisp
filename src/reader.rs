use std::rc::Rc;

use lazy_static::lazy_static;
use regex::Regex;

use crate::types::MalType;

lazy_static! {
    static ref RE: Regex =
        Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
            .unwrap();
}

/// Read and get next token string
pub struct Reader {
    tokens: Vec<String>,
    pos: usize,
}

impl Reader {
    fn new(tokens: Vec<String>) -> Reader {
        Reader { tokens, pos: 0 }
    }

    /// Get token from next positon
    fn next(&mut self) -> Option<String> {
        self.pos += 1;
        if let Some(s) = self.tokens.get(self.pos - 1) {
            Some(s.into())
        } else {
            None
        }
    }

    /// Peek a token in current position
    fn peek(&self) -> Option<String> {
        if let Some(s) = self.tokens.get(self.pos) {
            Some(s.into())
        } else {
            None
        }
    }
}

/// Read input, tokenize, return MalType
pub fn read_str(input: &String) -> Option<MalType> {
    let tokens = tokenize(input);
    let mut reader = Reader::new(tokens);
    read_form(&mut reader)
}

/// Use regex to tokenize input
fn tokenize(input: &String) -> Vec<String> {
    let captured = RE.captures_iter(&input);
    captured
        .filter(|c| !c[1].trim().starts_with(";"))
        .map(|c| String::from(&c[1]))
        .collect()
}

fn read_shorthand(reader: &mut Reader, sym_name: &str) -> Option<MalType> {
    let _ = reader.next();
    let sym = match read_form(reader) {
        Some(v) => v,
        None => return None,
    };
    Some(MalType::list(vec![MalType::Sym(sym_name.to_string()), sym]))
}

fn read_form(reader: &mut Reader) -> Option<MalType> {
    let token = match reader.peek() {
        Some(t) => t,
        None => return None,
    };
    match token.as_str() {
        "" => None,
        _ => match token.chars().nth(0) {
            Some('(') => read_list(reader, ")"),
            Some('[') => read_list(reader, "]"),
            Some('"') => read_string(reader),
            Some('@') => read_shorthand(reader, "deref"),
            Some('\'') => read_shorthand(reader, "quote"),
            Some('`') => read_shorthand(reader, "quasiquote"),
            Some('~') => read_shorthand(reader, "unquote"),
            _ => read_atom(reader),
        },
    }
}

fn read_list(reader: &mut Reader, end: &str) -> Option<MalType> {
    reader.next();
    let mut lst: Vec<MalType> = Vec::new();
    while let Some(token) = reader.peek() {
        if token == end {
            break;
        }
        match read_form(reader) {
            Some(v) => lst.push(v),
            None => return None,
        }
    }
    reader.next();
    Some(MalType::List(Rc::new(lst), Rc::new(MalType::Nil)))
}

fn read_atom(reader: &mut Reader) -> Option<MalType> {
    if let Some(token) = reader.next() {
        match token.as_str() {
            "nil" => Some(MalType::Nil),
            "true" => Some(MalType::Bool(true)),
            "false" => Some(MalType::Bool(false)),
            _ => {
                if token.chars().all(|c| c.is_digit(10)) {
                    Some(MalType::Int(token.parse::<i32>().unwrap_or(0)))
                } else {
                    Some(MalType::Sym(token))
                }
            }
        }
    } else {
        None
    }
}

fn read_string(reader: &mut Reader) -> Option<MalType> {
    if let Some(token) = reader.next() {
        match token.chars().nth(0) {
            Some('"') => {
                let token = match token.strip_prefix('"') {
                    Some(s) => s,
                    None => return None,
                };
                let token = match token.strip_suffix('"') {
                    Some(s) => s,
                    None => return None,
                };
                Some(MalType::Str(token.to_string()))
            }
            _ => None,
        }
    } else {
        None
    }
}
