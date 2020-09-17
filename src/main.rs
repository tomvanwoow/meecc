#[macro_use]

extern crate bitflags;

mod lib;
use lib::lexer::*;

fn main() {
    let a = lex(&String::from("int"));
    match a {
        Ok(toks) => for tok in toks {
            println!("{:?}", tok);
        },
        Err(err) => println!("{}", err),
    }
}
