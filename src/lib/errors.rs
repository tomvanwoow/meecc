use std::fmt;
use colored::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    pub msg: String,
    pub line: u64,
    pub col: u64,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}: {}: {} ", self.line, self.col, "error".red(), self.msg)
    }
}
