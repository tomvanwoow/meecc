use super::errors::Error;
use std::str::Chars;
use std::iter::Peekable;

use ::f128::f128;

use num_enum::{IntoPrimitive, TryFromPrimitive};

pub const KEYWORDS: &[&str] = &[
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",

    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",

    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Generic",
    "_Imaginary",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
];

pub const SYMBOLS: &[&str] = &[
    "...",
    ">>=",
    "<<=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "&=",
    "^=",
    "|=",
    ">>",
    "<<",
    "++",
    "--",
    "->",
    "&&",
    "||",
    "<=",
    ">=",
    "==",
    "!=",
    ";",
    "{",
    "}",
    ",",
    ":",
    "=",
    "(",
    ")",
    "[",
    "]",
    ".",
    "&",
    "!",
    "~",
    "-",
    "+",
    "*",
    "/",
    "%",
    "<",
    ">",
    "^",
    "|",
    "?",
];

#[derive(Clone, Copy, PartialEq, Eq, Debug, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Alignas,
    Alignof,
    Atomic,
    Bool,
    Complex,
    Generic,
    Imaginary,
    Noreturn,
    StaticAssert,
    ThreadLocal,
}

impl From<Keyword> for &'static str {
    fn from(x: Keyword) -> &'static str {
        let x: u8 = x.into();
        KEYWORDS[x as usize]
    }
}

impl Keyword {
    pub fn new(s: &String) -> Option<Keyword> {
        KEYWORDS.iter().position(|&x| x == &s[..]).map(|x| Keyword::try_from_primitive(x as u8).unwrap())
    }

    pub fn to_str(self) -> &'static str {
        From::<Keyword>::from(self)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum Symbol {
    Ellipsis,
    RShiftAssign,
    LShiftAssign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    XorAssign,
    OrAssign,
    RShift,
    LShift,
    Inc,
    Dec,
    Arrow,
    LogicalAnd,
    LogicalOr,
    LE,
    GE,
    EQ,
    NE,
    Semicolon,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Equals,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Dot,
    Ampersand,
    LogicalNot,
    Not,
    Sub,
    Add,
    Asterisk,
    Div,
    Mod,
    LT,
    GT,
    Xor,
    Or,
    QuestionMark,
}

impl From<Symbol> for &'static str {
    fn from(x: Symbol) -> &'static str {
        let x: u8 = x.into();
        SYMBOLS[x as usize]
    }
}

impl Symbol {
    pub fn new(s: &'static str) -> Option<Symbol> {
        SYMBOLS.iter().position(|&x| x == s).map(|x| Symbol::try_from_primitive(x as u8).unwrap())
    }

    pub fn to_str(self) -> &'static str {
        From::<Symbol>::from(self)
    }
}

bitflags! {
    pub struct IntegerSuffix: u8 {
        const NONE     = 0b000;
        const UNSIGNED = 0b001;
        const LONG     = 0b010;
        const LONGLONG = 0b110;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum FloatSuffix {
    None,
    Float,
    Long,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum StringPrefix {
    None,
    UTF8,
    UTF16,
    UTF32,
    Wide,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    Int(u128, IntegerSuffix, bool),
    Float(f128, FloatSuffix),
    Char(char, StringPrefix),
    Str(String, StringPrefix),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Tok {
    Keyword(Keyword),
    Iden(String),
    Symbol(Symbol),
    Literal(Literal),
}

pub fn lex(s: &String) -> Result<Vec<Tok>, Error> {
    Lexer{iter: s.chars().peekable(), line: 1, col: 1}.lex()
}

struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    line: u64,
    col: u64,
}

impl Iterator for Lexer<'_> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        match self.iter.next() {
            Some('\n') => {
                self.line += 1;
                self.col = 1;
                Some('\n')
            }
            Some(c) => {
                self.col += 1;
                Some(c)
            }
            None => None,
        }
    }
}

impl Lexer<'_> {
    fn is_iden_char(c: char) -> bool {
        !c.is_ascii() || c.is_ascii_alphanumeric() || c == '_'
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn lex(&mut self) -> Result<Vec<Tok>, Error> {
        let mut result = Vec::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else if SYMBOLS.iter().any(|&s| s.starts_with(*c)) {
                result.push(Tok::Symbol(self.lex_symbol()?));
            } else {
                result.push(self.lex_iden());
            }
        }
        Ok(result)
    }

    fn lex_while<F: Fn(char) -> bool>(&mut self, f: F) -> String {
        let next_char = |c_option: Option<&char>| {
            if let Some(&c) = c_option {
                return if f(c) { 
                    Some(c) 
                } else { 
                    None 
                };
            }

            None
        };

        let mut s = String::new();
        while let Some(c) = next_char(self.peek()) {
            s.push(c);
            self.next();
        }
        s
    }

    fn lex_if(&mut self, mut tokens: Vec<&'static str>) -> Option<&'static str> {
        let mut iter = itertools::multipeek(self.iter.clone());
        let mut s = String::new();
        let mut result: Option<&'static str> = None;
        while let Some(&c) = iter.peek() {
            s.push(c);
            tokens.retain(|tok| tok.starts_with(&s[..]));
            if tokens.is_empty() {
                break;
            }
            if let Some(s) = tokens.iter().find(|&&tok| s == tok) {
                result = Some(s);
            }
        }
        for _ in 0..(result.map(|x| x.len()).unwrap_or(0)) {
            self.next();
        }
        result
    }

    fn lex_iden(&mut self) -> Tok {
        let s = self.lex_while(Lexer::is_iden_char);
        if let Some(tok) = Keyword::new(&s) {
            Tok::Keyword(tok)
        } else {
            Tok::Iden(s)
        }
    }

    fn lex_symbol(&mut self) -> Result<Symbol, Error> {
        Ok(Symbol::new(self.lex_if(SYMBOLS.iter().map(|&s| s).collect()).unwrap()).unwrap())
    }

    fn lex_number(&mut self) -> Result<Literal, Error> {
        fn lex_digits(this: &mut Lexer, base: u32) -> String {
            this.lex_while(|c| c.is_digit(base))
        }
        
        fn lex_decimal(this: &mut Lexer) -> (Option<String>, Option<String>) {
            (if let Some(&'.') = this.peek() {
                this.next();
                Some(lex_digits(this, 10))
            } else {
                None
            },

            if let Some(&'e') | Some(&'E') = this.peek() {
                this.next();
                if let Some(&'-') = this.peek() {
                    Some(format!("-{}", lex_digits(this, 10)))
                } else {
                    Some(lex_digits(this, 10))
                }
            } else {
                None
            })
        }

        fn lex_integer_suffix(this: &mut Lexer) -> IntegerSuffix {
            let mut result = IntegerSuffix::NONE;
            match this.peek() {
                Some(&'u') | Some(&'U') => {
                    this.next();
                    result |= IntegerSuffix::UNSIGNED;
                },
                _ => {},
            };
            match this.lex_if(vec!["l", "L", "ll", "LL"]) {
                Some("l") | Some("L") => result |= IntegerSuffix::LONG,
                Some("ll") | Some("LL") => result |= IntegerSuffix::LONGLONG,
                _ => {},
            };
            result
        }

        fn lex_float_suffix(this: &mut Lexer) -> FloatSuffix {
            match this.peek() {
                Some(&'f') | Some(&'F') => {
                    this.next();
                    FloatSuffix::Float
                },
                Some(&'l') | Some(&'L') => {
                    this.next();
                    FloatSuffix::Long
                },
                _ => FloatSuffix::None,
            }
        }

        match self.lex_if(vec!["0", "0x", "0X", "0b", "0B"]) {
            Some("0") => {
                let significand = lex_digits(self, 10);
                let a = lex_decimal(self);

                if let (None, None) = a {
                    if !significand.chars().all(|c| c.is_digit(8)) {
                        return Err(Error{line: self.line, col: self.col, msg: format!("Invalid octal literal {}", significand)});
                    }

                    let x = u128::from_str_radix(&significand[..], 8).unwrap();
                    let suffix = lex_integer_suffix(self);

                    return Ok(Literal::Int(x, suffix, false));
                } else {
                    let d = f128::parse(match a {
                        (Some(decimal), Some(exponent)) => format!("{}.{}e{}", significand, decimal, exponent),
                        (Some(decimal), None) => format!("{}.{}", significand, decimal),
                        (None, Some(exponent)) => format!("{}e{}", significand, exponent),
                        _ => unreachable!()
                    }).expect("Unreachable");
                    
                    let suffix = lex_float_suffix(self);
                    return Ok(Literal::Float(d, suffix));
                }
            },
            Some("0x") | Some("0X") => {
                let val = u128::from_str_radix(&lex_digits(self, 16)[..], 16).unwrap();
                let suffix = lex_integer_suffix(self);
                return Ok(Literal::Int(val, suffix, false))
            },
            Some("0b") | Some("0B") => {
                let val = u128::from_str_radix(&lex_digits(self, 2)[..], 2).unwrap();
                let suffix = lex_integer_suffix(self);
                return Ok(Literal::Int(val, suffix, false))

            }
            _ => unreachable!(),
        }
        unreachable!()
    }
}