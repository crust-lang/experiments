#[macro_use]
extern crate nom;
extern crate rustyline;

use nom::{IResult,digit,multispace};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::str;
use std::str::FromStr;

named!(pub number<i64>,
       map_res!(
           map_res!(
               digit,
               str::from_utf8
           ),
           FromStr::from_str
       )
);

#[derive(Clone,Copy,Debug,PartialEq)]
pub enum Symbol {
    Plus,
    Minus,
    Mult,
    Div,
}

impl Symbol {
    fn from_char(c: char) -> Result<Self, ()> {
        use Symbol::*;
        match c {
            '+' => Ok(Plus),
            '-' => Ok(Minus),
            '*' => Ok(Mult),
            '/' => Ok(Div),
            _ => Err(()),
        }
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Expression {
    els: Vec<Expr>
}

impl Expression {
    fn from_tuple(els: Vec<Expr>) -> Self {
        Self {
            els: els,
        }
    }
}

#[derive(Clone,Debug,PartialEq)]
pub enum Expr {
    Number(i64),
    Expression(Expression),
}

impl Expr {
    fn from_digit(x: i64) -> Self {
        Expr::Number(x)
    }

    fn from_expression(e: Expression) -> Self {
        Expr::Expression(e)
    }
}

named!(pub symbol<Symbol>,
       map_res!(
           one_of!("+-*/"),
           Symbol::from_char
       )
);

named!(pub expression<Expression>,
       map!(
           delimited!(
               char!('('),
               do_parse!(
                        opt!(multispace) >>
                   els: separated_nonempty_list_complete!(multispace, expr) >>
                        opt!(multispace) >>
                       (els)
               ),
               char!(')')
           ),
           Expression::from_tuple
       )
);

named!(pub expr<Expr>,
       do_parse!(
                opt!(multispace) >>
           exp: alt!(map!(number, Expr::from_digit) |
                     map!(expression, Expr::from_expression)) >>
               (exp)
       )
);

#[derive(Clone,Debug,PartialEq)]
pub enum Error {
    DivideByZero
}

fn map2<F: Fn(i64, i64) -> Result<i64,Error>>(a: Result<i64, Error>, b: Result<i64, Error>, f: F) -> Result<i64, Error> {
    match a {
        Ok(a) => {
            match b {
                Ok(b) => f(a,b),
                Err(_) => b
            }
        },
        Err(_) => a
    }
}

pub fn plus_op(acc: Result<i64,Error>, next: Result<i64,Error>) -> Result<i64, Error> {
    map2(acc, next, |acc, next| {
        Ok(acc + next)
    })
}

pub fn minus_op(acc: Result<i64,Error>, next: Result<i64,Error>) -> Result<i64, Error> {
    map2(acc, next, |acc, next| {
        Ok(acc - next)
    })
}

pub fn mult_op(acc: Result<i64,Error>, next: Result<i64,Error>) -> Result<i64, Error> {
    map2(acc, next, |acc, next| {
        Ok(acc * next)
    })
}

pub fn div_op(acc: Result<i64,Error>, next: Result<i64,Error>) -> Result<i64, Error> {
    map2(acc, next, |acc, next| {
        if next == 0 {
            Err(Error::DivideByZero)
        } else {
            Ok(acc / next)
        }
    })
}

pub fn eval(e: Expr) -> Result<i64, Error> {
    match e {
        Expr::Number(x) => Ok(x),
        Expr::Expression(e) => {
            let mut els : Vec<Result<i64,Error>> = e.els.into_iter().map(eval).collect();
            let first = els.swap_remove(0);
            els.into_iter().fold(first, plus_op)
        }
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history("history.txt") {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("lispy> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match expr(line.as_bytes()) {
                    IResult::Done(_, e) => println!("{:?}", eval(e)),
                    IResult::Incomplete(rest) => println!("Incomplete input: {:?}", rest),
                    IResult::Error(_) => ()
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    fn done<T>(t: T) -> IResult<&'static [u8], T> {
        IResult::Done(&b""[..], t)
    }

    fn done_leftover<T>(l: &'static [u8], t: T) -> IResult<&'static [u8], T> {
        IResult::Done(l, t)
    }

    #[test]
    fn test_parse_number() {
        assert_eq!(expr(b"1"), done(Expr::Number(1)));
        assert_eq!(expr(b"10"), done(Expr::Number(10)));
    }

    #[test]
    fn test_parse_symbol() {
        assert_eq!(symbol(b"+"), done(Symbol::Plus));
        assert_eq!(symbol(b"-"), done(Symbol::Minus));
        assert_eq!(symbol(b"*"), done(Symbol::Mult));
        assert_eq!(symbol(b"/"), done(Symbol::Div));
    }

    #[test]
    fn test_parse_expr() {
        let e = Expr::from_expression(
            Expression::from_tuple(vec![Expr::Number(1),
                                        Expr::Number(2)]));
        assert_eq!(expr(b"(+ 1 2)"), done(e.clone()));
        assert_eq!(expr(b"( + 1 2 )"), done(e.clone()));
        assert_eq!(expr(b"(    +   1    2   )"), done(e.clone()));
        assert_eq!(expr(b"  (    +   1    2   )"), done(e.clone()));
        assert_eq!(expr(b"(    +   1    2   )  "), done_leftover(&b"  "[..], e.clone()));
    }

    fn eval_expr(input: &'static str) -> IResult<&[u8], i64> {
        expr(input.as_bytes()).map(|e| eval(e).unwrap())
    }

    #[test]
    fn test_eval() {
        assert_eq!(eval_expr("1"), done(1));
        assert_eq!(eval_expr("(+ 1 0)"), done(1));
        assert_eq!(eval_expr("(+ 1 (- 5 2) 10)"), done(14));
    }
}
