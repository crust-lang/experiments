#[macro_use]
extern crate nom;
extern crate rustyline;

use nom::{IResult,anychar,digit,multispace};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::iter::FromIterator;

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
    Symbol(String),
    Expression(Expression),
}

impl Expr {
    fn from_digit(x: i64) -> Self {
        Expr::Number(x)
    }

    fn from_symbol((s, _): (Vec<char>, char)) -> Self {
        let str : String = String::from_iter(s);
        Expr::Symbol(str)
    }

    fn from_expression(e: Expression) -> Self {
        Expr::Expression(e)
    }

    fn symbol_from_string(s: &str) -> Self {
        Expr::Symbol(s.to_string())
    }
}

named!(pub symbol<Expr>,
       map!(
           many_till!(call!(anychar), peek!(one_of!("() \t\r\n"))),
           Expr::from_symbol
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
       alt!(map!(number, Expr::from_digit) |
            map!(expression, Expr::from_expression) |
            symbol)
);

named!(pub line<Expr>,
       ws!(expr)
);

#[derive(Clone,Debug,PartialEq)]
pub enum Error {
    UnknownFunction,
    DivideByZero,
    MisMatchedArguments(Box<Expr>, Box<Expr>)
}

fn map2<F: Fn(Expr, Expr) -> Result<Expr,Error>>(a: Result<Expr, Error>, b: &Result<Expr, Error>, f: F) -> Result<Expr, Error> {
    match a {
        Ok(a) => {
            match b.clone() {
                Ok(b) => f(a, b),
                b @ Err(_) => b
            }
        },
        Err(_) => a
    }
}

pub fn plus_op(acc: Result<Expr, Error>, next: &Result<Expr, Error>) -> Result<Expr, Error> {
    map2(acc, next, |acc, next| {
        match (acc, next) {
            (Expr::Number(acc), Expr::Number(next)) => Ok(Expr::Number(acc + next)),
            (acc, next) => Err(Error::MisMatchedArguments(Box::new(acc), Box::new(next)))
        }
    })
}

pub fn minus_op(acc: Result<Expr, Error>, next: &Result<Expr, Error>) -> Result<Expr, Error> {
    map2(acc, next, |acc, next| {
        match (acc, next) {
            (Expr::Number(acc), Expr::Number(next)) => Ok(Expr::Number(acc - next)),
            (acc, next) => Err(Error::MisMatchedArguments(Box::new(acc), Box::new(next)))
        }
    })
}

pub fn mult_op(acc: Result<Expr, Error>, next: &Result<Expr, Error>) -> Result<Expr, Error> {
    map2(acc, next, |acc, next| {
        match (acc, next) {
            (Expr::Number(acc), Expr::Number(next)) => Ok(Expr::Number(acc * next)),
            (acc, next) => Err(Error::MisMatchedArguments(Box::new(acc), Box::new(next)))
        }

    })
}

pub fn div_op(acc: Result<Expr, Error>, next: &Result<Expr, Error>) -> Result<Expr, Error> {
    map2(acc, next, |acc, next| {
        match (acc, next) {
            (Expr::Number(acc), Expr::Number(next)) => {
                if next == 0 {
                    Err(Error::DivideByZero)
                } else {
                    Ok(Expr::Number(acc / next))
                }
            }
            (acc, next) => Err(Error::MisMatchedArguments(Box::new(acc), Box::new(next)))
        }
    })
}

pub fn mk_error_op(e: Result<Expr, Error>) -> Box<FnMut(Result<Expr, Error>, &Result<Expr, Error>) -> Result<Expr, Error>> {
    Box::new(move |_, _| e.clone())
}

pub fn fn_from_sym_name(name: &str) -> Box<FnMut(Result<Expr, Error>, &Result<Expr, Error>) -> Result<Expr, Error>> {
    match name {
        "+" => Box::new(plus_op) as Box<FnMut(Result<Expr, Error>, &Result<Expr, Error>) -> Result<Expr, Error>>,
        "-" => Box::new(minus_op) as Box<FnMut(Result<Expr, Error>, &Result<Expr, Error>) -> Result<Expr, Error>>,
        "*" => Box::new(mult_op) as Box<FnMut(Result<Expr, Error>, &Result<Expr, Error>) -> Result<Expr, Error>>,
        "/" => Box::new(div_op) as Box<FnMut(Result<Expr, Error>, &Result<Expr, Error>) -> Result<Expr, Error>>,
        _ => mk_error_op(Err(Error::UnknownFunction))
    }
}

pub fn eval(e: Expr) -> Result<Expr, Error> {
    match e {
        x @ Expr::Number(_) => Ok(x),
        s @ Expr::Symbol(_) => Ok(s),
        Expr::Expression(e) => {
            let mut els : Vec<Result<Expr, Error>> = e.els.into_iter().map(eval).collect();
            let mut op = match els.remove(0) {
                Ok(Expr::Symbol(name)) => fn_from_sym_name(&name),
                e => mk_error_op(e),
            };
            let first = els.remove(0);
            els.iter().fold(first, |x, y| op(x, y))
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
                    IResult::Done(_, e) => {
                        match eval(e) {
                            Ok(res) => println!("{:?}", res),
                            Err(err) => println!("Error: {:?}", err),
                        }
                    },
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
        assert_eq!(symbol(b"+ "), done_leftover(&b" "[..], Expr::symbol_from_string("+")));
        assert_eq!(symbol(b"- "), done_leftover(&b" "[..], Expr::symbol_from_string("-")));
        assert_eq!(symbol(b"* "), done_leftover(&b" "[..], Expr::symbol_from_string("*")));
        assert_eq!(symbol(b"/ "), done_leftover(&b" "[..], Expr::symbol_from_string("/")));
    }

    #[test]
    fn test_parse_expr() {
        let e = Expr::from_expression(
            Expression::from_tuple(vec![Expr::symbol_from_string("+"),
                                        Expr::Number(1),
                                        Expr::Number(2)]));
        assert_eq!(expr(b"(+ 1 2)"), done(e.clone()));
        assert_eq!(expr(b"( + 1 2 )"), done(e.clone()));
        assert_eq!(expr(b"(    +   1    2   )"), done(e.clone()));
        assert_eq!(line(b"  (    +   1    2   )"), done(e.clone()));
        assert_eq!(line(b"(    +   1    2   )  "), done(e.clone()));
    }

    fn eval_expr(input: &'static str) -> IResult<&[u8], Expr> {
        expr(input.as_bytes()).map(|e| eval(e).unwrap())
    }

    #[test]
    fn test_eval() {
        assert_eq!(eval_expr("1"), done(Expr::Number(1)));
        assert_eq!(eval_expr("(+ 1 0)"), done(Expr::Number(1)));
        assert_eq!(eval_expr("(- 5 2)"), done(Expr::Number(3)));
        assert_eq!(eval_expr("(+ 1 (- 5 2) 10)"), done(Expr::Number(14)));
    }
}
