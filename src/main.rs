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
    operator: Symbol,
    operands: Vec<Expr>
}

impl Expression {
    fn from_tuple(t: (Symbol, Vec<Expr>)) -> Self {
        let (operator, operands) = t;
        Self {
            operator: operator,
            operands: operands,
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

named!(pub operator<Symbol>,
       map_res!(
           one_of!("+-*/"),
           Symbol::from_char
       )
);

named!(pub expression<Expression>,
       map!(
           do_parse!(
               opt!(multispace) >>
                   char!('(') >>
                   opt!(multispace) >>
                   operator: operator >>
                   opt!(multispace) >>
                   operands: separated_nonempty_list_complete!(multispace, expr) >>
                   opt!(multispace) >>
                   char!(')') >>
                   (operator, operands)
           ),
           Expression::from_tuple
       )
);

named!(pub expr<Expr>,
       alt!(map!(number, Expr::from_digit) |
            map!(expression, Expr::from_expression))
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
            let op_fn = match e.operator {
                Symbol::Plus  => plus_op  as fn(Result<i64,Error>, Result<i64,Error>) -> Result<i64, Error>,
                Symbol::Minus => minus_op as fn(Result<i64,Error>, Result<i64,Error>) -> Result<i64, Error>,
                Symbol::Mult  => mult_op  as fn(Result<i64,Error>, Result<i64,Error>) -> Result<i64, Error>,
                Symbol::Div   => div_op   as fn(Result<i64,Error>, Result<i64,Error>) -> Result<i64, Error>,
            };
            let mut operands : Vec<Result<i64,Error>> = e.operands.into_iter().map(eval).collect();
            let first = operands.swap_remove(0);
            operands.into_iter().fold(first, op_fn)
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
                    IResult::Done(_, e) => println!("No you're a {:?}", eval(e)),
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

    #[test]
    fn test_parse_number() {
        assert_eq!(expr(b"1"), IResult::Done(&b""[..], Expr::Number(1)));
        assert_eq!(expr(b"10"), IResult::Done(&b""[..], Expr::Number(10)));
    }

    #[test]
    fn test_parse_operator() {
        assert_eq!(operator(b"+"), IResult::Done(&b""[..], Symbol::Plus));
        assert_eq!(operator(b"-"), IResult::Done(&b""[..], Symbol::Minus));
        assert_eq!(operator(b"*"), IResult::Done(&b""[..], Symbol::Mult));
        assert_eq!(operator(b"/"), IResult::Done(&b""[..], Symbol::Div));
    }

    #[test]
    fn test_parse_expr() {
        let e = Expr::from_expression(
            Expression::from_tuple((Symbol::Plus, vec![Expr::Number(1),
                                                         Expr::Number(2)])));
        assert_eq!(expr(b"(+ 1 2)"), IResult::Done(&b""[..], e.clone()));
        assert_eq!(expr(b"( + 1 2 )"), IResult::Done(&b""[..], e.clone()));
        assert_eq!(expr(b"(    +   1    2   )"), IResult::Done(&b""[..], e.clone()));
        assert_eq!(expr(b"  (    +   1    2   )"), IResult::Done(&b""[..], e.clone()));
    }

    fn eval_expr(input: &str) -> i64 {
        let (_, e) = expr(input.as_bytes()).unwrap();
        eval(e).unwrap()
    }

    #[test]
    fn test_eval() {
        assert_eq!(eval_expr("1"), 1);
        assert_eq!(eval_expr("(+ 1 0)"), 1);
        assert_eq!(eval_expr("(+ 1 (- 5 2) 10)"), 14);
    }
}
