use crate::arith::AST::{BinOp, Num};
use crate::arith::Operator::{Div, Minus, Mult, Plus};

#[derive(Eq, PartialEq, Debug)]
enum Operator {
    Plus,
    Mult,
    Minus,
    Div,
}

#[derive(Eq, PartialEq, Debug)]
enum AST {
    Num(i32),
    BinOp(Operator, Box<AST>, Box<AST>),
}

fn parse_num(s: &str) -> Option<(AST, &str)> {
    let pos = s.find(|c: char| !c.is_digit(10)).unwrap_or_else(|| s.len());
    let ans = s.split_at(pos);
    Some((Num(ans.0.parse().ok()?), ans.1))
}

fn parse_op(s: &str) -> Option<(Operator, &str)> {
    match s.chars().next()? {
        '*' => Some((Operator::Mult, s.split_at(1).1)),
        '+' => Some((Operator::Plus, s.split_at(1).1)),
        '-' => Some((Operator::Minus, s.split_at(1).1)),
        '/' => Some((Operator::Div, s.split_at(1).1)),
        _ => None
    }
}

fn parse_mult(s: &str) -> Option<(AST, &str)> {
    let (l, rest) = parse_num(s)?;
    match parse_op(rest) {
        None => Some((l, rest)),
        Some((op, rest1)) if op == Mult || op == Div => {
            let (r, rest2) = parse_mult(rest1)?;
            Some((BinOp(op, l.into(), r.into()), rest2))
        }
        _ => Some((l, rest))
    }
}

fn parse_sum(s: &str) -> Option<(AST, &str)> {
    let (l, rest) = parse_mult(s)?;

    match parse_op(rest) {
        Some((op, rest1)) if op == Plus || op == Minus => {
            let (r, rest2) = parse_sum(rest1)?;
            Some((BinOp(op, l.into(), r.into()), rest2))
        }
        _ => Some((l, rest))
    }
}

fn to_postfix_buffered(t: AST, s: &mut String) {
    match t {
        Num(x) => s.push_str(&format!("{} ", x)),
        BinOp(op, l, r) => {
            to_postfix_buffered(*l, s);
            to_postfix_buffered(*r, s);
            s.push(match op {
                Plus => '+',
                Minus => '-',
                Mult => '*',
                Div => '/'
            });
            s.push(' ');
        }
    }
}

fn to_postfix(t: AST) -> String {
    let mut ans = String::new();
    to_postfix_buffered(t, &mut ans);
    ans.pop();
    ans
}

fn from_postfix(s: &str) -> Option<AST> {
    let mut t: Vec<AST> = vec![];
    for tok in s.split(' ') {
        match tok {
            "+" | "-" | "*" | "/" => {
                let r = t.pop()?;
                let l = t.pop()?;
                t.push(BinOp(match tok {
                    "+" => Plus,
                    "-" => Minus,
                    "/" => Div,
                    "*" => Mult,
                    _ => unreachable!()
                }, l.into(), r.into()))
            }
            _ => t.push(Num(tok.parse().ok()?))
        }
    }
    if t.len() == 1 {
        Some(t.pop().unwrap())
    } else { None }
}

#[cfg(test)]
mod tests {
    use crate::arith::*;
    use crate::arith::AST::*;
    use crate::arith::Operator::*;

    #[test]
    fn test_parse_num() {
        assert_eq!(parse_num("7"), Some((Num(7), "")));
        assert_eq!(parse_num("12+3"), Some((Num(12), "+3")));
        assert_eq!(parse_num("007"), Some((Num(7), "")));
        assert_eq!(parse_num("+3"), None);
        assert_eq!(parse_num("a"), None);
    }

    #[test]
    fn test_parse_op() {
        assert_eq!(parse_op("+1"), Some((Plus, "1")));
        assert_eq!(parse_op("**"), Some((Mult, "*")));
        assert_eq!(parse_op("-2"), Some((Minus, "2")));
        assert_eq!(parse_op("/1"), Some((Div, "1")));
        assert_eq!(parse_op("12"), None);
    }

    #[test]
    fn test_parse_mult() {
        assert_eq!(parse_mult("1*2*3"),
                   Some((BinOp(Mult, Num(1).into(), BinOp(Mult, Num(2).into(), Num(3).into()).into()), "")));
        assert_eq!(parse_mult("123"), Some((Num(123), "")));
        assert_eq!(parse_mult("1*2+3*4"), Some((BinOp(Mult, Num(1).into(), Num(2).into()), "+3*4")));
    }

    #[test]
    fn test_parse_sum() {
        assert_eq!(parse_sum("1*2*3"),
                   Some((BinOp(Mult, Num(1).into(), BinOp(Mult, Num(2).into(), Num(3).into()).into()), "")));
        assert_eq!(parse_sum("123"), Some((Num(123), "")));
        assert_eq!(
            parse_sum("1*2+3*4"),
            Some((
                BinOp(Plus,
                      BinOp(Mult, Num(1).into(), Num(2).into()).into(),
                      BinOp(Mult, Num(3).into(), Num(4).into()).into()),
                "")
            )
        );
        assert_eq!(
            parse_sum("1+2*3+4"),
            Some((
                BinOp(Plus,
                      Num(1).into(),
                      BinOp(Plus,
                            BinOp(Mult, Num(2).into(), Num(3).into()).into(),
                            Num(4).into()).into()),
                "")
            )
        );
    }

    #[test]
    fn test_to_postfix() {
        assert_eq!(to_postfix(Num(123)), "123");
        assert_eq!(to_postfix(BinOp(Plus, Num(13).into(), Num(42).into())), "13 42 +");
        assert_eq!(to_postfix(BinOp(
            Plus,
            BinOp(Mult, Num(1).into(), Num(2).into()).into(),
            Num(3).into())), "1 2 * 3 +");
        assert_eq!(to_postfix(BinOp(
            Plus,
            BinOp(
                Mult,
                BinOp(
                    Minus,
                    Num(1).into(),
                    Num(2).into(),
                ).into(),
                BinOp(
                    Div,
                    Num(3).into(),
                    Num(4).into(),
                ).into(),
            ).into(),
            BinOp(
                Plus,
                Num(5).into(),
                BinOp(
                    Minus,
                    Num(6).into(),
                    Num(7).into(),
                ).into(),
            ).into(),
        )), "1 2 - 3 4 / * 5 6 7 - + +")
    }

    #[test]
    fn test_from_postfix() {
        assert_eq!(from_postfix("123"), Some(Num(123)));
        assert_eq!(from_postfix("1 2 * 3 +"), Some(BinOp(
            Plus,
            BinOp(Mult, Num(1).into(), Num(2).into()).into(),
            Num(3).into())));
        assert_eq!(from_postfix("13 42 +"), Some(BinOp(Plus, Num(13).into(), Num(42).into())));
        assert_eq!(from_postfix("1 2 - 3 4 / * 5 6 7 - + +"), Some(BinOp(
            Plus,
            BinOp(
                Mult,
                BinOp(
                    Minus,
                    Num(1).into(),
                    Num(2).into(),
                ).into(),
                BinOp(
                    Div,
                    Num(3).into(),
                    Num(4).into(),
                ).into(),
            ).into(),
            BinOp(
                Plus,
                Num(5).into(),
                BinOp(
                    Minus,
                    Num(6).into(),
                    Num(7).into(),
                ).into(),
            ).into(),
        )));
        assert_eq!(from_postfix("1 2 3 +"), None);
        assert_eq!(from_postfix("1 2 + *"), None);
    }
}