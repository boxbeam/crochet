#![feature(try_trait_v2)]
// #![allow(dead_code)]

// use std::{
//     cell::LazyCell,
//     sync::{Arc, RwLock},
// };

// use crochet::*;

use crochet::{error::ParserError, parse_literal, parse_matching_char, repeating, ParserResult};

fn main() {
    let s = "-5";
    let n = parse_int(s).unwrap();
    println!("{n}");
}

fn parse_int(input: &str) -> ParserResult<i32, ParserError> {
    parse_literal("-", input)
        .optional(input)
        .and(repeating(
            |s| parse_matching_char("digit", |c| c.is_ascii_digit(), s),
            1..,
        ))
        .map_slice(input, |s| s.parse::<i32>().unwrap())
}

// enum Expr {
//     Literal(i64),
//     Unary(char, Box<Expr>),
//     Binary(Box<Expr>, char, Box<Expr>),
// }

// impl Default for Expr {
//     fn default() -> Self {
//         Expr::Literal(0)
//     }
// }

// fn operate_binary(a: i64, op: char, b: i64) -> i64 {
//     match op {
//         '+' => a + b,
//         '-' => a - b,
//         '/' => a / b,
//         '*' => a * b,
//         c => panic!("illegal operator {c}"),
//     }
// }

// impl Expr {
//     fn eval(&self) -> i64 {
//         match self {
//             Expr::Literal(i) => *i,
//             Expr::Unary(_, e) => -e.eval(),
//             Expr::Binary(a, op, b) => operate_binary(a.eval(), *op, b.eval()),
//         }
//     }
// }

// fn op_parser<'a>() -> impl Parser<'a, (), char, ParserError> {
//     char_match("operator", |c| "+-/*^".contains(c))
// }

// fn term_parser<'a>() -> impl Parser<'a, (), Expr, ParserError> {
//     literal("-")
//         .then(expr_parser())
//         .map(|(_, e)| Expr::Unary('-', e.into()))
//         .or(expr_parser().wrapped("(", ")"))
//         .or(int_literal_parser().map(Expr::Literal))
// }

// fn int_literal_parser<'a>() -> impl Parser<'a, (), i64, ParserError> {
//     literal("-")
//         .optional()
//         .then(char_range("digit", '0'..='9').sequence(1..=10))
//         .slice()
//         .map(|s| s.parse().unwrap())
// }

// fn expr_parser<'a>() -> impl Parser<'a, (), Expr, ParserError> {
//     term_parser()
//         .boxed()
//         .delimited_by(op_parser())
//         .map(|v| v.fold(|a, op, b| Expr::Binary(a.into(), op, b.into())))
// }
