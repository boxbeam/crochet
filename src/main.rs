#![feature(lazy_cell, closure_lifetime_binder)]
use std::cell::{LazyCell, OnceCell, RefCell};

use crochet::*;

fn main() {
    let num = "-0";
    // println!("{}", int_parser()(num).unwrap().0);
}

enum Expr {
    Literal(i64),
    Unary(char, Box<Expr>),
    Binary(Box<Expr>, char, Box<Expr>),
}

impl Default for Expr {
    fn default() -> Self {
        Expr::Literal(0)
    }
}

// fn op_parser<'a>() -> impl Parser<'a, (), char, ParserError> {
//     char_match("operator", |c| "+-/*^".contains(c))
// }

// fn term_parser<'a>() -> impl Parser<'a, (), Expr, ParserError> {
//     literal("-")
//         .then(expr_parser().boxed())
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
//     delimited_list(expr_parser().boxed(), op_parser()).map(|mut v| {
//         let (head, rest) = v.split_at_mut(1);
//         let (mut expr, mut op) = std::mem::take(&mut head[0]);
//         for (next_elem, next_op) in rest {
//             expr = Expr::Binary(expr.into(), op.unwrap(), std::mem::take(next_elem).into());
//             if let Some(next_op) = next_op {
//                 op.replace(*next_op);
//             }
//         }
//         expr
//     })
// }
