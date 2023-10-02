#![feature(lazy_cell, closure_lifetime_binder, type_alias_impl_trait)]
#![allow(dead_code)]

use crochet::*;

fn main() {
    // let num = "-0";
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

fn op_parser<'a>() -> impl Parser<'a, (), char, ParserError> {
    char_match("operator", |c| "+-/*^".contains(c))
}

fn term_parser<'a>() -> impl Parser<'a, (), Expr, ParserError> {
    literal("-")
        .then(expr_parser().boxed())
        .map(|(_, e)| Expr::Unary('-', e.into()))
        .or(expr_parser().wrapped("(", ")"))
        .or(int_literal_parser().map(Expr::Literal))
}

fn int_literal_parser<'a>() -> impl Parser<'a, (), i64, ParserError> {
    literal("-")
        .optional()
        .then(char_range("digit", '0'..='9').sequence(1..=10))
        .slice()
        .map(|s| s.parse().unwrap())
}

fn expr_parser<'a>() -> impl Parser<'a, (), Expr, ParserError> {
    term_parser()
        .boxed()
        .delimited_by(op_parser())
        .map(|v| v.fold(|a, op, b| Expr::Binary(a.into(), op, b.into())))
}
