use std::num::ParseIntError;

use crate::*;

type Result<'a, T> = ParserResult<'a, T, JSONError>;

#[derive(Debug)]
pub enum JSONValue {
    String(String),
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    List(Vec<JSONValue>),
}

#[derive(Debug)]
pub enum JSONError {
    ParseInt(ParseIntError),
    ParserError(ParserError),
    InvalidToken(char),
}

impl From<ParserError> for JSONError {
    fn from(value: ParserError) -> Self {
        JSONError::ParserError(value)
    }
}

fn parse_num(s: &str) -> Result<JSONValue> {
    let (neg, s) = literal("-", s).optional(s)?;
    let neg = if neg.is_some() { -1 } else { 1 };
    let (num, s) =
        take_while("digit", |c| c.is_ascii_digit(), s).map(|s| s.parse::<i64>().unwrap())?;
    if peek(s).ok().is_some_and(|c| c == '.') {
        let (decimal, s) = literal(".", s)
            .and(cur!(take_while <= "digit", |c| c.is_ascii_digit()))
            .map_slice(|s| s.parse::<f64>().unwrap(), s)?;
        let num = num as f64 + decimal;
        ParserResult::from_val(JSONValue::Float(num * neg as f64), s)
    } else {
        ParserResult::from_val(JSONValue::Integer(num * neg), s)
    }
}

fn parse_str(s: &str) -> Result<JSONValue> {
    let (_, mut s) = literal("\"", s)?;
    let string: String = iter(
        |s| parse_esc(s).or(|s| matching_char("char", |c| c != '"', s), s),
        &mut s,
    )
    .ok()
    .collect();
    let (_, s) = literal("\"", s)?;
    ParserResult::from_val(JSONValue::String(string), s)
}

fn parse_esc(s: &str) -> Result<char> {
    let (_, s) = literal("\\", s)?;
    let (c, s) = advance(s)?;
    ParserResult::from_val(
        match c {
            'n' => '\n',
            't' => '\t',
            _ => c,
        },
        s,
    )
}

fn parse_bool(s: &str) -> Result<JSONValue> {
    "true"
        .map(|_| true)
        .or("false".map(|_| false))
        .parse(s)
        .map(JSONValue::Bool)
        .err_into()
}

fn parse_null(s: &str) -> Result<JSONValue> {
    literal("null", s).map(|_| JSONValue::Null).err_into()
}

fn parse_list(s: &str) -> Result<JSONValue> {
    let (_, mut s) = literal("[", s).and(opt_whitespace)?;
    let list = iter_delimited(parse_value, ",".and(opt_whitespace).err_into(), &mut s)
        .ok()
        .collect();
    let (_, s) = literal("]", s)?;
    ParserResult::from_val(JSONValue::List(list), s)
}

pub fn parse_value(s: &str) -> Result<JSONValue> {
    let (c, s) = peek(s)?;
    match c {
        '"' => parse_str(s),
        '-' | '0'..='9' => parse_num(s),
        'n' => parse_null(s),
        't' | 'f' => parse_bool(s),
        '[' => parse_list(s),
        _ => ParserResult::from_err(JSONError::InvalidToken(c), s),
    }
}
