#![feature(try_trait_v2, slice_index_methods, impl_trait_in_assoc_type)]

use std::{
    fmt::Debug,
    ops::{Bound, RangeBounds},
};

use container::Container;
use error::ParserError;
use iter::{ParsIter, ParsIterDelim, ParsingIterator};
pub use parser::Parser;
pub use parser_result::{ParserResult, ParserResultType};

pub mod container;
pub mod error;
pub mod iter;
pub mod json;
pub mod parser;
pub mod parser_result;
pub mod parsers;

#[macro_export]
macro_rules! cur {
    ($p:ident <= $($arg:expr),+) => {
        |s| $p($($arg),+, s)
    }
}

fn is_under(num: usize, bound: Bound<&usize>) -> bool {
    match bound {
        Bound::Included(bound) => num <= *bound,
        Bound::Excluded(bound) => num < *bound,
        Bound::Unbounded => true,
    }
}

pub struct Literal(pub &'static str);

impl From<&'static str> for Literal {
    fn from(value: &'static str) -> Self {
        Literal(value)
    }
}

/// Parse a literal string token
pub fn literal<'a>(
    literal: impl Into<Literal>,
    input: &'a str,
) -> ParserResult<'a, Literal, ParserError> {
    let Literal(literal) = literal.into();
    if input.starts_with(literal) {
        let (_parsed, rest) = input.split_at(literal.len());
        ParserResult::from_val(Literal(literal), rest)
    } else {
        ParserResult::from_err(ParserError::ExpectedLiteral(literal), input)
    }
}

/// Parse a delimited list of elements using two parsers
pub fn delimited_list<
    'a,
    Elem,
    Delim,
    Error,
    ElemContainer: Container<Elem>,
    DelimContainer: Container<Delim>,
>(
    elem_parser: impl Parser<'a, Elem, Error>,
    delim_parser: impl Parser<'a, Delim, Error>,
    input: &'a str,
) -> ParserResult<'a, (ElemContainer, DelimContainer), Error> {
    let mut elems = ElemContainer::default();
    let mut delims = DelimContainer::default();

    let (first, mut input) = elem_parser.parse(input)?;
    elems.add(first);

    loop {
        let delim = delim_parser.parse(input);
        if !delim.is_ok() {
            break;
        }
        input = delim.source;
        delims.add(delim.unwrap());
        let (elem, new_slice) = elem_parser.parse(input)?;
        input = new_slice;
        elems.add(elem);
    }

    ParserResult::from_val((elems, delims), input)
}

/// Parse a single character matching a predicate
pub fn matching_char<'a>(
    token_name: &'static str,
    filter: impl Fn(char) -> bool,
    input: &'a str,
) -> ParserResult<'a, char, ParserError> {
    match input.chars().next() {
        Some(c) if filter(c) => ParserResult::from_val(c, &input[c.len_utf8()..]),
        _ => ParserResult::from_err(ParserError::ExpectedToken(token_name), input),
    }
}

/// Consume characters as long as they match a predicate
pub fn take_while<'a>(
    token_name: &'static str,
    filter: impl Fn(char) -> bool,
    input: &'a str,
) -> ParserResult<'a, &'a str, ParserError> {
    let len: usize = input
        .chars()
        .take_while(|c| filter(*c))
        .map(|c| c.len_utf8())
        .sum();
    if len == 0 {
        ParserResult::from_err(ParserError::ExpectedToken(token_name), input)
    } else {
        let (parsed, rest) = input.split_at(len);
        ParserResult::from_val(parsed, rest)
    }
}

/// Parse whitespace
pub fn whitespace(input: &str) -> ParserResult<&str, ParserError> {
    take_while("whitespace", char::is_whitespace, input)
}

pub fn opt_whitespace(input: &str) -> ParserResult<Option<&str>, ParserError> {
    whitespace(input).optional(input)
}

/// Parse any number of elements, but at least one
pub fn repeating<'a, T, E>(
    parser: impl Parser<'a, T, E>,
    bounds: impl RangeBounds<usize> + 'a,
    mut source: &'a str,
) -> ParserResult<'a, Vec<T>, E> {
    let mut elems = vec![];
    let mut err = None;
    while is_under(elems.len(), bounds.end_bound()) {
        let parsed = parser.parse(source);
        if let ParserResultType::Ok(v) = parsed.typ {
            elems.push(v);
            source = parsed.source;
        } else {
            err = Some(parsed);
            break;
        }
    }
    if !bounds.contains(&elems.len()) {
        err.expect("error must be present if not enough matches were found")
            .map(|_| unreachable!())
    } else {
        ParserResult::from_val(elems, source)
    }
}

/// Create a [ParsingIterator] from a parser and source slice
pub fn iter<'a, 'b, T: 'a, E: 'a>(
    parser: impl Parser<'a, T, E> + 'b,
    source: &'b mut &'a str,
) -> impl ParsingIterator<'a, T, E> + 'b {
    ParsIter {
        phantom: Default::default(),
        source,
        parser,
        err: false,
    }
}

pub fn iter_delimited<'a, 'b, Elem: 'a, Delim: 'a, Error: 'a, DelimError: Into<Error> + 'a>(
    elem_parser: impl Parser<'a, Elem, Error> + 'b,
    delim_parser: impl Parser<'a, Delim, DelimError> + 'b,
    source: &'b mut &'a str,
) -> impl ParsingIterator<'a, Elem, Error> + 'b {
    ParsIterDelim {
        phantom: Default::default(),
        source,
        elem_parser,
        delim_parser: move |s| delim_parser.parse(s).err_into(),
        err: false,
        first: true,
    }
}

/// Explicitly ignore the output of a parser, advancing the parsing head using a mutable reference
pub fn ignore<'a, 'b, T, E>(
    parser: impl Parser<'a, T, E>,
    input: &'b mut &'a str,
) -> ParserResult<'a, (), E> {
    let res = parser.parse(input);
    *input = res.source;
    res.map(|_| ())
}

/// Check a single character of the input without consuming it
pub fn peek(input: &str) -> ParserResult<char, ParserError> {
    match input.chars().next() {
        Some(c) => ParserResult::from_val(c, input),
        None => ParserResult::from_err(ParserError::UnexpectedEndOfFile, input),
    }
}

/// Consume a single character from the input
pub fn advance(input: &str) -> ParserResult<char, ParserError> {
    match input.chars().next() {
        Some(c) => ParserResult::from_val(c, &input[c.len_utf8()..]),
        None => ParserResult::from_err(ParserError::UnexpectedEndOfFile, input),
    }
}
