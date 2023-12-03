#![feature(try_trait_v2, slice_index_methods, impl_trait_in_assoc_type)]

use std::{
    convert::Infallible,
    ops::{Bound, ControlFlow, FromResidual, RangeBounds, Try},
};

use container::Container;
use error::ParserError;
use iter::{ParsIter, ParsIterDelim, ParsingIterator};

pub mod container;
pub mod error;
pub mod iter;
mod json;
pub mod parsers;

#[macro_export]
macro_rules! cur {
    ($p:ident <= $($arg:expr),+) => {
        |s| $p($($arg),+, s)
    }
}

pub struct ParserResult<'a, T, E> {
    pub source: &'a str,
    pub typ: ParserResultType<T, E>,
}

impl<'a, T, E> ParserResult<'a, T, E> {
    pub fn unwrap(self) -> T {
        if let ParserResultType::Ok(t) = self.typ {
            t
        } else {
            panic!("unwrap called on erroneous or incomplete parser result")
        }
    }
}

pub enum ParserResultType<T, E> {
    Ok(T),
    Err(E),
    Incomplete,
}

impl<'a, T, E> Try for ParserResult<'a, T, E> {
    type Output = (T, &'a str);

    type Residual = ParserResult<'a, Infallible, E>;

    fn from_output(output: Self::Output) -> Self {
        Self::from_val(output.0, output.1)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self.typ {
            ParserResultType::Ok(v) => ControlFlow::Continue((v, self.source)),
            ParserResultType::Err(e) => ControlFlow::Break(ParserResult::from_err(e, self.source)),
            ParserResultType::Incomplete => {
                ControlFlow::Break(ParserResult::incomplete(self.source))
            }
        }
    }
}

impl<'a, T, E, F: From<E>> FromResidual<ParserResult<'a, Infallible, E>>
    for ParserResult<'a, T, F>
{
    fn from_residual(residual: ParserResult<'a, Infallible, E>) -> Self {
        Self {
            source: residual.source,
            typ: match residual.typ {
                ParserResultType::Ok(_) => unreachable!(),
                ParserResultType::Err(e) => ParserResultType::Err(e.into()),
                ParserResultType::Incomplete => ParserResultType::Incomplete,
            },
        }
    }
}

impl<T, E> ParserResultType<T, E> {
    pub fn as_ref(&self) -> ParserResultType<&T, &E> {
        match self {
            ParserResultType::Ok(v) => ParserResultType::Ok(v),
            ParserResultType::Err(e) => ParserResultType::Err(e),
            ParserResultType::Incomplete => ParserResultType::Incomplete,
        }
    }

    pub fn map<V>(self, f: impl FnOnce(T) -> V) -> ParserResultType<V, E> {
        match self {
            ParserResultType::Ok(t) => ParserResultType::Ok(f(t)),
            ParserResultType::Incomplete => ParserResultType::Incomplete,
            ParserResultType::Err(e) => ParserResultType::Err(e),
        }
    }

    pub fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> ParserResultType<T, E2> {
        match self {
            ParserResultType::Ok(v) => ParserResultType::Ok(v),
            ParserResultType::Err(e) => ParserResultType::Err(f(e)),
            ParserResultType::Incomplete => ParserResultType::Incomplete,
        }
    }
}

pub trait Identity {
    type I;

    fn ident(self) -> Self::I;
}
impl<T> Identity for T {
    type I = T;

    fn ident(self) -> T {
        self
    }
}

impl<'a, T, E> ParserResult<'a, T, E> {
    pub fn from_val(val: T, source: &'a str) -> Self {
        Self {
            source,
            typ: ParserResultType::Ok(val),
        }
    }

    pub fn from_err(err: E, source: &'a str) -> Self {
        Self {
            source,
            typ: ParserResultType::Err(err),
        }
    }

    pub fn incomplete(source: &'a str) -> Self {
        Self {
            source,
            typ: ParserResultType::Incomplete,
        }
    }

    pub fn is_incomplete(&self) -> bool {
        matches!(self.typ, ParserResultType::Incomplete)
    }

    pub fn is_err(&self) -> bool {
        matches!(self.typ, ParserResultType::Err(_))
    }

    pub fn is_ok(&self) -> bool {
        matches!(self.typ, ParserResultType::Ok(_))
    }

    pub fn as_ref(&self) -> ParserResult<'a, &T, &E> {
        ParserResult {
            source: self.source,
            typ: self.typ.as_ref(),
        }
    }

    pub fn ok(self) -> Option<T> {
        match self.typ {
            ParserResultType::Ok(v) => Some(v),
            _ => None,
        }
    }

    pub fn err(self) -> Option<E> {
        match self.typ {
            ParserResultType::Err(e) => Some(e),
            _ => None,
        }
    }

    pub fn optional(self, start: &'a str) -> ParserResult<'a, Option<T>, E> {
        let position = match self.typ {
            ParserResultType::Ok(_) => self.source,
            _ => start,
        };
        ParserResult::from_val(self.ok(), position)
    }

    pub fn map<V>(self, f: impl FnOnce(T) -> V) -> ParserResult<'a, V, E> {
        ParserResult {
            source: self.source,
            typ: self.typ.map(f),
        }
    }

    pub fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> ParserResult<'a, T, E2> {
        ParserResult {
            source: self.source,
            typ: self.typ.map_err(f),
        }
    }

    pub fn or(self, p: impl Parser<'a, T, E>, from: &'a str) -> Self {
        if self.is_ok() {
            self
        } else {
            p.parse(from)
        }
    }

    pub fn and<V>(self, p: impl Parser<'a, V, E>) -> ParserResult<'a, (T, V), E> {
        let (e1, s) = self?;
        let (e2, s) = p.parse(s)?;
        ParserResult::from_val((e1, e2), s)
    }

    pub fn flat_map<V>(
        self,
        p: impl FnOnce(T, &'a str) -> ParserResult<'a, V, E>,
    ) -> ParserResult<'a, V, E> {
        let (val, s) = self?;
        p(val, s)
    }

    pub fn flatten<V>(self) -> ParserResult<'a, V, E>
    where
        T: Identity<I = ParserResult<'a, V, E>>,
    {
        let (res, _) = self?;
        let res: ParserResult<'a, V, E> = res.ident();
        res
    }

    pub fn parsed_slice(&self, original: &'a str) -> &'a str {
        &original[..original.len() - self.source.len()]
    }

    pub fn map_slice<V>(
        self,
        original: &'a str,
        f: impl FnOnce(&'a str) -> V,
    ) -> ParserResult<'a, V, E> {
        ParserResult {
            source: self.source,
            typ: match self.typ {
                ParserResultType::Ok(_) => ParserResultType::Ok(f(self.parsed_slice(original))),
                _ => self.typ.map(|_| unreachable!()),
            },
        }
    }
}

fn is_under(num: usize, bound: Bound<&usize>) -> bool {
    match bound {
        Bound::Included(bound) => num <= *bound,
        Bound::Excluded(bound) => num < *bound,
        Bound::Unbounded => true,
    }
}

pub trait Parser<'a, T, E> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, T, E>;

    fn map<V>(&self, f: impl Fn(T) -> V) -> impl Parser<'a, V, E> {
        move |s| self.parse(s).map(&f)
    }

    fn optional(&self) -> impl Parser<'a, Option<T>, E> {
        |s| self.parse(s).optional(s)
    }

    fn and<V>(&self, other: impl Parser<'a, V, E>) -> impl Parser<'a, (T, V), E> {
        move |s| self.parse(s).and(|s| other.parse(s))
    }

    fn or(&self, other: impl Parser<'a, T, E>) -> impl Parser<'a, T, E> {
        move |s| self.parse(s).or(|s| other.parse(s), s)
    }

    fn repeating(
        &self,
        bounds: impl RangeBounds<usize> + Clone + 'a,
    ) -> impl Parser<'a, Vec<T>, E> {
        move |s| repeating(|s| self.parse(s), bounds.clone(), s)
    }
}

impl<'a, T, E, F> Parser<'a, T, E> for F
where
    F: Fn(&'a str) -> ParserResult<'a, T, E>,
{
    fn parse(&self, input: &'a str) -> ParserResult<'a, T, E> {
        self(input)
    }
}

/// Parse a literal string token
pub fn literal<'a>(
    literal: &'static str,
    input: &'a str,
) -> ParserResult<'a, &'a str, ParserError> {
    if input.starts_with(literal) {
        let (parsed, rest) = input.split_at(literal.len());
        ParserResult::from_val(parsed, rest)
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

pub fn iter_delimited<'a, 'b, Elem: 'a, Delim: 'a, Error: 'a>(
    elem_parser: impl Parser<'a, Elem, Error> + 'b,
    delim_parser: impl Parser<'a, Delim, Error> + 'b,
    source: &'b mut &'a str,
) -> impl ParsingIterator<'a, Elem, Error> + 'b {
    ParsIterDelim {
        phantom: Default::default(),
        source,
        elem_parser,
        delim_parser,
        err: false,
        first: true,
    }
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
