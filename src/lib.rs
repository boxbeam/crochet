#![feature(
    closure_lifetime_binder,
    lazy_cell,
    return_position_impl_trait_in_trait,
    negative_impls,
    auto_traits,
    with_negative_coherence,
    type_alias_impl_trait
)]

use std::cell::{LazyCell, RefCell};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{RangeBounds, RangeInclusive};

use either::{Either, HeterogenousEither};

pub mod either;

#[derive(Clone, Debug)]
pub struct TextPosition {
    pub line: usize,
    pub row: usize,
}

pub type BoxedParser<C, T, E> = Box<dyn for<'a> FnMut(&mut C, &'a str) -> ParserResult<'a, T, E>>;

#[derive(Debug)]
pub enum ParserErrorType {
    ExpectedRange(String, usize),
    ExpectedString(&'static str),
    UnexpectedEOF,
    ExpectedToken(&'static str),
}

#[derive(Debug)]
pub struct ParserError {
    pub len: usize,
    pub error_type: ParserErrorType,
}

pub type ParserResult<'a, T, E> = Result<(T, &'a str), E>;

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at position {}", self.error_type, self.len)
    }
}

pub fn err<T, E: From<ParserError>>(error_type: ParserErrorType, slice: &str) -> Result<T, E> {
    Err(ParserError {
        error_type,
        len: slice.len(),
    }
    .into())
}

#[macro_export]
macro_rules! all_of {
    ($($parser:ident),+) => {
        move |ctx: &'_ mut _, mut s| {
            Ok((
                ($(
                    {
                        let (res, new_slice) = $crate::Parser::parse(&mut $parser, ctx, s)?;
                        s = new_slice;
                        res
                    }
                ),+),
            s))
        }
    }
}

pub trait Parser<C, E: From<ParserError>>: Sized {
    type Output<'a>;

    fn parse<'a>(
        &mut self,
        ctx: &mut C,
        input: &'a str,
    ) -> ParserResult<'a, <Self as Parser<C, E>>::Output<'a>, E>;

    fn repeating(
        mut self,
        bounds: impl RangeBounds<usize> + Debug + Clone + 'static,
    ) -> impl Parser<C, E> {
        move |ctx: &mut C, s| {
            let mut out = vec![];
            let mut count = 0;
            let mut slice = s;
            while bounds.contains(&(count + 1)) {
                match self.parse(ctx, slice) {
                    Ok((v, new_slice)) => {
                        out.push(v);
                        count += 1;
                        slice = new_slice;
                    }
                    Err(_) => {
                        break;
                    }
                }
            }
            if bounds.contains(&count) {
                Ok((out, slice))
            } else {
                err(
                    ParserErrorType::ExpectedRange(format!("{:?}", bounds), count),
                    slice,
                )
            }
        }
    }

    fn sequence(
        mut self,
        bounds: impl RangeBounds<usize> + Debug + Clone + 'static,
    ) -> impl Parser<C, E> {
        move |ctx: &mut C, s| {
            let mut count = 0;
            let mut slice = s;
            while bounds.contains(&(count + 1)) {
                match self.parse(ctx, slice) {
                    Ok((_, new_slice)) => {
                        count += 1;
                        slice = new_slice;
                    }
                    Err(_) => {
                        break;
                    }
                }
            }
            if bounds.contains(&count) {
                Ok((&s[0..s.len() - slice.len()], slice))
            } else {
                err(
                    ParserErrorType::ExpectedRange(format!("{:?}", bounds), count),
                    slice,
                )
            }
        }
    }

    fn slice(mut self) -> impl Parser<C, E> {
        move |ctx: &mut C, s| {
            let (_, slice) = self.parse(ctx, s)?;
            Ok((&s[0..s.len() - slice.len()], slice))
        }
    }

    fn map<V>(mut self, func: impl for<'a> Fn(Self::Output<'a>) -> V) -> impl Parser<C, E> {
        move |ctx, str| self.parse(ctx, str).map(|(v, s)| (func(v), s))
    }

    fn optional(mut self) -> impl Parser<C, E> {
        move |ctx: &mut C, s| {
            Ok(self
                .parse(ctx, s)
                .ok()
                .map(|(v, s)| (Some(v), s))
                .unwrap_or_else(|| (None, s)))
        }
    }

    fn boxed<'a>(self) -> BoxedParser<C, Self::Output<'a>, E> {
        Box::from(self)
    }

    fn then<V>(mut self, mut other: impl Parser<C, E>) -> impl Parser<C, E> {
        move |ctx: &mut C, s| {
            let (a, s) = self.parse(ctx, s)?;
            let (b, s) = other.parse(ctx, s)?;
            Ok(((a, b), s))
        }
    }

    fn or(
        mut self,
        mut other: impl for<'a> Parser<C, E, Output<'a> = Self::Output<'a>>,
    ) -> impl Parser<C, E> {
        parser(move |ctx: &'_ mut C, c| self.parse(ctx, c).or_else(|_| other.parse(ctx, c)))
    }

    fn coerce_ctx<Ctx>(mut self) -> impl Parser<Ctx, E>
    where
        Self: Parser<(), E>,
    {
        move |_: &mut Ctx, s| self.parse(&mut (), s)
    }

    fn wrapped(mut self, prefix: &'static str, suffix: &'static str) -> impl Parser<C, E> {
        let mut prefix = literal(prefix).coerce_ctx();
        let mut suffix = literal(suffix).coerce_ctx();
        parser(|_, s| Ok((&s[1..], s)));
        all_of!(prefix, self, suffix).map(|(_, e, _)| e)
    }
}

impl<T, C, E: From<ParserError>, F> Parser<C, E> for F
where
    F: for<'a> FnMut(&mut C, &'a str) -> ParserResult<'a, T, E>,
{
    type Output<'a> = T;

    fn parse<'a, 'b>(&mut self, ctx: &'b mut C, input: &'a str) -> ParserResult<'a, T, E> {
        self(ctx, input)
    }
}

pub const fn parser<C, T, E: From<ParserError>>(
    thing: impl for<'a> FnMut(&mut C, &'a str) -> ParserResult<'a, T, E>,
) -> impl Parser<C, E> {
    thing
}

pub const fn literal<'a, E: From<ParserError>>(string_literal: &'static str) -> impl Parser<(), E> {
    move |_: &mut (), s: &'a str| {
        if &s[..string_literal.len()] == string_literal {
            Ok((string_literal, &s[string_literal.len()..]))
        } else {
            err(ParserErrorType::ExpectedString(string_literal), s)
        }
    }
}

pub const fn char_match<'a, E: From<ParserError>>(
    token_name: &'static str,
    mut f: impl FnMut(char) -> bool,
) -> impl Parser<(), E> {
    move |_: &mut (), s: &'a str| {
        let next = s.chars().next();
        match next {
            Some(c) => {
                if f(c) {
                    Ok((c, &s[c.len_utf8()..]))
                } else {
                    err(ParserErrorType::ExpectedToken(token_name), s)
                }
            }
            None => err(ParserErrorType::UnexpectedEOF, s),
        }
    }
}

pub fn char_range<E: From<ParserError>>(
    token_name: &'static str,
    range: RangeInclusive<char>,
) -> impl Parser<(), E> {
    char_match(token_name, move |c| range.contains(&c))
}

pub fn take_while<E: From<ParserError>>(
    token_name: &'static str,
    f: impl Fn(char) -> bool,
) -> impl Parser<(), E> {
    char_match(token_name, f).sequence(1..)
}

pub const fn delimited_list<'a, A: 'a, B: 'a, C: 'a, E: From<ParserError>>(
    mut elem: impl Parser<C, E>,
    mut delim: impl Parser<(), E>,
) -> impl Parser<C, E> {
    move |ctx: &mut C, mut s| {
        let mut tokens = vec![];
        while let Ok((e, new_slice)) = elem.parse(ctx, s) {
            tokens.push((e, None));
            s = new_slice;
            match delim.parse(&mut (), s) {
                Ok((d, new_slice)) => {
                    s = new_slice;
                    tokens.last_mut().map(|e| e.1.replace(d));
                }
                Err(_) => break,
            }
        }
        Ok((tokens, s))
    }
}

#[macro_export]
macro_rules! first_matching {
    ($last:expr) => {
        $last
    };
    ($first:expr, $($rest:expr),+) => {
        $first.or_same($crate::first_matching!($($rest),+))
    };
}

pub fn int_parser() -> impl Parser<(), ParserError> {
    literal("-")
        .optional()
        .then(char_range("digit", '0'..='9').sequence(1..))
        .slice()
        .map(|s| s.parse().unwrap())
}
