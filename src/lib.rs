#![feature(return_position_impl_trait_in_trait)]
#![feature(negative_impls)]
#![feature(with_negative_coherence)]
#![feature(auto_traits)]
#![feature(closure_lifetime_binder)]

use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{RangeBounds, RangeInclusive};

#[derive(Clone, Debug)]
pub struct TextPosition {
    pub line: usize,
    pub row: usize,
}

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

pub trait Parser<'a, T: 'a, E: From<ParserError>>:
    FnMut(&'a str) -> ParserResult<'a, T, E> + Sized
{
    fn parse(&mut self, input: &'a str) -> ParserResult<'a, T, E>;

    fn repeating(
        mut self,
        bounds: impl RangeBounds<usize> + Debug + Clone + 'static,
    ) -> impl Parser<'a, Vec<T>, E> {
        move |s| {
            let mut out = vec![];
            let mut count = 0;
            let mut slice = s;
            while bounds.contains(&(count + 1)) {
                match self.parse(slice) {
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
    ) -> impl Parser<'a, &'a str, E> {
        move |s| {
            let mut count = 0;
            let mut slice = s;
            while bounds.contains(&(count + 1)) {
                match self.parse(slice) {
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

    fn slice(mut self) -> impl Parser<'a, &'a str, E> {
        move |s| {
            let (_, slice) = self.parse(s)?;
            Ok((&s[0..s.len() - slice.len()], slice))
        }
    }

    fn map<V: 'a>(mut self, func: impl Fn(T) -> V + 'a) -> impl Parser<'a, V, E> {
        move |c| self.parse(c).map(|(v, s)| (func(v), s))
    }

    fn optional(mut self) -> impl Parser<'a, Option<T>, E> {
        move |s| {
            Ok(self
                .parse(s)
                .ok()
                .map(|(v, s)| (Some(v), s))
                .unwrap_or_else(|| (None, s)))
        }
    }

    fn then<V: 'a>(mut self, mut other: impl Parser<'a, V, E>) -> impl Parser<'a, (T, V), E> {
        move |s| {
            let (a, s) = self.parse(s)?;
            let (b, s) = other.parse(s)?;
            Ok(((a, b), s))
        }
    }

    fn or<V: 'a>(mut self, mut other: impl Parser<'a, V, E>) -> impl Parser<'a, Either<T, V>, E>
    where
        Either<T, V>: HeterogenousEither,
    {
        move |c| {
            self.parse(c)
                .map(|(v, s)| (v.into(), s))
                .or_else(|_| other.parse(c).map(|(v, s)| (v.into(), s)))
        }
    }

    fn or_same(mut self, mut other: impl Parser<'a, T, E>) -> impl Parser<'a, T, E> {
        move |c| self.parse(c).or_else(|_| other.parse(c))
    }
}

impl<'a, T: 'a, E: From<ParserError>, F: FnMut(&'a str) -> ParserResult<'a, T, E>> Parser<'a, T, E>
    for F
{
    fn parse(&mut self, input: &'a str) -> ParserResult<'a, T, E> {
        self(input)
    }
}

pub fn literal<'a, E: From<ParserError>>(
    string_literal: &'static str,
) -> impl Parser<'a, &'static str, E> {
    move |s| {
        if &s[..string_literal.len()] == string_literal {
            Ok((string_literal, &s[string_literal.len()..]))
        } else {
            err(ParserErrorType::ExpectedString(string_literal), s)
        }
    }
}

pub fn char_match<'a, E: From<ParserError>>(
    token_name: &'static str,
    mut f: impl FnMut(char) -> bool,
) -> impl Parser<'a, char, E> {
    move |s| {
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

pub fn char_range<'a, E: From<ParserError>>(
    token_name: &'static str,
    range: RangeInclusive<char>,
) -> impl Parser<'a, char, E> {
    char_match(token_name, move |c| range.contains(&c))
}

// pub fn take_while<E: From<ParserError>>(f: impl Fn(char) -> bool) -> impl Parser<&str, >

#[macro_export]
macro_rules! first_matching {
    ($last:expr) => {
        $last
    };
    ($first:expr, $($rest:expr),+) => {
        $first.or_same($crate::first_matching!($($rest),+))
    };
}

pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> From<A> for Either<A, B> {
    fn from(value: A) -> Self {
        Self::A(value)
    }
}

impl<A, B> From<B> for Either<A, B>
where
    Self: HeterogenousEither,
{
    fn from(value: B) -> Self {
        Self::B(value)
    }
}

trait MaybeInto<T> {
    fn maybe_into(self) -> Option<T>;
}

impl<T> MaybeInto<T> for T {
    fn maybe_into(self) -> Option<T> {
        Some(self)
    }
}

pub auto trait HeterogenousEither {}
#[allow(suspicious_auto_trait_impls)]
impl<T> !HeterogenousEither for Either<T, T> {}

impl<A: MaybeInto<C>, B, C> MaybeInto<C> for Either<A, B>
where
    Self: HeterogenousEither,
{
    fn maybe_into(self) -> Option<C> {
        match self {
            Either::A(a) => a.maybe_into(),
            _ => None,
        }
    }
}

impl<A, B: MaybeInto<C>, C> MaybeInto<C> for Either<A, B>
where
    Self: HeterogenousEither,
{
    fn maybe_into(self) -> Option<C> {
        match self {
            Either::B(b) => b.maybe_into(),
            _ => None,
        }
    }
}

pub fn int_parser<'a>() -> impl Parser<'a, i32, ParserError> {
    literal("-")
        .optional()
        .then(char_range("digit", '0'..='9').sequence(1..))
        .slice()
        .map(|s| s.parse().unwrap())
}
