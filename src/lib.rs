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

macro_rules! all_of {
    ($($parser:ident),+) => {
        move |mut s| {
            Ok((
                ($(
                    {
                        let (res, new_slice) = $crate::Parser::parse(&mut $parser, s)?;
                        s = new_slice;
                        res
                    }
                ),+),
            s))
        }
    }
}

pub trait Parser<'a, T: 'a, E: From<ParserError>>: Sized {
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

    fn wrapped(mut self, prefix: &'static str, suffix: &'static str) -> impl Parser<'a, T, E> {
        let mut prefix = literal(prefix);
        let mut suffix = literal(suffix);
        all_of!(prefix, self, suffix).map(|(_, e, _)| e)
    }
}

impl<'a, T: 'a, E: From<ParserError>, F: FnMut(&'a str) -> ParserResult<'a, T, E>> Parser<'a, T, E>
    for F
{
    fn parse(&mut self, input: &'a str) -> ParserResult<'a, T, E> {
        self(input)
    }
}

pub const fn literal<'a, E: From<ParserError>>(
    string_literal: &'static str,
) -> impl Parser<'a, &'static str, E> {
    move |s: &'a str| {
        if &s[..string_literal.len()] == string_literal {
            Ok((string_literal, &s[string_literal.len()..]))
        } else {
            err(ParserErrorType::ExpectedString(string_literal), s)
        }
    }
}

pub trait Unzip<'a, A: 'a, B: 'a, E: From<ParserError>>: Parser<'a, (A, B), E> {
    fn drop_left(self) -> impl Parser<'a, B, E>;
    fn drop_right(self) -> impl Parser<'a, A, E>;
}

impl<'a, P, A: 'a, B: 'a, E: From<ParserError>> Unzip<'a, A, B, E> for P
where
    P: Parser<'a, (A, B), E>,
{
    fn drop_left(self) -> impl Parser<'a, B, E> {
        self.map(|(_, b)| b)
    }

    fn drop_right(self) -> impl Parser<'a, A, E> {
        self.map(|(a, _)| a)
    }
}

pub const fn char_match<'a, E: From<ParserError>>(
    token_name: &'static str,
    mut f: impl FnMut(char) -> bool,
) -> impl Parser<'a, char, E> {
    move |s: &'a str| {
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

pub fn take_while<'a, E: From<ParserError>>(
    token_name: &'static str,
    f: impl Fn(char) -> bool,
) -> impl Parser<'a, &'a str, E> {
    char_match(token_name, f).sequence(1..)
}

pub const fn delimited_list<'a, A: 'a, B: 'a, E: From<ParserError>>(
    mut elem: impl Parser<'a, A, E>,
    mut delim: impl Parser<'a, B, E>,
) -> impl Parser<'a, Vec<A>, E> {
    move |mut s| {
        let mut tokens = vec![];
        while let Ok((e, new_slice)) = elem.parse(s) {
            tokens.push(e);
            s = new_slice;
            match delim.parse(s) {
                Ok((_, new_slice)) => s = new_slice,
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

// #[macro_export]
// macro_rules! all_of {
//     ($last:expr) => {
//         $last
//     };
//     ($first:expr, $($rest:expr),+) => {
//         $first.then(all_of!($($rest),+)).map(flatten::Flatten::flatten)
//     };
// }

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
