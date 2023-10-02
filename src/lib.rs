#![feature(
    lazy_cell,
    return_position_impl_trait_in_trait,
    try_trait_v2,
    const_trait_impl,
    impl_trait_in_assoc_type
)]

pub mod delimited_list;

use std::{
    cell::LazyCell,
    convert::Infallible,
    ops::{Bound, ControlFlow, FromResidual, RangeBounds, RangeInclusive, Try},
};

use delimited_list::DelimitedList;

pub struct ParserResult<'a, T, E> {
    pub slice: &'a str,
    pub typ: ParserResultType<T, E>,
}

impl<'a, T, E> FromResidual for ParserResult<'a, T, E> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        Self {
            slice: residual.slice,
            typ: match residual.typ {
                ParserResultType::Ok(_) => unreachable!(),
                ParserResultType::Incomplete => ParserResultType::Incomplete,
                ParserResultType::Failure(e) => ParserResultType::Failure(e),
            },
        }
    }
}

impl<'a, T, E> Try for ParserResult<'a, T, E> {
    type Output = (&'a str, T);

    type Residual = ParserResult<'a, Infallible, E>;

    fn from_output((slice, val): Self::Output) -> Self {
        ParserResult {
            slice,
            typ: ParserResultType::Ok(val),
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self.typ {
            ParserResultType::Ok(t) => ControlFlow::Continue((self.slice, t)),
            _ => ControlFlow::Break(self.map(|_| unreachable!())),
        }
    }
}

#[macro_export]
macro_rules! tuple_parser {
    ($($parser:ident),+) => {
        move |ctx: &'_ mut _, mut slice| {
            let outputs = ($(
                    {
                        let (new_slice, res) = $crate::Parser::parse(&$parser, ctx, slice)?;
                        slice = new_slice;
                        res
                    }
                ),+);
            $crate::ParserResult::ok(slice, outputs)
        }
    }
}

impl<'a, T, E> ParserResult<'a, T, E> {
    pub fn ok(slice: &'a str, val: T) -> Self {
        ParserResult {
            slice,
            typ: ParserResultType::Ok(val),
        }
    }

    pub fn err(slice: &'a str, err: E) -> Self {
        ParserResult {
            slice,
            typ: ParserResultType::Failure(err),
        }
    }

    pub fn into_option(self) -> Option<(&'a str, T)> {
        if let ParserResultType::Ok(t) = self.typ {
            Some((self.slice, t))
        } else {
            None
        }
    }

    pub fn incomplete(slice: &'a str) -> Self {
        ParserResult {
            slice,
            typ: ParserResultType::Incomplete,
        }
    }

    pub fn map<V>(self, f: impl Fn(T) -> V) -> ParserResult<'a, V, E> {
        ParserResult {
            slice: self.slice,
            typ: match self.typ {
                ParserResultType::Ok(t) => ParserResultType::Ok(f(t)),
                ParserResultType::Failure(e) => ParserResultType::Failure(e),
                ParserResultType::Incomplete => ParserResultType::Incomplete,
            },
        }
    }

    pub fn or_else(self, f: impl FnOnce() -> Self) -> Self {
        match self.typ {
            ParserResultType::Ok(_) => self,
            _ => f(),
        }
    }

    pub fn and_then<V>(
        self,
        f: impl FnOnce(T) -> ParserResult<'a, V, E>,
    ) -> ParserResult<'a, V, E> {
        ParserResult {
            slice: self.slice,
            typ: match self.typ {
                ParserResultType::Ok(t) => return f(t),
                ParserResultType::Incomplete => ParserResultType::Incomplete,
                ParserResultType::Failure(e) => ParserResultType::Failure(e),
            },
        }
    }
}

pub enum ParserResultType<T, E> {
    Ok(T),
    Incomplete,
    Failure(E),
}

pub enum ParserError {
    ExpectedLiteral(&'static str),
    ExpectedToken(&'static str),
}

fn is_under(num: usize, end_bound: Bound<usize>) -> bool {
    match end_bound {
        Bound::Included(n) => num <= n,
        Bound::Excluded(n) => num < n,
        Bound::Unbounded => true,
    }
}

pub type BoxedParser<'a, C, T, E> = Box<dyn Parser<'a, C, T, E> + 'a>;

impl<'a, C, T: 'a, E> Parser<'a, C, T, E> for BoxedParser<'a, C, T, E> {
    fn parse(&self, ctx: &mut C, input: &'a str) -> ParserResult<'a, T, E> {
        self.as_ref().parse(ctx, input)
    }
}

pub trait Parser<'a, C, T: 'a, E> {
    fn parse(&self, ctx: &mut C, input: &'a str) -> ParserResult<'a, T, E>;

    fn boxed(self) -> BoxedParser<'a, C, T, E>
    where
        Self: Sized + 'a,
    {
        Box::from(self)
    }

    fn slice(self) -> impl Parser<'a, C, &'a str, E>
    where
        Self: Sized,
    {
        move |c: &mut C, s| {
            let (new_slice, _) = self.parse(c, s)?;
            let consumed = &s[..s.len() - new_slice.len()];
            ParserResult::ok(s, consumed)
        }
    }

    fn wrapped(self, prefix: &'static str, suffix: &'static str) -> impl Parser<'a, C, T, E>
    where
        Self: Sized,
        E: From<ParserError>,
    {
        let prefix = literal(prefix).coerce_ctx();
        let suffix = literal(suffix).coerce_ctx();
        tuple_parser!(prefix, self, suffix).map(|(_, e, _)| e)
    }

    fn repeating(self, bounds: impl RangeBounds<usize>) -> impl Parser<'a, C, Vec<T>, E>
    where
        Self: Sized,
        E: From<ParserError>,
    {
        move |c: &mut C, mut s| {
            let mut count = 0;
            let mut elems = vec![];
            let mut err = None;
            while is_under(count, bounds.end_bound().cloned()) {
                let parsed = self.parse(c, s);
                if let ParserResultType::Ok(e) = parsed.typ {
                    elems.push(e);
                } else {
                    err = Some(parsed);
                    break;
                }
                s = parsed.slice;
                count += 1;
            }
            if !bounds.contains(&count) {
                err.expect("error is present when not enough matches are found")
                    .map(|_| unreachable!("error will never have an ok value"))
            } else {
                ParserResult::ok(s, elems)
            }
        }
    }

    fn sequence(self, bounds: impl RangeBounds<usize>) -> impl Parser<'a, C, &'a str, E>
    where
        Self: Sized,
        E: From<ParserError>,
    {
        move |c: &mut C, s| {
            let mut count = 0;
            let mut new_slice = s;
            let mut err = None;
            while is_under(count, bounds.end_bound().cloned()) {
                let parsed = self.parse(c, s);
                if let ParserResultType::Ok(_) = parsed.typ {
                    new_slice = parsed.slice;
                } else {
                    err = Some(parsed);
                    break;
                }
                count += 1;
            }
            if !bounds.contains(&count) {
                err.expect("error is present when not enough matches are found")
                    .map(|_| unreachable!("error will never have an ok value"))
            } else {
                ParserResult::ok(s, &s[..s.len() - new_slice.len()])
            }
        }
    }

    fn delimited_by<D: 'a>(
        self,
        delim: impl Parser<'a, C, D, E>,
    ) -> impl Parser<'a, C, DelimitedList<T, D>, E>
    where
        Self: Sized,
    {
        move |c: &mut C, s| {
            let (mut s, head) = self.parse(c, s)?;
            let mut list = DelimitedList::from(head);
            while let Some((new_slice, parsed_delim)) = delim.parse(c, s).into_option() {
                let (new_slice, elem) = self.parse(c, new_slice)?;
                list.push(parsed_delim, elem);
                s = new_slice;
            }
            ParserResult::ok(s, list)
        }
    }

    fn map<V: 'a>(self, f: impl Fn(T) -> V + 'a) -> impl Parser<'a, C, V, E>
    where
        Self: Sized,
    {
        move |c: &mut C, s| self.parse(c, s).map(&f)
    }

    fn discard(self) -> impl Parser<'a, C, (), E>
    where
        Self: Sized,
    {
        self.map(|_| ())
    }

    fn coerce_ctx<Ctx>(self) -> impl Parser<'a, Ctx, T, E>
    where
        Self: Parser<'a, (), T, E> + Sized,
    {
        move |_: &mut Ctx, s| self.parse(&mut (), s)
    }

    fn or(self, other: impl Parser<'a, C, T, E>) -> impl Parser<'a, C, T, E>
    where
        Self: Sized,
    {
        move |c: &mut C, s| self.parse(c, s).or_else(|| other.parse(c, s))
    }

    fn or_with<P: Parser<'a, C, T, E>>(self, other: impl FnOnce() -> P) -> impl Parser<'a, C, T, E>
    where
        Self: Sized,
    {
        let lazy = LazyCell::new(other);
        move |c: &mut C, s| self.parse(c, s).or_else(|| lazy.parse(c, s))
    }

    fn optional(self) -> impl Parser<'a, C, Option<T>, E>
    where
        Self: Sized,
    {
        move |c: &mut C, s| {
            let parsed = self.parse(c, s);
            if let ParserResultType::Ok(e) = parsed.typ {
                ParserResult::ok(parsed.slice, Some(e))
            } else {
                ParserResult::ok(parsed.slice, None)
            }
        }
    }

    fn then<V: 'a>(self, next: impl Parser<'a, C, V, E>) -> impl Parser<'a, C, (T, V), E>
    where
        Self: Sized,
    {
        move |c: &mut C, s| {
            let (s, a) = self.parse(c, s)?;
            let (s, b) = next.parse(c, s)?;
            ParserResult::ok(s, (a, b))
        }
    }

    fn then_with<V: 'a, P: Parser<'a, C, V, E>>(
        self,
        other: impl FnOnce() -> P,
    ) -> impl Parser<'a, C, (T, V), E>
    where
        Self: Sized,
    {
        let lazy = LazyCell::new(other);
        move |c: &mut C, s| {
            let (s, a) = self.parse(c, s)?;
            let (s, b) = lazy.parse(c, s)?;
            ParserResult::ok(s, (a, b))
        }
    }

    fn drop_left<A: 'a, B: 'a>(self) -> impl Parser<'a, C, B, E>
    where
        Self: Parser<'a, C, (A, B), E> + Sized,
    {
        self.map(|(_, b)| b)
    }

    fn drop_right<A: 'a, B: 'a>(self) -> impl Parser<'a, C, A, E>
    where
        Self: Parser<'a, C, (A, B), E> + Sized,
    {
        self.map(|(a, _)| a)
    }
}

impl<'a, F, C, T: 'a, E> const Parser<'a, C, T, E> for F
where
    F: Fn(&mut C, &'a str) -> ParserResult<'a, T, E>,
{
    fn parse(&self, ctx: &mut C, input: &'a str) -> ParserResult<'a, T, E>
    where
        T: 'a,
    {
        self(ctx, input)
    }
}

pub const fn literal<'a, E: From<ParserError>>(
    literal: &'static str,
) -> impl Parser<'a, (), &'a str, E> {
    move |_: &mut (), s: &'a str| {
        if s.starts_with(literal) {
            ParserResult::ok(s, &s[literal.len()..])
        } else {
            ParserResult::err(s, ParserError::ExpectedLiteral(literal).into())
        }
    }
}

pub const fn char_match<'a, E: From<ParserError>>(
    token_name: &'static str,
    pred: impl Fn(char) -> bool,
) -> impl Parser<'a, (), char, E> {
    move |_: &mut (), s: &'a str| {
        let Some(c) = s.chars().next() else {
            return ParserResult::err(s, ParserError::ExpectedToken(token_name).into());
        };
        if pred(c) {
            ParserResult::ok(&s[c.len_utf8()..], c)
        } else {
            ParserResult::err(s, ParserError::ExpectedToken(token_name).into())
        }
    }
}

pub fn char_range<'a, E: From<ParserError>>(
    token_name: &'static str,
    range: RangeInclusive<char>,
) -> impl Parser<'a, (), char, E> {
    char_match(token_name, move |c| range.contains(&c))
}

pub fn take_while<'a, E: From<ParserError>>(
    token_name: &'static str,
    pred: impl Fn(char) -> bool,
) -> impl Parser<'a, (), &'a str, E> {
    char_match(token_name, pred).sequence(..)
}
