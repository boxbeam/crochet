#![feature(
    closure_lifetime_binder,
    lazy_cell,
    return_position_impl_trait_in_trait,
    negative_impls,
    auto_traits,
    with_negative_coherence,
    type_alias_impl_trait,
    try_trait_v2
)]

use std::{
    cell::LazyCell,
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};

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
    ExpectedString(&'static str),
}

pub trait Parser<'a, C, T: 'a, E> {
    fn parse(&self, ctx: &mut C, input: &'a str) -> ParserResult<'a, T, E>;

    fn boxed(self) -> Box<dyn for<'b> Fn(&mut C, &'b str) -> ParserResult<'b, T, E>>
    where
        Self: for<'b> Fn(&mut C, &'b str) -> ParserResult<'b, T, E> + Sized + 'static,
    {
        Box::from(self)
    }

    fn map<V: 'a>(self, f: impl Fn(T) -> V + 'a) -> impl Parser<'a, C, V, E>
    where
        Self: Sized,
    {
        move |c: &mut C, s| self.parse(c, s).map(&f)
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
}

impl<'a, F, C, T: 'a, E> Parser<'a, C, T, E> for F
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
        if s.len() >= literal.len() && &s[..literal.len()] == literal {
            ParserResult::ok(s, &s[literal.len()..])
        } else {
            ParserResult::err(s, ParserError::ExpectedString(literal).into())
        }
    }
}
