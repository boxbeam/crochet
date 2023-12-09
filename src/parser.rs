use std::ops::RangeBounds;

use crate::{repeating, ParserResult};

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

    fn and_ignore<V, E2>(&self, p: impl Parser<'a, V, E2>) -> impl Parser<'a, T, E>
    where
        E2: Into<E>,
    {
        move |s| self.parse(s).and(p.err_into()).map(|(v, _)| v)
    }

    fn err_into<E2>(&self) -> impl Parser<'a, T, E2>
    where
        E: Into<E2>,
    {
        move |s| self.parse(s).err_into()
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
