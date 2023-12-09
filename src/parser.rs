use std::ops::RangeBounds;

use crate::{repeating, ParserResult};

pub trait Parser<'a, T, E> {
    /// Parse a value from the input
    fn parse(&self, input: &'a str) -> ParserResult<'a, T, E>;

    /// Map the output type of this parser using a mapping function
    fn map<V>(&self, f: impl Fn(T) -> V) -> impl Parser<'a, V, E> {
        move |s| self.parse(s).map(&f)
    }

    /// Make this parser optional, parsing nothing if it would otherwise fail
    fn optional(&self) -> impl Parser<'a, Option<T>, E> {
        |s| self.parse(s).optional(s)
    }

    /// Make this parser parse an additional value, returned in a tuple
    fn and<V>(&self, other: impl Parser<'a, V, E>) -> impl Parser<'a, (T, V), E> {
        move |s| self.parse(s).and(|s| other.parse(s))
    }

    /// Make this parser try another parser if it fails
    fn or(&self, other: impl Parser<'a, T, E>) -> impl Parser<'a, T, E> {
        move |s| self.parse(s).or(|s| other.parse(s), s)
    }

    /// Make this parser repeat a specific number of times within a range
    fn repeating(
        &self,
        bounds: impl RangeBounds<usize> + Clone + 'a,
    ) -> impl Parser<'a, Vec<T>, E> {
        move |s| repeating(|s| self.parse(s), bounds.clone(), s)
    }

    /// Makes this parser parse an additional value, and explicitly ignore it - useful for elements with trailing whitespace
    fn and_ignore<V, E2>(&self, p: impl Parser<'a, V, E2>) -> impl Parser<'a, T, E>
    where
        E2: Into<E>,
    {
        move |s| self.parse(s).and(p.err_into()).map(|(v, _)| v)
    }

    /// Convert the error type of this parser implicitly
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
