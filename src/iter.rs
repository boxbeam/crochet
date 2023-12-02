use std::marker::PhantomData;

use crate::{Parser, ParserResult};

pub(crate) struct ParsIter<'a, T, E, P>
where
    P: Parser<'a, T, E>,
{
    pub(crate) phantom: PhantomData<(T, E)>,
    pub(crate) source: &'a str,
    pub(crate) parser: P,
    pub(crate) err: bool,
}

impl<'a, T, E, P: Parser<'a, T, E>> Iterator for ParsIter<'a, T, E, P> {
    type Item = ParserResult<'a, T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.err {
            return None;
        }
        let res = (self.parser)(self.source);
        self.source = res.source;
        if res.is_err() {
            self.err = true;
        }
        Some(res)
    }
}

pub trait ParsingIterator<'a, T, E, P: Parser<'a, T, E>>:
    Iterator<Item = ParserResult<'a, T, E>>
{
    /// Create a [ParsingIterator] from a parser and source
    fn new(parser: P, source: &'a str) -> impl ParsingIterator<'a, T, E, P> {
        crate::iter(parser, source)
    }

    /// Turn `self` into an iterator over only the successfully-parsed elements, which may be none
    fn ok(self) -> impl Iterator<Item = T>
    where
        Self: Sized,
    {
        self.filter_map(ParserResult::ok)
    }

    /// Turn `self` into an iterator over only the successfully-parsed elements, requiring at least one
    fn require<C: FromIterator<T>>(mut self) -> ParserResult<'a, impl Iterator<Item = T>, E>
    where
        Self: Sized,
    {
        let (source, first) = self
            .next()
            .expect("parsing iterator must contain either an element or an error")?;
        let collection = std::iter::once(first).chain(self.ok());
        ParserResult::from_val(source, collection)
    }

    fn map_inner<V>(
        self,
        mut f: impl FnMut(T) -> V + 'a,
    ) -> impl Iterator<Item = ParserResult<'a, V, E>>
    where
        Self: Sized,
    {
        self.map(move |e| e.map(|inner| f(inner)))
    }
}

impl<'a, T, E, P: Parser<'a, T, E>, I> ParsingIterator<'a, T, E, P> for I where
    I: Iterator<Item = ParserResult<'a, T, E>>
{
}
