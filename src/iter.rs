use std::marker::PhantomData;

use crate::{Parser, ParserResult};

pub(crate) struct ParsIter<'a, 'b, T, E, P>
where
    P: Parser<'a, T, E>,
{
    pub(crate) phantom: PhantomData<(T, E)>,
    pub(crate) source: &'b mut &'a str,
    pub(crate) parser: P,
    pub(crate) err: bool,
}

pub(crate) struct ParsIterDelim<'a, 'b, Elem, Delim, Error, PElem, PDelim>
where
    PElem: Parser<'a, Elem, Error>,
    PDelim: Parser<'a, Delim, Error>,
{
    pub(crate) phantom: PhantomData<(Elem, Delim, Error)>,
    pub(crate) source: &'b mut &'a str,
    pub(crate) elem_parser: PElem,
    pub(crate) delim_parser: PDelim,
    pub(crate) err: bool,
    pub(crate) first: bool,
}

impl<'a, 'b, T, E, P: Parser<'a, T, E>> Iterator for ParsIter<'a, 'b, T, E, P> {
    type Item = ParserResult<'a, T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.err {
            return None;
        }
        let res = self.parser.parse(self.source);
        *self.source = res.source;
        if res.is_err() {
            self.err = true;
        }
        Some(res)
    }
}

impl<'a, 'b, Elem, Delim, Error, PElem, PDelim> Iterator
    for ParsIterDelim<'a, 'b, Elem, Delim, Error, PElem, PDelim>
where
    PElem: Parser<'a, Elem, Error>,
    PDelim: Parser<'a, Delim, Error>,
{
    type Item = ParserResult<'a, Elem, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.err {
            return None;
        }
        let res = if self.first {
            self.first = false;
            self.elem_parser.parse(self.source)
        } else {
            self.delim_parser
                .parse(self.source)
                .and(|s| self.elem_parser.parse(s))
                .map(|(_, elem)| elem)
        };
        *self.source = res.source;
        if res.is_err() {
            self.err = true;
        }
        Some(res)
    }
}

pub trait ParsingIterator<'a, T: 'a, E: 'a>: Iterator<Item = ParserResult<'a, T, E>> {
    /// Create a [ParsingIterator] from a parser and source
    fn new<'b>(
        parser: impl Parser<'a, T, E> + 'b,
        source: &'b mut &'a str,
    ) -> impl ParsingIterator<'a, T, E> {
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
    fn require(mut self) -> ParserResult<'a, impl Iterator<Item = T>, E>
    where
        Self: Sized,
    {
        let (source, first) = self
            .next()
            .expect("parsing iterator must contain either an element or an error")?;
        let chain = std::iter::once(first).chain(self.ok());
        ParserResult::from_val(source, chain)
    }

    /// Map the element type of the [ParserResult]
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

impl<'a, T: 'a, E: 'a, I> ParsingIterator<'a, T, E> for I where
    I: Iterator<Item = ParserResult<'a, T, E>>
{
}
