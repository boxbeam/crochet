use std::ops::RangeBounds;

use crate::{error::ParserError, literal, Parser};

impl<'a> Parser<'a, &'a str, ParserError> for &'static str {
    fn parse(&self, input: &'a str) -> crate::ParserResult<'a, &'a str, ParserError> {
        literal(self, input)
    }
}

impl<'a, P, R, T, E> Parser<'a, T, E> for (P, R)
where
    P: Parser<'a, T, E>,
    R: RangeBounds<usize>,
{
    fn parse(&self, input: &'a str) -> crate::ParserResult<'a, T, E> {
        todo!()
    }
}
