use crate::{error::ParserError, literal, Literal, Parser};

impl<'a> Parser<'a, Literal, ParserError> for &'static str {
    fn parse(&self, input: &'a str) -> crate::ParserResult<'a, Literal, ParserError> {
        literal(*self, input)
    }
}
