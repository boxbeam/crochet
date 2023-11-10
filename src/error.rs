use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum ParserError {
    ExpectedLiteral(&'static str),
    ExpectedToken(&'static str),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedLiteral(s) => write!(f, "Expected literal: '{s}'"),
            ParserError::ExpectedToken(s) => write!(f, "Expected {s}"),
        }
    }
}

impl Error for ParserError {}
