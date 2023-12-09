use std::{
    convert::Infallible,
    fmt::Debug,
    ops::{ControlFlow, FromResidual, Try},
};

use crate::Parser;

/// An output of a parser, contains the string slice to resume parsing from
#[must_use]
pub struct ParserResult<'a, T, E> {
    pub source: &'a str,
    pub typ: ParserResultType<T, E>,
}

impl<'a, T: Debug, E: Debug> Debug for ParserResult<'a, T, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.typ {
            ParserResultType::Ok(v) => f.debug_tuple("Ok").field(v).finish(),
            ParserResultType::Err(v) => f.debug_tuple("Err").field(v).finish(),
            ParserResultType::Incomplete => write!(f, "Incomplete"),
        }
    }
}

impl<'a, T, E> ParserResult<'a, T, E> {
    pub fn unwrap(self) -> T {
        if let ParserResultType::Ok(t) = self.typ {
            t
        } else {
            panic!("unwrap called on erroneous or incomplete parser result")
        }
    }
}

/// The type of a parser result
pub enum ParserResultType<T, E> {
    /// Successfully parsed, containing the parsed value
    Ok(T),
    /// Failed to parse, containing an error
    Err(E),
    /// Matched partially, might be recoverable but there was no error
    Incomplete,
}

impl<'a, T, E> Try for ParserResult<'a, T, E> {
    type Output = (T, &'a str);

    type Residual = ParserResult<'a, Infallible, E>;

    fn from_output(output: Self::Output) -> Self {
        Self::from_val(output.0, output.1)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self.typ {
            ParserResultType::Ok(v) => ControlFlow::Continue((v, self.source)),
            ParserResultType::Err(e) => ControlFlow::Break(ParserResult::from_err(e, self.source)),
            ParserResultType::Incomplete => {
                ControlFlow::Break(ParserResult::incomplete(self.source))
            }
        }
    }
}

impl<'a, T, E, F: From<E>> FromResidual<ParserResult<'a, Infallible, E>>
    for ParserResult<'a, T, F>
{
    fn from_residual(residual: ParserResult<'a, Infallible, E>) -> Self {
        Self {
            source: residual.source,
            typ: match residual.typ {
                ParserResultType::Ok(_) => unreachable!(),
                ParserResultType::Err(e) => ParserResultType::Err(e.into()),
                ParserResultType::Incomplete => ParserResultType::Incomplete,
            },
        }
    }
}

impl<T, E> ParserResultType<T, E> {
    /// Converts this to hold references instead of owned values
    pub fn as_ref(&self) -> ParserResultType<&T, &E> {
        match self {
            ParserResultType::Ok(v) => ParserResultType::Ok(v),
            ParserResultType::Err(e) => ParserResultType::Err(e),
            ParserResultType::Incomplete => ParserResultType::Incomplete,
        }
    }

    /// Maps the value of the Ok variant to another type
    pub fn map<V>(self, f: impl FnOnce(T) -> V) -> ParserResultType<V, E> {
        match self {
            ParserResultType::Ok(t) => ParserResultType::Ok(f(t)),
            ParserResultType::Incomplete => ParserResultType::Incomplete,
            ParserResultType::Err(e) => ParserResultType::Err(e),
        }
    }

    /// Maps the value of the Err variant to another type
    pub fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> ParserResultType<T, E2> {
        match self {
            ParserResultType::Ok(v) => ParserResultType::Ok(v),
            ParserResultType::Err(e) => ParserResultType::Err(f(e)),
            ParserResultType::Incomplete => ParserResultType::Incomplete,
        }
    }
}

impl<'a, T, E> ParserResult<'a, T, E> {
    /// Create a [ParserResult] from a value and source location to continue from
    pub fn from_val(val: T, source: &'a str) -> Self {
        Self {
            source,
            typ: ParserResultType::Ok(val),
        }
    }

    /// Create a [ParserResult] from an error and source location where the error occurred
    pub fn from_err(err: E, source: &'a str) -> Self {
        Self {
            source,
            typ: ParserResultType::Err(err),
        }
    }

    /// Create an incomplete [ParserResult] from a source location where the parsing stopped
    pub fn incomplete(source: &'a str) -> Self {
        Self {
            source,
            typ: ParserResultType::Incomplete,
        }
    }

    /// Returns whether this result is incomplete
    pub fn is_incomplete(&self) -> bool {
        matches!(self.typ, ParserResultType::Incomplete)
    }

    /// Returns whether this result is an error
    pub fn is_err(&self) -> bool {
        matches!(self.typ, ParserResultType::Err(_))
    }

    /// Returns whether this result succeeded
    pub fn is_ok(&self) -> bool {
        matches!(self.typ, ParserResultType::Ok(_))
    }

    /// Create a new [ParserResult] borrowing the value or error wrapped by this one
    pub fn as_ref(&self) -> ParserResult<'a, &T, &E> {
        ParserResult {
            source: self.source,
            typ: self.typ.as_ref(),
        }
    }

    /// Get an [Option] which is present only if the parsing succeeded
    pub fn ok(self) -> Option<T> {
        match self.typ {
            ParserResultType::Ok(v) => Some(v),
            _ => None,
        }
    }

    /// Get an [Option] which is present only if the parsing failed with an error
    pub fn err(self) -> Option<E> {
        match self.typ {
            ParserResultType::Err(e) => Some(e),
            _ => None,
        }
    }

    /// Force this result to be a success, resetting the position and using None if it was erroneous
    pub fn optional(self, start: &'a str) -> ParserResult<'a, Option<T>, E> {
        let position = match self.typ {
            ParserResultType::Ok(_) => self.source,
            _ => start,
        };
        ParserResult::from_val(self.ok(), position)
    }

    /// Maps this result's value to another type using a mapping function
    pub fn map<V>(self, f: impl FnOnce(T) -> V) -> ParserResult<'a, V, E> {
        ParserResult {
            source: self.source,
            typ: self.typ.map(f),
        }
    }

    /// Replaces this result's value with a given value
    pub fn is<V>(self, val: V) -> ParserResult<'a, V, E> {
        self.map(|_| val)
    }

    /// Maps this result's error to another type using a mapping function
    pub fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> ParserResult<'a, T, E2> {
        ParserResult {
            source: self.source,
            typ: self.typ.map_err(f),
        }
    }

    /// Implicitly convert the error type into another
    pub fn err_into<E2>(self) -> ParserResult<'a, T, E2>
    where
        E: Into<E2>,
    {
        self.map_err(Into::into)
    }

    /// Try another parser if this result failed, from the given position, and return whichever succeeded first, if any
    pub fn or<E2: Into<E>>(
        self,
        p: impl Parser<'a, T, E2>,
        from: &'a str,
    ) -> ParserResult<'a, T, E> {
        if self.is_ok() {
            self
        } else {
            p.parse(from).err_into()
        }
    }

    /// Parse another value after this one if this one succeeded, and return it in a tuple
    pub fn and<V, E2: Into<E>>(self, p: impl Parser<'a, V, E2>) -> ParserResult<'a, (T, V), E> {
        let (e1, s) = self?;
        let mut res2 = p.parse(s);
        if !res2.is_ok() {
            res2.source = s;
        }
        let (e2, s) = res2.err_into()?;
        ParserResult::from_val((e1, e2), s)
    }

    /// Parse another value after this one if this one succeeded, and discard the value
    pub fn and_ignore<V, E2: Into<E>>(self, p: impl Parser<'a, V, E2>) -> Self {
        self.and(p.err_into()).map(|(v, _)| v)
    }

    /// Parse another value using the previously-parsed value if this one succeeded
    pub fn flat_map<V>(
        self,
        p: impl FnOnce(T, &'a str) -> ParserResult<'a, V, E>,
    ) -> ParserResult<'a, V, E> {
        let (val, s) = self?;
        p(val, s)
    }

    /// Flatten this result if it contains another ParserResult
    pub fn flatten<V>(self) -> ParserResult<'a, V, E>
    where
        T: Identity<I = ParserResult<'a, V, E>>,
    {
        let (res, _) = self?;
        let res: ParserResult<'a, V, E> = res.ident();
        res
    }

    /// Converts the [ParserResult] into a slice over the parsed value, if it was successful
    pub fn parsed_slice(self, original: &'a str) -> ParserResult<'a, &'a str, E> {
        let slice = self.slice(original);
        self.map(|_| slice)
    }

    /// Gets the slice indicated by this [ParserResult], which may be either the parsed value or the erroneous input
    pub fn slice(&self, original: &'a str) -> &'a str {
        &original[..original.len() - self.source.len()]
    }

    /// Maps the parsed slice of this result
    pub fn map_slice<V>(
        self,
        f: impl FnOnce(&'a str) -> V,
        original: &'a str,
    ) -> ParserResult<'a, V, E> {
        self.parsed_slice(original).map(f)
    }
}

pub trait Identity {
    type I;

    fn ident(self) -> Self::I;
}

impl<T> Identity for T {
    type I = T;

    fn ident(self) -> T {
        self
    }
}
