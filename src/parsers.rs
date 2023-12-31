use crate::{error::ParserError, literal, Literal, Parser, ParserResult};

impl<'a> Parser<'a, Literal, ParserError> for &'static str {
    fn parse(&self, input: &'a str) -> crate::ParserResult<'a, Literal, ParserError> {
        literal(*self, input)
    }
}

/// Create a tuple parser which implicitly converts errors as needed
#[macro_export]
macro_rules! parser_tuple {
    ($($p:expr),+) => {
        ($($p.err_into()),+)
    }
}

macro_rules! impl_parser_tuple {
    ($($t:ident),+ : $($p:ident),+) => {
        impl<'a, $($t),+, $($p),+, E> Parser<'a, ($($t),+), E> for ($($p),+)
        where
            $($p: Parser<'a, $t, E>),+ {
            fn parse(&self, mut input: &'a str) -> ParserResult<'a, ($($t),+), E> {
                #[allow(non_snake_case)]
                let ($($p),+) = self;
                let tuple = (
                    $(
                        {
                            let (val, s) = $p.parse(input)?;
                            input = s;
                            val
                        }
                    ),+
                );
                ParserResult::from_val(tuple, input)
            }
        }
    };
}

impl_parser_tuple!(T1, T2: P1, P2);
impl_parser_tuple!(T1, T2, T3: P1, P2, P3);
impl_parser_tuple!(T1, T2, T3, T4: P1, P2, P3, P4);
impl_parser_tuple!(T1, T2, T3, T4, T5: P1, P2, P3, P4, P5);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6: P1, P2, P3, P4, P5, P6);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7: P1, P2, P3, P4, P5, P6, P7);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8: P1, P2, P3, P4, P5, P6, P7, P8);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9: P1, P2, P3, P4, P5, P6, P7, P8, P9);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18);
impl_parser_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19);
