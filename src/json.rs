use crate::*;

type Result<'a, T> = ParserResult<'a, T, ParserError>;

fn parse_num(input: &str) -> Result<f64> {
    literal("-", input)
        .optional(input)
        .and(cur!(take_while <= "digit", |c| c.is_ascii_digit()))
        .and(|s| {
            literal(".", s)
                .and(cur!(take_while <= "digit", |c| c.is_ascii_digit()))
                .optional(s)
        })
        .map_slice(input, |s| s.parse().unwrap())
}

fn parse_num_list(mut input: &str) -> Result<Vec<f64>> {
    let (iter, s) =
        iter_delimited(parse_num, |s| literal(",", s).and(whitespace), &mut input).require()?;
    ParserResult::from_val(iter.collect(), s)
}

fn parse_str(s: &str) -> Result<String> {
    let (_, mut s) = literal("\"", s)?;
    let string: String = iter(
        |s| parse_esc(s).or(|s| matching_char("char", |c| c != '"', s), s),
        &mut s,
    )
    .ok()
    .collect();
    let (_, s) = literal("\"", s)?;
    ParserResult::from_val(string, s)
}

fn parse_esc(s: &str) -> Result<char> {
    let (_, s) = literal("\\", s)?;
    let (c, s) = advance(s)?;
    ParserResult::from_val(
        match c {
            'n' => '\n',
            't' => '\t',
            _ => c,
        },
        s,
    )
}
