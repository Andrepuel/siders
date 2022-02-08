use nom::branch::alt;
use nom::bytes::complete::{is_not, take_while_m_n};
use nom::character::complete::{char, multispace1};
use nom::combinator::{map, map_opt, map_res, value, verify};
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;

fn parse_unicode(input: &str) -> IResult<&str, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let parse_delimited_hex = preceded(char('u'), delimited(char('{'), parse_hex, char('}')));
    let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));
    map_opt(parse_u32, std::char::from_u32)(input)
}

fn parse_escaped_char(input: &str) -> IResult<&str, char> {
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

fn parse_escaped_whitespace(input: &str) -> IResult<&str, &str> {
    preceded(char('\\'), multispace1)(input)
}

fn parse_literal(input: &str) -> IResult<&str, &str> {
    let not_quote_slash = is_not("\"\\");
    verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWS,
}

fn parse_fragment(input: &str) -> IResult<&str, StringFragment<'_>> {
    alt((
        map(parse_literal, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

pub fn parse_string(input: &str) -> IResult<&str, String> {
    let build_string = fold_many0(parse_fragment, String::new, |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(s),
            StringFragment::EscapedChar(c) => string.push(c),
            StringFragment::EscapedWS => {}
        }
        string
    });

    delimited(char('"'), build_string, char('"'))(input)
}
