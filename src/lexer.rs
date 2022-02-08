use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{map, recognize},
    error::{self, Error, ParseError},
    multi::{fold_many0, many0},
    sequence::pair,
    IResult, Needed,
};

use crate::parser::parse_string;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexical {
    Identifier(String),
    String(String),
    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Equals,
    Comma,
    Colon,
    Semicolon,
}
impl Lexical {
    pub fn parse(input: &str) -> IResult<&str, Lexical> {
        let (input, _) = multispace0(input)?;
        let (input, token) = alt((
            map(identifier, |id| Lexical::Identifier(id.to_string())),
            map(parse_string, Lexical::String),
            map(char('('), |_| Lexical::Lparen),
            map(char(')'), |_| Lexical::Rparen),
            map(char('{'), |_| Lexical::Lbracket),
            map(char('}'), |_| Lexical::Rbracket),
            map(char('='), |_| Lexical::Equals),
            map(char(','), |_| Lexical::Comma),
            map(char(':'), |_| Lexical::Colon),
            map(char(';'), |_| Lexical::Semicolon),
        ))(input)?;

        Ok((input, token))
    }

    pub fn parse_all(input: &str) -> IResult<&str, Vec<Lexical>> {
        fold_many0(Self::parse, Vec::new, |mut result, lex| {
            result.push(lex);
            result
        })(input)
    }
}
pub trait LexicalSlice<'a>: Into<&'a [Lexical]> {
    fn first(self) -> IResult<&'a [Lexical], &'a Lexical> {
        let input = self.into();
        if input.is_empty() {
            return Err(nom::Err::Incomplete(Needed::new(1)));
        }

        let (token, input) = input.split_at(1);

        Ok((input, &token[0]))
    }

    fn identifier(self) -> IResult<&'a [Lexical], &'a str> {
        let (input, first) = self.first()?;
        let name = match first {
            Lexical::Identifier(x) => x,
            _ => {
                return Err(nom::Err::Error(Error::from_error_kind(
                    input,
                    error::ErrorKind::IsNot,
                )))
            }
        };

        Ok((input, name))
    }

    fn string(self) -> IResult<&'a [Lexical], String> {
        let (input, first) = self.first()?;
        let name = match first {
            Lexical::String(x) => x.clone(),
            _ => {
                return Err(nom::Err::Error(Error::from_error_kind(
                    input,
                    error::ErrorKind::IsNot,
                )))
            }
        };

        Ok((input, name))
    }

    fn lbracket(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Lbracket)
    }
    fn rbracket(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Rbracket)
    }
    fn lparen(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Lparen)
    }
    fn rparen(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Rparen)
    }
    fn equals(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Equals)
    }
    fn comma(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Comma)
    }
    fn colon(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Colon)
    }
    fn semicolon(self) -> IResult<&'a [Lexical], ()> {
        self.control(Lexical::Semicolon)
    }

    fn control(self, control: Lexical) -> IResult<&'a [Lexical], ()> {
        let (input, first) = self.first()?;
        match first == &control {
            true => (),
            false => {
                return Err(nom::Err::Error(Error::from_error_kind(
                    input,
                    error::ErrorKind::IsNot,
                )))
            }
        };

        Ok((input, ()))
    }
}
impl<'a> LexicalSlice<'a> for &'a [Lexical] {}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn identifier() {
        assert_eq!(
            Lexical::parse_all(" hello\nworld").unwrap().1,
            vec![
                Lexical::Identifier("hello".to_string()),
                Lexical::Identifier("world".to_string()),
            ],
        );
    }

    #[test]
    fn control() {
        assert_eq!(
            Lexical::parse_all("() {} = ,:;").unwrap().1,
            vec![
                Lexical::Lparen,
                Lexical::Rparen,
                Lexical::Lbracket,
                Lexical::Rbracket,
                Lexical::Equals,
                Lexical::Comma,
                Lexical::Colon,
                Lexical::Semicolon,
            ]
        );
    }

    #[test]
    fn string_literal() {
        assert_eq!(
            Lexical::parse_all(r#""Hello" "Wo\"rld""#).unwrap().1,
            vec![
                Lexical::String("Hello".to_string()),
                Lexical::String(r#"Wo"rld"#.to_string()),
            ]
        )
    }
}
