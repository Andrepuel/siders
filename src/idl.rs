use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::{alpha1, alphanumeric1, multispace0},
        streaming::char,
    },
    combinator::{opt, recognize},
    multi::many0,
    sequence::pair,
    IResult,
};

use crate::parser::parse_string;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub name: String,
    pub methods: Vec<Method>,
}
impl Interface {
    pub fn parse(input: &str) -> IResult<&str, Interface> {
        entity("interface", input, |input, name| {
            let (input, methods) = Method::parse_multi(input)?;

            let r = Interface {
                name: name.to_string(),
                methods,
            };

            Ok((input, r))
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub name: String,
    pub methods: Vec<Method>,
}
impl Class {
    pub fn parse(input: &str) -> IResult<&str, Class> {
        entity("class", input, |input, name| {
            let (input, methods) = Method::parse_multi(input)?;

            let r = Class {
                name: name.to_string(),
                methods,
            };

            Ok((input, r))
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enumeration {
    pub name: String,
    pub variants: Vec<String>,
}
impl Enumeration {
    pub fn parse(input: &str) -> IResult<&str, Enumeration> {
        entity("enum", input, |input, name| {
            let (input, variants) = comma_separated(input, |input| {
                let (input, variant) = identifier(input)?;
                Ok((input, variant.to_string()))
            })?;

            let r = Enumeration {
                name: name.to_string(),
                variants,
            };

            Ok((input, r))
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    pub name: String,
    pub is_static: bool,
    pub ret: Type,
    pub args: Vec<Argument>,
}
impl Method {
    pub fn parse(input: &str) -> IResult<&str, Method> {
        let (input, _) = multispace0(input)?;
        let (input, is_static) = opt(tag("static"))(input)?;
        let is_static = is_static.is_some();
        let (input, _) = multispace0(input)?;
        let (input, name) = identifier(input)?;

        let (input, _) = char('(')(input)?;
        let (input, args) = comma_separated(input, Self::parse_argument)?;
        let (input, _) = char(')')(input)?;
        let (input, ret) = Self::parse_type(input)?;
        let (input, _) = char(';')(input)?;

        let r = Method {
            name: name.to_string(),
            is_static,
            ret,
            args,
        };

        Ok((input, r))
    }

    pub fn parse_multi(input: &str) -> IResult<&str, Vec<Method>> {
        let mut methods = vec![];
        let (input, _) = Self::parse_multi_recursive(input, &mut methods)?;
        Ok((input, methods))
    }

    fn parse_multi_recursive<'a>(
        input: &'a str,
        methods: &mut Vec<Method>,
    ) -> IResult<&'a str, ()> {
        let (input, method) = match Self::parse(input) {
            Ok(x) => x,
            Err(_) => return Ok((input, ())),
        };
        methods.push(method);

        Self::parse_multi_recursive(input, methods)
    }

    fn parse_argument(input: &str) -> IResult<&str, Argument> {
        let (input, _) = multispace0(input)?;
        let (input, name) = identifier(input)?;
        let (input, ret) = Self::parse_type(input)?;

        let r = Argument(name.to_string(), ret);
        Ok((input, r))
    }

    fn parse_type(input: &str) -> IResult<&str, Type> {
        let (input, _) = multispace0(input)?;
        let (input, ret_separator) = opt(char(':'))(input)?;
        let (input, _) = multispace0(input)?;
        let (input, ret) = match ret_separator {
            Some(_) => {
                let (input, ret) = identifier(input)?;
                (input, Some(ret))
            }
            None => (input, None),
        };
        let ret = Type(ret.map(|x| x.to_owned()));

        Ok((input, ret))
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Type(Option<String>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument(String, Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constant(String, String);
impl Constant {
    pub fn parse(input: &str) -> IResult<&str, Constant> {
        entity("constant", input, |input, name| {
            let (input, _) = multispace0(input)?;
            let (input, value) = parse_string(input)?;
            let r = Constant(name.to_string(), value);

            Ok((input, r))
        })
    }
}

fn entity<'a, T, F: FnOnce(&'a str, &'a str) -> IResult<&'a str, T>>(
    entity: &str,
    input: &'a str,
    f: F,
) -> IResult<&'a str, T> {
    let (input, _) = multispace0(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(entity)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;

    let (input, r) = f(input, name)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;

    Ok((input, r))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn comma_separated<'a, T, F: FnMut(&'a str) -> IResult<&'a str, T>>(
    input: &'a str,
    f: F,
) -> IResult<&'a str, Vec<T>> {
    let mut result = vec![];
    //FIXME tail recursion
    let (input, _) = comma_separated_recurse(input, f, &mut result)?;
    Ok((input, result))
}

fn comma_separated_recurse<'a, T, F: FnMut(&'a str) -> IResult<&'a str, T>>(
    input: &'a str,
    mut f: F,
    result: &mut Vec<T>,
) -> IResult<&'a str, ()> {
    let (input, _) = multispace0(input)?;
    let (input, one) = match f(input) {
        Ok(x) => x,
        Err(_) => return Ok((input, ())),
    };
    result.push(one);
    let (input, _) = multispace0(input)?;
    let (input, comma) = opt(char(','))(input)?;
    let comma = comma.is_some();

    if comma {
        comma_separated_recurse(input, f, result)
    } else {
        Ok((input, ()))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn interface() {
        let interface = Interface::parse(
            "
            Xis_Thing = interface {
                method_one();
            }
        ",
        )
        .unwrap()
        .1;

        assert_eq!(
            interface,
            Interface {
                name: "Xis_Thing".to_string(),
                methods: vec![Method {
                    name: "method_one".to_string(),
                    is_static: false,
                    ret: Default::default(),
                    args: Default::default(),
                }]
            }
        );
    }

    #[test]
    fn class() {
        let class = Class::parse(
            "
            Class_Thing = class {
                method_one();
            }
            ",
        )
        .unwrap()
        .1;

        assert_eq!(
            class,
            Class {
                name: "Class_Thing".to_string(),
                methods: vec![Method {
                    name: "method_one".to_string(),
                    is_static: false,
                    ret: Default::default(),
                    args: Default::default(),
                }]
            }
        );
    }

    #[test]
    fn enumeration() {
        let enumeration = Enumeration::parse(
            "
            Enum_eration = enum {
                One,
                Two
            }
            ",
        )
        .unwrap()
        .1;

        assert_eq!(
            enumeration,
            Enumeration {
                name: "Enum_eration".to_string(),
                variants: vec!["One".to_string(), "Two".to_string(),]
            }
        );

        let enumeration = Enumeration::parse("Empty = enum {}").unwrap().1;
        assert_eq!(
            enumeration,
            Enumeration {
                name: "Empty".to_string(),
                variants: vec![]
            }
        );
    }

    #[test]
    fn method() {
        let method = Method::parse("faz(): i32;").unwrap().1;

        assert_eq!(
            method,
            Method {
                name: "faz".to_string(),
                is_static: false,
                ret: Type(Some("i32".to_string())),
                args: Default::default(),
            }
        );

        let method = Method::parse("faz(a: i32, b: u32, c);").unwrap().1;
        assert_eq!(
            method,
            Method {
                name: "faz".to_string(),
                is_static: false,
                ret: Type(None),
                args: vec![
                    Argument("a".to_string(), Type(Some("i32".to_string()))),
                    Argument("b".to_string(), Type(Some("u32".to_string()))),
                    Argument("c".to_string(), Type(None)),
                ],
            }
        );

        let method = Method::parse("static faz();").unwrap().1;
        assert_eq!(
            method,
            Method {
                name: "faz".to_string(),
                is_static: true,
                ret: Type(None),
                args: vec![],
            }
        );
    }

    #[test]
    fn constants() {
        let constant = Constant::parse(r#"name = constant { "hel\"lo" } "#)
            .unwrap()
            .1;

        assert_eq!(
            constant,
            Constant("name".to_string(), r#"hel"lo"#.to_string())
        );
    }
}
