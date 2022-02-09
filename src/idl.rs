use nom::{branch::alt, combinator::map, error::ParseError, multi::fold_many0, IResult};

use crate::lexer::{Lexical, LexicalSlice};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Entity {
    Interface(Interface),
    Class(Class),
    Enumeration(Enumeration),
    Constant(Constant),
}
impl Entity {
    pub fn parse(input: &[Lexical]) -> IResult<&[Lexical], Entity> {
        alt((
            map(Interface::parse, Entity::Interface),
            map(Class::parse, Entity::Class),
            map(Enumeration::parse, Entity::Enumeration),
            map(Constant::parse, Entity::Constant),
        ))(input)
    }

    pub fn parse_all(input: &[Lexical]) -> IResult<&[Lexical], Vec<Entity>> {
        fold_many0(Self::parse, Vec::new, |mut result, entity| {
            result.push(entity);
            result
        })(input)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub name: String,
    pub methods: Vec<Method>,
}
impl Interface {
    pub fn parse(input: &[Lexical]) -> IResult<&[Lexical], Interface> {
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
    pub fn parse(input: &[Lexical]) -> IResult<&[Lexical], Class> {
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
    pub fn parse(input: &[Lexical]) -> IResult<&[Lexical], Enumeration> {
        entity("enum", input, |input, name| {
            let (input, variants) = comma_separated(input, |input| {
                let (input, variant) = input.identifier()?;
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
    pub fn parse(input: &[Lexical]) -> IResult<&[Lexical], Method> {
        let (input, is_static) = match input.identifier() {
            Ok((input, "static")) => (input, true),
            _ => (input, false),
        };
        let (input, name) = input.identifier()?;
        let (input, _) = input.lparen()?;
        let (input, args) = comma_separated(input, Self::parse_argument)?;
        let (input, _) = input.rparen()?;
        let (input, ret) = Self::parse_type(input)?;
        let (input, _) = input.semicolon()?;

        let r = Method {
            name: name.to_string(),
            is_static,
            ret,
            args,
        };

        Ok((input, r))
    }

    pub fn parse_multi(input: &[Lexical]) -> IResult<&[Lexical], Vec<Method>> {
        fold_many0(Self::parse, Vec::new, |mut result, method| {
            result.push(method);
            result
        })(input)
    }

    fn parse_argument(input: &[Lexical]) -> IResult<&[Lexical], Argument> {
        let (input, name) = input.identifier()?;
        let (input, ret) = Self::parse_type(input)?;

        let r = Argument(name.to_string(), ret);
        Ok((input, r))
    }

    fn parse_type(input: &[Lexical]) -> IResult<&[Lexical], Type> {
        let (input, ret) = match input.colon() {
            Ok((input, _)) => {
                let (input, ret) = input.identifier()?;
                (input, Some(ret))
            }
            _ => (input, None),
        };
        let ret = Type(ret.map(|x| x.to_owned()));

        Ok((input, ret))
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Type(pub Option<String>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Argument(pub String, pub Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constant(pub String, pub String);
impl Constant {
    pub fn parse(input: &[Lexical]) -> IResult<&[Lexical], Constant> {
        entity("constant", input, |input, name| {
            let (input, value) = input.string()?;
            let r = Constant(name.to_string(), value);

            Ok((input, r))
        })
    }
}

fn entity<'a, T, F: FnOnce(&'a [Lexical], &'a str) -> IResult<&'a [Lexical], T>>(
    entity: &str,
    input: &'a [Lexical],
    f: F,
) -> IResult<&'a [Lexical], T> {
    let (input, name) = input.identifier()?;
    let (input, _) = input.equals()?;
    let (input, entity_type) = input.identifier()?;

    if entity_type != entity {
        return Err(nom::Err::Error(nom::error::Error::from_error_kind(
            input,
            nom::error::ErrorKind::IsNot,
        )));
    }

    let (input, _) = input.lbracket()?;
    let (input, r) = f(input, name)?;
    let (input, _) = input.rbracket()?;

    Ok((input, r))
}

fn comma_separated<'a, T, F: FnMut(&'a [Lexical]) -> IResult<&'a [Lexical], T>>(
    input: &'a [Lexical],
    f: F,
) -> IResult<&'a [Lexical], Vec<T>> {
    comma_separated_recurse(input, f, vec![])
}

fn comma_separated_recurse<'a, T, F: FnMut(&'a [Lexical]) -> IResult<&'a [Lexical], T>>(
    input: &'a [Lexical],
    mut f: F,
    mut result: Vec<T>,
) -> IResult<&'a [Lexical], Vec<T>> {
    let (input, one) = match f(input) {
        Ok(x) => x,
        Err(_) => return Ok((input, result)),
    };
    result.push(one);
    let (input, comma) = match input.comma() {
        Ok((input, _)) => (input, true),
        _ => (input, false),
    };

    if comma {
        comma_separated_recurse(input, f, result)
    } else {
        Ok((input, result))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn interface() {
        let input = Lexical::parse_all(
            "
                Xis_Thing = interface {
                    method_one();
                }
            ",
        )
        .unwrap()
        .1;
        let interface = Interface::parse(&input).unwrap().1;

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
        let input = Lexical::parse_all(
            "
        Class_Thing = class {
            method_one();
        }
        ",
        )
        .unwrap()
        .1;
        let class = Class::parse(&input).unwrap().1;

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
        let input = Lexical::parse_all(
            "
        Enum_eration = enum {
            One,
            Two
        }
        ",
        )
        .unwrap()
        .1;
        let enumeration = Enumeration::parse(&input).unwrap().1;

        assert_eq!(
            enumeration,
            Enumeration {
                name: "Enum_eration".to_string(),
                variants: vec!["One".to_string(), "Two".to_string(),]
            }
        );

        let input = Lexical::parse_all("Empty = enum {}").unwrap().1;
        let enumeration = Enumeration::parse(&input).unwrap().1;
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
        let input = Lexical::parse_all("faz(): i32;").unwrap().1;
        let method = Method::parse(&input).unwrap().1;

        assert_eq!(
            method,
            Method {
                name: "faz".to_string(),
                is_static: false,
                ret: Type(Some("i32".to_string())),
                args: Default::default(),
            }
        );

        let input = Lexical::parse_all("faz(a: i32, b: u32, c);").unwrap().1;
        let method = Method::parse(&input).unwrap().1;
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

        let input = Lexical::parse_all("static faz();").unwrap().1;
        let method = Method::parse(&input).unwrap().1;
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
        let input = Lexical::parse_all(r#"name = constant { "hel\"lo" } "#)
            .unwrap()
            .1;
        let constant = Constant::parse(&input).unwrap().1;

        assert_eq!(
            constant,
            Constant("name".to_string(), r#"hel"lo"#.to_string())
        );
    }

    #[test]
    fn entity() {
        let input = Lexical::parse_all(
            r#"
            a_class = class {}
            a_interface = interface {}
            a_enum = enum {}
            a_constant = constant { "" }
            "#,
        )
        .unwrap()
        .1;

        let entities = Entity::parse_all(&input).unwrap().1;

        assert_eq!(
            entities,
            vec![
                Entity::Class(Class {
                    name: "a_class".to_string(),
                    methods: Default::default(),
                }),
                Entity::Interface(Interface {
                    name: "a_interface".to_string(),
                    methods: Default::default()
                }),
                Entity::Enumeration(Enumeration {
                    name: "a_enum".to_string(),
                    variants: Default::default()
                }),
                Entity::Constant(Constant("a_constant".to_string(), Default::default()))
            ]
        );
    }
}
