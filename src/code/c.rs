use std::collections::BTreeSet;

use crate::{
    code::{Code, File},
    idl,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Header<C: Code>(pub String, pub C);
impl<C: Code> File for Header<C> {
    fn name(&self) -> String {
        self.0.clone()
    }
}
impl<C: Code> Code for Header<C> {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "#pragma once")?;
        writeln!(f)?;
        self.1.write(f)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Void,
    Primitive(&'static str),
    Complex(String),
}
impl From<idl::Type> for Type {
    fn from(type_: idl::Type) -> Self {
        match type_.0.as_deref() {
            None => Type::Void,
            Some("i8") => Type::Primitive("i8"),
            Some("i16") => Type::Primitive("i16"),
            Some("i32") => Type::Primitive("i32"),
            Some("i64") => Type::Primitive("i64"),
            Some("size") => Type::Primitive("size"),
            Some("bool") => Type::Primitive("bool"),
            Some(x) => Type::Complex(x.to_string()),
        }
    }
}
impl Code for Type {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void")?,
            Type::Primitive(x) => write!(
                f,
                "{}",
                match *x {
                    "i8" => "char",
                    "u8" => "unsigned char",
                    "i16" => "short",
                    "u16" => "unsigned short",
                    "i32" => "int",
                    "u32" => "unsigned int",
                    "i64" => "long long",
                    "u64" => "unsigned long long",
                    "size" => "unsigned long",
                    "bool" => "char",
                    x => unreachable!("{}", x),
                }
            )?,
            Type::Complex(x) => write!(f, "{}*", x)?,
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Argument(pub String, pub Type);
impl Code for Argument {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1.write(f)?;
        write!(f, " {}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub ret: Type,
    pub args: Vec<Argument>,
}
impl Code for Function {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ret.write(f)?;
        write!(f, " {}(", self.name)?;
        for (idx, arg) in self.args.iter().enumerate() {
            if idx > 0 {
                write!(f, ", ")?;
            }
            arg.write(f)?;
        }
        write!(f, ");")?;

        Ok(())
    }
}
impl<'a> From<(&'a idl::Class, &'a idl::Method)> for Function {
    fn from((class, method): (&'a idl::Class, &'a idl::Method)) -> Self {
        let mut args = vec![];
        if !method.is_static {
            args.push(Argument(
                "self".to_string(),
                TypeStub::from(class).get_type(),
            ))
        }
        args.extend(
            method
                .args
                .iter()
                .map(|x| Argument(x.0.clone(), Type::from(x.1.clone()))),
        );

        let ret = Type::from(method.ret.clone());

        Function {
            name: format!("{}_{}", class.name, method.name),
            ret,
            args,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeStub(pub String);
impl TypeStub {
    pub fn get_type(&self) -> Type {
        let name = &self.0;
        Type::Complex(format!("{name}_t"))
    }
}
impl Code for TypeStub {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = &self.0;
        write!(f, "typedef struct {name}_s {name}_t;")
    }
}
impl<'a> From<&'a idl::Class> for TypeStub {
    fn from(class: &'a idl::Class) -> Self {
        TypeStub(class.name.clone())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Class(pub idl::Class);
impl Code for Class {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.dependencies(), self.stub(), self.methods()).write(f)
    }
}
impl Class {
    pub fn dependencies(&self) -> BTreeSet<TypeStub> {
        self.idl_dependencies()
            .map(|x| Type::from(x.clone()))
            .filter_map(|x| match x {
                Type::Complex(name) if name != self.0.name => Some(name),
                _ => None,
            })
            .map(TypeStub)
            .collect()
    }

    pub fn stub(&self) -> TypeStub {
        TypeStub::from(&self.0)
    }

    pub fn methods(&self) -> Vec<Function> {
        self.0
            .methods
            .iter()
            .map(|method| Function::from((&self.0, method)))
            .collect()
    }

    fn idl_dependencies(&self) -> impl Iterator<Item = &idl::Type> {
        self.0
            .methods
            .iter()
            .flat_map(|x| [&x.ret].into_iter().chain(x.args.iter().map(|x| &x.1)))
    }
}

#[cfg(test)]
pub mod tests {
    use crate::code::Literal;

    use super::*;

    #[test]
    fn header_contains_pragma_once() {
        let header = Header("".to_string(), Literal("Hello".to_string()));

        assert_eq!(header.code(), "#pragma once\n\nHello")
    }

    #[test]
    fn header_name_is_specified() {
        let header = Header("name".to_string(), Literal("Hello".to_string()));

        assert_eq!(header.name(), "name")
    }

    #[test]
    fn types_from_name() {
        assert_eq!(Type::from(idl::Type(None)), Type::Void);
        for i in ["i8", "i16", "i32", "i64", "size", "bool"] {
            assert_eq!(
                Type::from(idl::Type(Some(i.to_string()))),
                Type::Primitive(i)
            );
        }

        assert_eq!(
            Type::from(idl::Type(Some("Complex".to_string()))),
            Type::Complex("Complex".to_string())
        );
    }

    #[test]
    fn types_code() {
        assert_eq!(Type::Void.code(), "void");
        assert_eq!(Type::Complex("xis_t".to_string()).code(), "xis_t*");

        for (name, c_name) in [
            ("i8", "char"),
            ("u8", "unsigned char"),
            ("i16", "short"),
            ("u16", "unsigned short"),
            ("i32", "int"),
            ("u32", "unsigned int"),
            ("i64", "long long"),
            ("u64", "unsigned long long"),
            ("size", "unsigned long"),
            ("bool", "char"),
        ] {
            assert_eq!(Type::Primitive(name).code(), c_name);
        }
    }

    #[test]
    fn arguments_code() {
        assert_eq!(Argument("a1".to_string(), Type::Void).code(), "void a1");
        assert_eq!(
            Argument("a2".to_string(), Type::Primitive("i32")).code(),
            "int a2"
        );
        assert_eq!(
            Argument("a3".to_string(), Type::Complex("xis_t".to_string())).code(),
            "xis_t* a3"
        );
    }

    #[test]
    fn functions() {
        assert_eq!(
            Function {
                name: "name".to_string(),
                ret: Type::Void,
                args: vec![],
            }
            .code(),
            "void name();"
        );

        assert_eq!(
            Function {
                name: "name".to_string(),
                ret: Type::Primitive("i32"),
                args: vec![],
            }
            .code(),
            "int name();"
        );

        assert_eq!(
            Function {
                name: "name".to_string(),
                ret: Type::Primitive("i32"),
                args: vec![Argument("a2".to_string(), Type::Primitive("i32")),],
            }
            .code(),
            "int name(int a2);"
        );

        assert_eq!(
            Function {
                name: "name".to_string(),
                ret: Type::Primitive("i32"),
                args: vec![
                    Argument("a2".to_string(), Type::Primitive("i32")),
                    Argument("a3".to_string(), Type::Complex("xis_t".to_string())),
                ],
            }
            .code(),
            "int name(int a2, xis_t* a3);"
        );
    }

    #[test]
    fn function_from_idl() {
        let class = idl::Class {
            name: "MyClass".to_string(),
            methods: vec![],
        };

        assert_eq!(
            Function::from((
                &class,
                &idl::Method {
                    name: "doit".to_string(),
                    is_static: false,
                    ret: idl::Type(None),
                    args: vec![],
                }
            )),
            Function {
                name: "MyClass_doit".to_string(),
                ret: Type::Void,
                args: vec![Argument(
                    "self".to_string(),
                    Type::Complex("MyClass_t".to_string())
                ),]
            }
        );

        assert_eq!(
            Function::from((
                &class,
                &idl::Method {
                    name: "doit".to_string(),
                    is_static: true,
                    ret: idl::Type(None),
                    args: vec![],
                }
            )),
            Function {
                name: "MyClass_doit".to_string(),
                ret: Type::Void,
                args: vec![]
            }
        );

        assert_eq!(
            Function::from((
                &class,
                &idl::Method {
                    name: "doit".to_string(),
                    is_static: true,
                    ret: idl::Type(Some("i32".to_string())),
                    args: vec![],
                }
            )),
            Function {
                name: "MyClass_doit".to_string(),
                ret: Type::Primitive("i32"),
                args: vec![]
            }
        );

        assert_eq!(
            Function::from((
                &class,
                &idl::Method {
                    name: "doit".to_string(),
                    is_static: true,
                    ret: idl::Type(None),
                    args: vec![idl::Argument(
                        "thing".to_string(),
                        idl::Type(Some("i32".to_string()))
                    ),],
                }
            )),
            Function {
                name: "MyClass_doit".to_string(),
                ret: Type::Void,
                args: vec![Argument("thing".to_string(), Type::Primitive("i32")),]
            }
        );
    }

    #[test]
    fn type_stub() {
        let stub = TypeStub("thing".to_string());
        assert_eq!(stub.code(), "typedef struct thing_s thing_t;");
        assert_eq!(stub.get_type(), Type::Complex("thing_t".to_string()));
    }

    fn given_a_c_class() -> Class {
        Class(idl::Class {
            name: "MyClass".to_string(),
            methods: vec![idl::Method {
                name: "doit".to_string(),
                is_static: false,
                ret: idl::Type(None),
                args: vec![],
            }],
        })
    }

    #[test]
    fn c_class_basic_structure() {
        let class = given_a_c_class();

        assert_eq!(class.dependencies(), Default::default());
        assert_eq!(class.stub(), TypeStub("MyClass".to_string()));
        assert_eq!(
            class.methods(),
            vec![Function {
                name: "MyClass_doit".to_string(),
                ret: Type::Void,
                args: vec![Argument(
                    "self".to_string(),
                    Type::Complex("MyClass_t".to_string())
                )]
            }]
        );
    }

    #[test]
    fn c_class_with_dependencies() {
        let mut class = given_a_c_class();
        class.0.methods[0].ret = idl::Type(Some("Complex2".to_string()));
        class.0.methods[0].args = vec![
            idl::Argument("a1".to_string(), idl::Type(Some("i32".to_string()))),
            idl::Argument("a2".to_string(), idl::Type(Some("Complex".to_string()))),
            idl::Argument("a3".to_string(), idl::Type(Some("Complex".to_string()))),
            idl::Argument("a4".to_string(), idl::Type(Some("MyClass".to_string()))),
        ];

        assert_eq!(
            class.dependencies(),
            BTreeSet::from([
                TypeStub("Complex".to_string()),
                TypeStub("Complex2".to_string())
            ])
        );
    }

    #[test]
    fn c_class_writes_deps_stubs_and_functions() {
        let mut class = given_a_c_class();
        class.0.methods[0].args = vec![idl::Argument(
            "a2".to_string(),
            idl::Type(Some("Complex".to_string())),
        )];

        assert_eq!(
            class.code(),
            (class.dependencies(), class.stub(), class.methods()).code()
        );
    }
}
