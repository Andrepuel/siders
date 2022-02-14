use std::collections::BTreeSet;

use crate::{
    code::{Code, File},
    idl,
};

pub trait ComplexTypeIdl {
    fn name(&self) -> String;
}
impl ComplexTypeIdl for idl::Class {
    fn name(&self) -> String {
        self.name.clone()
    }
}
impl ComplexTypeIdl for idl::Interface {
    fn name(&self) -> String {
        self.name.clone()
    }
}

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
    Function(Box<Function>),
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
            Some(x) => Type::Complex(format!("{x}_t")),
        }
    }
}
impl From<Function> for Type {
    fn from(f: Function) -> Self {
        Type::Function(Box::new(f))
    }
}
impl Type {
    pub fn write_variable(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        variable: Option<&str>,
    ) -> std::fmt::Result {
        match self {
            Type::Function(function) => function.write_function(f, variable)?,
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

        let variable = match self {
            Type::Function(_) => None,
            _ => variable,
        };

        if let Some(variable) = variable {
            write!(f, " {variable}")?;
        }

        Ok(())
    }
}
impl Code for Type {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_variable(f, None)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Argument(pub String, pub Type);
impl Code for Argument {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1.write_variable(f, Some(&self.0))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub ret: Type,
    pub args: Vec<Argument>,
}
impl Function {
    pub fn write_function(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ptr_name: Option<&str>,
    ) -> std::fmt::Result {
        self.ret.write(f)?;
        match ptr_name {
            Some(name) => write!(f, " (*{name})(")?,
            None => write!(f, " {name}(", name = self.name)?,
        }
        for (idx, arg) in self.args.iter().enumerate() {
            if idx > 0 {
                write!(f, ", ")?;
            }
            arg.write(f)?;
        }
        write!(f, ")")?;

        if ptr_name.is_none() {
            write!(f, ";")?;
        }

        Ok(())
    }

    pub fn nameless(self) -> Function {
        Self {
            name: Default::default(),
            ret: self.ret,
            args: self.args,
        }
    }
}
impl Code for Function {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_function(f, None)
    }
}
impl<'a, T: ComplexTypeIdl> From<(&'a T, &'a idl::Method)> for Function {
    fn from((class, method): (&'a T, &'a idl::Method)) -> Self {
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
            name: format!("{}_{}", class.name(), method.name),
            ret,
            args,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeStub(pub String);
impl TypeStub {
    pub fn name(&self) -> String {
        self.0.clone()
    }

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
impl<'a, T: ComplexTypeIdl> From<&'a T> for TypeStub {
    fn from(class: &'a T) -> Self {
        TypeStub(class.name())
    }
}
impl TypeStub {
    pub fn from_complex(name: &str) -> Self {
        assert!(name.ends_with("_t"));
        Self(name[0..name.len() - 2].to_string())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Structure {
    name: String,
    attributes: Vec<Argument>,
}
impl Code for Structure {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = &self.name;
        writeln!(f, "typedef struct {name}_s {{")?;
        for attribute in &self.attributes {
            write!(f, "    ")?;
            attribute.write(f)?;
            writeln!(f, ";")?;
        }
        write!(f, "}} {name}_t;")?;

        Ok(())
    }
}

pub trait HasDependencies {
    fn name(&self) -> &str;
    fn idl_methods(&self) -> &[idl::Method];
    fn include_self(&self) -> bool {
        false
    }
    fn dependencies(&self) -> BTreeSet<TypeStub> {
        let include_self = match self.include_self() {
            true => Some(TypeStub(self.name().to_string())),
            false => None,
        };

        self.idl_methods()
            .iter()
            .flat_map(|x| [&x.ret].into_iter().chain(x.args.iter().map(|x| &x.1)))
            .map(|x| Type::from(x.clone()))
            .filter_map(|x| match x {
                Type::Complex(name) => Some(name),
                _ => None,
            })
            .map(|x| TypeStub::from_complex(&x))
            .filter(|x| x.0 != self.name())
            .chain(include_self)
            .collect()
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
    pub fn stub(&self) -> TypeStub {
        TypeStub::from(&self.0)
    }

    pub fn methods(&self) -> Vec<Function> {
        [Function::from((
            &self.0,
            &idl::Method {
                name: "destruct".to_string(),
                ret: idl::Type(None),
                args: vec![],
                is_static: false,
            },
        ))]
        .into_iter()
        .chain(
            self.0
                .methods
                .iter()
                .map(|method| Function::from((&self.0, method))),
        )
        .collect()
    }
}
impl HasDependencies for Class {
    fn name(&self) -> &str {
        &self.0.name
    }

    fn idl_methods(&self) -> &[idl::Method] {
        &self.0.methods
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Vtable(pub idl::Interface);
impl Code for Vtable {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.structure().write(f)
    }
}
impl Vtable {
    fn type_stub(&self) -> TypeStub {
        let name = &self.0.name;
        TypeStub(format!("{name}_vtable"))
    }

    fn structure(&self) -> Structure {
        Structure {
            name: self.type_stub().name(),
            attributes: [idl::Method {
                name: "destruct".to_string(),
                ret: idl::Type(None),
                is_static: false,
                args: vec![],
            }]
            .iter()
            .chain(self.0.methods.iter())
            .map(|method| {
                Argument(
                    method.name.clone(),
                    Function::from((&self.0, method)).nameless().into(),
                )
            })
            .collect(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Interface(pub idl::Interface);
impl Code for Interface {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.dependencies(), self.vtable(), self.structure()).write(f)
    }
}
impl Interface {
    pub fn vtable(&self) -> Vtable {
        Vtable(self.0.clone())
    }

    pub fn structure(&self) -> Structure {
        Structure {
            name: self.0.name.clone(),
            attributes: vec![Argument(
                "_siders_vtable".to_string(),
                self.vtable().type_stub().get_type(),
            )],
        }
    }
}
impl HasDependencies for Interface {
    fn name(&self) -> &str {
        &self.0.name
    }

    fn include_self(&self) -> bool {
        true
    }

    fn idl_methods(&self) -> &[idl::Method] {
        &self.0.methods
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
            Type::Complex("Complex_t".to_string())
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

        assert_eq!(
            Argument(
                "ptr".to_string(),
                Function {
                    name: Default::default(),
                    ret: Type::Primitive("i32"),
                    args: vec![Argument("a2".to_string(), Type::Primitive("i32"))]
                }
                .into()
            )
            .code(),
            "int (*ptr)(int a2)"
        )
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
            vec![
                Function {
                    name: "MyClass_destruct".to_string(),
                    ret: Type::Void,
                    args: vec![Argument(
                        "self".to_string(),
                        Type::Complex("MyClass_t".to_string()),
                    )]
                },
                Function {
                    name: "MyClass_doit".to_string(),
                    ret: Type::Void,
                    args: vec![Argument(
                        "self".to_string(),
                        Type::Complex("MyClass_t".to_string())
                    )]
                }
            ]
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

    #[test]
    fn structure_is_list_of_variables() {
        assert_eq!(
            Structure {
                name: "MyStruct".to_string(),
                attributes: vec![
                    Argument("a1".to_string(), Type::Primitive("i32")),
                    Argument(
                        "a2".to_string(),
                        Function {
                            name: Default::default(),
                            ret: Type::Void,
                            args: vec![],
                        }
                        .into()
                    )
                ]
            }
            .code(),
            "typedef struct MyStruct_s {\n    int a1;\n    void (*a2)();\n} MyStruct_t;"
        );
    }

    fn given_an_idl_interface() -> idl::Interface {
        idl::Interface {
            name: "MyInterface".to_string(),
            methods: vec![idl::Method {
                name: "doit".to_string(),
                is_static: false,
                ret: idl::Type(None),
                args: vec![],
            }],
        }
    }

    #[test]
    fn vtable_basic_structure() {
        let vtable = Vtable(given_an_idl_interface());

        assert_eq!(
            vtable.structure(),
            Structure {
                name: "MyInterface_vtable".to_string(),
                attributes: vec![
                    Argument(
                        "destruct".to_string(),
                        Function {
                            name: Default::default(),
                            ret: Type::Void,
                            args: vec![Argument(
                                "self".to_string(),
                                Type::Complex("MyInterface_t".to_string()),
                            )]
                        }
                        .into()
                    ),
                    Argument(
                        "doit".to_string(),
                        Function {
                            name: Default::default(),
                            ret: Type::Void,
                            args: vec![Argument(
                                "self".to_string(),
                                Type::Complex("MyInterface_t".to_string())
                            )],
                        }
                        .into()
                    )
                ]
            }
        );

        assert_eq!(vtable.code(), vtable.structure().code());
    }

    #[test]
    fn c_interface_basic_structure() {
        let interface = Interface(given_an_idl_interface());

        assert_eq!(
            interface.dependencies(),
            BTreeSet::from([TypeStub("MyInterface".to_string()),])
        );
        assert_eq!(interface.vtable(), Vtable(given_an_idl_interface()));
        assert_eq!(
            interface.structure(),
            Structure {
                name: "MyInterface".to_string(),
                attributes: vec![Argument(
                    "_siders_vtable".to_string(),
                    Type::Complex("MyInterface_vtable_t".to_string())
                )]
            }
        );
    }

    #[test]
    fn c_interface_with_dependencies() {
        let mut interface = Interface(given_an_idl_interface());
        interface.0.methods[0].ret = idl::Type(Some("Complex2".to_string()));
        interface.0.methods[0].args = vec![
            idl::Argument("a1".to_string(), idl::Type(Some("i32".to_string()))),
            idl::Argument("a2".to_string(), idl::Type(Some("Complex".to_string()))),
            idl::Argument("a3".to_string(), idl::Type(Some("Complex".to_string()))),
            idl::Argument("a4".to_string(), idl::Type(Some("MyInterface".to_string()))),
        ];

        assert_eq!(
            interface.dependencies(),
            BTreeSet::from([
                TypeStub("MyInterface".to_string()),
                TypeStub("Complex".to_string()),
                TypeStub("Complex2".to_string())
            ])
        );
    }

    #[test]
    fn c_interface_writes_deps_vtable_and_structure() {
        let interface = Interface(given_an_idl_interface());

        assert_eq!(
            interface.code(),
            (
                interface.dependencies(),
                interface.vtable(),
                interface.structure()
            )
                .code()
        );
    }
}
