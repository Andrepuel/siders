pub mod c;
mod internal;

pub trait Code: std::fmt::Debug + PartialEq + Eq {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    fn code(&self) -> String {
        format!("{}", internal::CodeWrite(self))
    }
}
pub trait DynCode: std::fmt::Debug {
    fn dyn_write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    fn dyn_code(&self) -> String;
}
impl<T: Code> DynCode for T {
    fn dyn_write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Code::write(self, f)
    }

    fn dyn_code(&self) -> String {
        Code::code(self)
    }
}
impl Code for Box<dyn DynCode> {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DynCode::dyn_write(self.as_ref(), f)
    }
}
impl Eq for Box<dyn DynCode> {}
impl PartialEq for Box<dyn DynCode> {
    fn eq(&self, rhs: &Self) -> bool {
        format!("{self:?}") == format!("{rhs:?}")
    }
}

pub trait File: Code {
    fn name(&self) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Literal(pub String);
impl Code for Literal {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
