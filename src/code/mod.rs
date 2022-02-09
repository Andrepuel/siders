pub mod c;
mod internal;

pub trait Code: std::fmt::Debug + PartialEq + Eq {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    fn code(&self) -> String {
        format!("{}", internal::CodeWrite(self))
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
