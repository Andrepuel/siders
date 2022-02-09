use super::Code;
use std::collections::BTreeSet;

pub struct CodeWrite<'a, T: Code + ?Sized>(pub &'a T);
impl<'a, T: Code + ?Sized> std::fmt::Display for CodeWrite<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.write(f)
    }
}

impl<T1: Code, T2: Code> Code for (T1, T2) {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.write(f)?;
        writeln!(f)?;
        self.1.write(f)?;
        Ok(())
    }
}

impl<T1: Code, T2: Code, T3: Code> Code for (T1, T2, T3) {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.write(f)?;
        writeln!(f)?;
        self.1.write(f)?;
        writeln!(f)?;
        self.2.write(f)?;
        Ok(())
    }
}

impl<'a, T: Code> Code for &'a [T] {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_iterator(self.iter(), f)
    }
}
impl<T: Code> Code for Vec<T> {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_slice().write(f)
    }
}
impl<T: Code> Code for BTreeSet<T> {
    fn write(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_iterator(self.iter(), f)
    }
}

fn write_iterator<'a, U: Code + 'a, T: Iterator<Item = &'a U>>(
    it: T,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    for (idx, code) in it.enumerate() {
        if idx > 0 {
            writeln!(f)?;
        }
        code.write(f)?;
    }

    Ok(())
}
