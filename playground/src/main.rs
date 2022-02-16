use lazy_static::lazy_static;
use main_sys::{Main_run, Main_set, Provider_t, Provider_vtable_t};

pub mod main_sys;

pub trait Provider {
    fn print(&mut self, a: i8);
}
fn create_vtable() -> Provider_vtable_t {
    unsafe extern "C" fn destruct(self_: *mut Provider_t) {
        let self_ = self_ as *mut ProviderBox;
        Box::from_raw(self_);
    }
    unsafe extern "C" fn print(self_: *mut Provider_t, ascii: ::std::os::raw::c_char) {
        let self_ = self_ as *mut ProviderBox;
        (*self_)._siders_self.print(ascii as i8)
    }

    Provider_vtable_t {
        destruct: Some(destruct),
        print: Some(print),
    }
}
pub struct ProviderBox {
    _siders_super: Provider_t,
    _siders_self: Box<dyn Provider>,
}
impl From<Box<dyn Provider>> for *mut Provider_t {
    fn from(self_: Box<dyn Provider>) -> Self {
        lazy_static! {
            static ref VTABLE: Provider_vtable_t = create_vtable();
        }
        let vtable: &'static Provider_vtable_t = &VTABLE;

        Box::into_raw(Box::new(ProviderBox {
            _siders_super: Provider_t {
                _siders_vtable: vtable as *const _ as *mut _,
            },
            _siders_self: self_,
        })) as *mut Provider_t
    }
}

pub struct Main {}
impl Main {
    fn set(provider: Box<dyn Provider>) {
        unsafe { Main_set(provider.into()) }
    }

    fn run() {
        unsafe { Main_run() }
    }
}

//

struct MyProvider();
impl Drop for MyProvider {
    fn drop(&mut self) {
        println!();
        println!("Done");
    }
}
impl Provider for MyProvider {
    fn print(&mut self, a: i8) {
        print!("{}", a as u8 as char)
    }
}

fn main() {
    Main::set(Box::new(MyProvider()));
    Main::run();
}
