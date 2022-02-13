use std::io::Read;

use nameless::InputByteStream;
use siders::{
    code::{c, DynCode},
    idl::Entity,
    lexer::Lexical,
};

#[kommand::main]
fn main(mut input_stream: InputByteStream) {
    let mut input = String::new();
    input_stream.read_to_string(&mut input).unwrap();

    let input = Lexical::parse_all(&input).unwrap().1;
    let entities = Entity::parse_all(&input).unwrap().1;

    for entity in entities {
        let (name, code): (String, Box<dyn DynCode>) = match entity {
            Entity::Interface(x) => (x.name.clone(), Box::new(c::Interface(x))),
            Entity::Class(x) => (x.name.clone(), Box::new(c::Class(x))),
            _ => continue,
        };

        println!("// {}.h", name);
        println!("{}", c::Header(name, code).dyn_code());
        println!();
    }
}
