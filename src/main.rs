use std::io::Read;

use nameless::InputByteStream;
use siders::{
    code::{c, Code},
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
        if let Entity::Class(class) = entity {
            println!("// {}.h", class.name);
            println!("{}", c::Header(class.name.clone(), c::Class(class)).code());
            println!();
        }
    }
}
