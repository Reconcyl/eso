mod parse;
mod state;

use state::{World, Pos};

fn main() {
    let program = match std::env::args().nth(1) {
        Some(p) => p,
        None => {
            println!("please provide a program.");
            return
        }
    };
    let parsed = match parse::parse(&program) {
        Ok(p) => p,
        Err(e) => {
            println!("parse error: {:?}", e);
            return
        }
    };

    let mut world = World::new(parsed);
    world.set(Pos { x: 10000, y: 10000 }, true);
    println!("{}", world.get(Pos { x: 10064, y: 10064 }));
    println!("{}", world.get(Pos { x: 10000, y: 10000 }));
    world.set(Pos { x: 10000, y: 10000 }, false);
    println!("{}", world.get(Pos { x: 10000, y: 10000 }));
}
