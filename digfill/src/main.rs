mod parse;

fn main() {
    let program = match std::env::args().nth(1) {
        Some(p) => p,
        None => {
            println!("please provide a program.");
            return
        }
    };
    println!("{:?}", parse::parse(&program));
}
