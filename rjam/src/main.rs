mod parse;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Please pass the program as an argument.");
        return;
    }
    match parse::parse(args[1].as_bytes()) {
        Ok(bytecode) => println!("Parsed: {:?}", bytecode),
        Err(e) => eprintln!("Failed to parse: {:?}", e),
    }
}
