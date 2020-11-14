mod parse;
mod runtime;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Please pass the program as an argument.");
        return;
    }
    let bytecode = match parse::parse(args[1].as_bytes()) {
        Err(e) => { eprintln!("Failed to parse: {:?}", e); return }
        Ok(bytecode) => bytecode
    };
    let mut runtime = runtime::Runtime::new();
    runtime.run(&bytecode);
}
