#[macro_use]
mod value;
mod bytecode;
mod parse;
mod runtime;
mod utils;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("error: please pass the program as an argument");
        return;
    }
    let bytecode = match parse::parse(args[1].as_bytes()) {
        Err(e) => { eprintln!("syntax error: {}", e); return }
        Ok(bytecode) => bytecode
    };
    let mut runtime = runtime::Runtime::new();
    if let Err(e) = runtime.run(&bytecode) {
        eprintln!("runtime error: {}", e);
    }
    runtime.print_stack();
}
