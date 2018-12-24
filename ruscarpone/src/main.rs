use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::process::exit;

mod interpreter;
use interpreter::fmt_to_string;

fn run() -> Result<(), String> {
    let mut files = Vec::new();
    for file_name in env::args().skip(1) {
        let file: File = File::open(file_name).map_err(fmt_to_string)?;
        files.push(io::BufReader::new(file));
    }
    let mut state = interpreter::State::new(io::stdin(), io::stdout());
    if files.is_empty() {
        return Err(String::from("No files given"))
    }
    for file in files {
        for byte in file.bytes() {
            state.run_symbol(byte.map_err(fmt_to_string)? as char)?;
        }
    }
    Ok(())
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => {
            eprintln!("Error: {}", e);
            exit(1);
        }
    }
}