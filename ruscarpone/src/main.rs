use std::env;
use std::io::{self, Write};
use std::process::exit;

mod interpreter;

fn print_and_flush(s: &str) -> Result<(), String> {
    print!("{}", s);
    io::stdout().flush().map_err(|e| e.to_string())
}

fn read_line(stdin: &io::Stdin, prompt: &str) -> Result<Option<String>, String> {
    print_and_flush(prompt)?;
    let mut line = String::new();
    match stdin.read_line(&mut line) {
        Ok(0) => Ok(None),
        Ok(_) => Ok(Some(line)),
        Err(e) => Err(e.to_string()),
    }
}

fn run() -> Result<(), String> {
    let mut files: Vec<String> = Vec::new();
    let mut debug = false;
    for file_name in env::args().skip(1) {
        if file_name == "-d" {
            debug = true;
            continue;
        }
        files.push(std::fs::read_to_string(file_name).map_err(|e| e.to_string())?);
    }
    if files.is_empty() {
        repl()
    } else {
        let stdin = io::stdin();
        let mut state = interpreter::State::new(stdin.lock(), io::stdout());
        for file in files {
            state.run(file.chars())?;
        }
        if debug {
            state.debug_stack_contents();
        }
        Ok(())
    }
}

fn repl() -> Result<(), String> {
    let mut state = interpreter::State::new(io::stdin(), io::stdout());
    let stdin = io::stdin();
    while let Some(line) = read_line(&stdin, "> ")? {
        state.run(line.chars())?;
        state.debug_stack_contents();
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
