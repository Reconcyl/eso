#![feature(fnbox)]

extern crate num_bigint;
extern crate num_traits;
extern crate rand;

mod interpret;
use interpret::Options;

fn err_and_exit(msg: String) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1)
}

fn main() {
    let mut options = Options::default();
    let mut args = std::env::args();
    let _program_name = args.next();
    let file_name = args.next().unwrap_or_else(
        || err_and_exit(String::from("Expected file name.")));
    for option in args {
        match option.as_str() {
            "--wrap-on-invalid-output" | "-w"
                => options.wrap_on_invalid_output = true,
            "--ignore-invalid-terms" | "-i"
                => options.ignore_invalid_terms = true,
            "--enable-decimal-io-extension" | "-d"
                => options.enable_decimal_io_extension = true,
            "--suppress-final-result" | "-r"
                => options.suppress_final_result = true,
            other => err_and_exit(format!("`{}` is not a valid flag.", other))
        }
    }
    let code = std::fs::read_to_string(file_name).unwrap_or_else(
        |e| err_and_exit(format!("Could not read file: {}", e)));
    let stdin = std::io::stdin();
    interpret::run(
        &code,
        stdin.lock(),
        std::io::stdout(),
        rand::thread_rng(),
        options
    ).unwrap_or_else(|e| err_and_exit(e));
}