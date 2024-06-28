use std::io::{self, Read as _};

fn main() -> io::Result<()> {
    let mut chars = Vec::new();
    std::io::stdin().read_to_end(&mut chars)?;

    if let Some(c) = chars.iter().position(|&b| b == b'\t') {
        chars.drain(0..c + 1);
    }

    let mut compact = false;
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--compact" => compact = true,
            _ => eprintln!("Warning: ignoring unknown argument {:?}", arg),
        }
    }

    let utf8 = lazyk_minifier_core::minify(&chars);
    println!("# {} bytes\n\n```", utf8.len());
    if compact {
        println!("{}", std::str::from_utf8(&utf8).unwrap());
    } else {
        for chunk in utf8.chunks(80) {
            println!("{}", std::str::from_utf8(chunk).unwrap());
        }
    }
    println!("```");

    Ok(())
}
