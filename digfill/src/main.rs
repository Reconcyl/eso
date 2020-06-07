use structopt::StructOpt;
use termion::color;

use std::fs;
use std::io;

mod parse;
mod state;

use state::World;

#[derive(StructOpt)]
#[structopt(name="digfill", no_version)]
struct Options {
    #[structopt(short)] debug: bool,
    #[structopt(short)] eval: bool,
    arg: String,
}

fn main() {
    let options = Options::from_args();

    let code = if options.eval {
        options.arg
    } else {
        match fs::read_to_string(&options.arg) {
            Ok(code) => code,
            Err(e) => {
                eprintln!(
                    "{}error:{} Couldn't read file '{}': {}",
                    color::Fg(color::LightRed),
                    color::Fg(color::Reset),
                    options.arg,
                    e
                );
                return
            }
        }
    };

    let parsed = match parse::parse(&code) {
        Ok(p) => p,
        Err(e) => {
            eprintln!(
                "{}parse error:{} {}",
                color::Fg(color::LightRed),
                color::Fg(color::Reset),
                e,
            );
            return
        }
    };

    if options.debug {
        let io = state::BufIo::default();
        let world = World::new(parsed, io);
        let mut debugger = state::Debugger::new(world);
        debugger.run();
    } else {
        let stdin = io::stdin();
        let stdin = stdin.lock();
        let stdout = io::stdout();
        let io = state::StdIo::new(stdin, stdout);
        let mut world = World::new(parsed, io);
        world.run();
    }
    /*
    let mut world = World::new(parsed, todo!());
    let stdin = std::io::stdin();
    while {
        print!("\x1b[2J\x1b[1;1H");
        world.debug();
        stdin.read_line(&mut String::new()).unwrap();
        world.step()
    } {}
    */
}
