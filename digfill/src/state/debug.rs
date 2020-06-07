use termion::AsyncReader;
use termion::clear;
use termion::color;
use termion::cursor::{self, HideCursor};
use termion::event::Key;
use termion::input::{Keys, TermRead as _};
use termion::raw::{RawTerminal, IntoRawMode as _};

use std::io::{BufWriter, Write as _};
use std::fs::File;
use std::thread;
use std::time::Duration;

use super::{Pos, World, BufIo};

pub struct Debugger {
    world: World<BufIo>,
    finished: bool,
    cursor: Option<Pos>,
    stdin: Keys<AsyncReader>,
    tty: BufWriter<HideCursor<RawTerminal<File>>>,
    // ui_state: UiState,
}

impl Debugger {
    pub fn new(world: World<BufIo>) -> Self {
        Self {
            world,
            finished: false,
            cursor: None,
            stdin: termion::async_stdin().keys(),
            tty: BufWriter::new(HideCursor::from(termion::get_tty()
                .expect("couldn't get tty")
                .into_raw_mode()
                .expect("couldn't convert to raw mode"))),
        }
    }
    
    pub fn run(&mut self) {
        self.render();
        loop {
            if let Some(Ok(key)) = self.stdin.next() {
                match key {
                    Key::Char('q') => break,
                    Key::Char(' ') => if self.finished {
                        // log that you can press q to quit
                    } else {
                        self.finished = !self.world.step();
                    }
                    _ => writeln!(&mut self.tty, "{:?}", key).unwrap()
                }
                self.render();
            } else {
                self.tty.flush().unwrap();
                thread::sleep(Duration::from_millis(16));
            }
        }
        self.clear();
    }
    
    fn clear(&mut self) {
        write!(&mut self.tty, "{}", clear::All).unwrap();
    }

    fn go_to(&mut self, x: u16, y: u16) {
        write!(&mut self.tty, "{}", cursor::Goto(x, y)).unwrap();
    }
    
    fn render(&mut self) {
        self.clear();
        let mut line = 2;
        for dy in -15..=15 {
            self.go_to(2, line);
            for dx in -20..=20 {
                let pos = Pos {
                    x: self.world.miner.x + dx,
                    y: self.world.miner.y + dy,
                };
                let c = if pos == self.world.miner {
                    '@'
                } else if self.world.inscriptions.contains_key(&pos) {
                    '.'
                } else {
                    ' '
                };
                if self.world.get(pos) {
                    write!(&mut self.tty,
                        "{}{}{}{}{}",
                        color::Bg(color::White),
                        color::Fg(color::Black),
                        c,
                        color::Bg(color::Reset),
                        color::Fg(color::Reset),
                    ).unwrap();
                } else {
                    write!(&mut self.tty, "{}", c).unwrap();
                }
            }
            line += 1;
        }
    }
}
