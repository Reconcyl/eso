use std::str::Chars;

/// Represents an error encountered while parsing.
#[derive(Debug)]
pub enum Error {
    Unexpected { c: char, line: usize, col: usize },
    UnexpectedEof,
}

/// Represents an instruction.
#[derive(Debug)]
pub enum Ins {
    Dir(Dir, DirIns),
    Nop,
    End,
}

/// Represents a direction.
#[derive(Debug)]
pub enum Dir { N, E, W, S }

/// Represents a directed instruction.
#[derive(Debug)]
pub enum DirIns {
    Dig,
    Fill,
    Step,
    Walk,
    Input,
    Output,
    Execute,
    InscribeMe,
    Inscribe(InscriptionIdx),
}

/// Inscriptions cannot be dynamically constructed at runtime - they can
/// only contain one of a fixed set of contents that appear in the program.
/// As such, they can be referred to by their index.
#[derive(Debug)]
pub struct InscriptionIdx(usize);

struct Parser<'a> {
    inscriptions: Vec<Vec<Ins>>,
    code: Chars<'a>,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn next(&mut self) -> Option<char> {
        let c = self.code.next()?;
        if c == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn parse_dir(&mut self) -> Result<Dir, Error> {
        match self.next() {
            Some('n') => Ok(Dir::N),
            Some('e') => Ok(Dir::E),
            Some('s') => Ok(Dir::S),
            Some('w') => Ok(Dir::W),
            Some(c) => Err(Error::Unexpected { c, line: self.line, col: self.col }),
            None => Err(Error::UnexpectedEof),
        }
    }

    fn parse_inscription(&mut self, top_level: bool) -> Result<InscriptionIdx, Error> {
        let mut body = Vec::new();

        macro_rules! make_dir_ins {
            ($dir_ins:expr) => {
                body.push(Ins::Dir(self.parse_dir()?, $dir_ins))
            }
        }

        loop {
            match self.next() {
                None => if top_level {
                    break
                } else {
                    return Err(Error::UnexpectedEof)
                }

                Some(')') => if top_level {
                    return Err(Error::Unexpected { c: ')', line: self.line, col: self.col })
                } else {
                    break
                }
                
                Some('@') => make_dir_ins!(DirIns::Dig),
                Some('#') => make_dir_ins!(DirIns::Fill),
                Some('$') => make_dir_ins!(DirIns::Step),
                Some('%') => make_dir_ins!(DirIns::Walk),
                Some('^') => make_dir_ins!(DirIns::Input),
                Some('&') => make_dir_ins!(DirIns::Output),
                Some('~') => make_dir_ins!(DirIns::Execute),
                Some('+') => make_dir_ins!(DirIns::InscribeMe),
                Some('(') => {
                    let insc = self.parse_inscription(false)?;
                    make_dir_ins!(DirIns::Inscribe(insc))
                }
                Some('!') => body.push(Ins::Nop),
                Some('*') => body.push(Ins::End),
                Some(_) => {}
            }
        }

        let idx = self.inscriptions.len();
        self.inscriptions.push(body);
        Ok(InscriptionIdx(idx))
    }
}

pub fn parse(s: &str) -> Result<Vec<Vec<Ins>>, Error> {
    let mut parser = Parser {
        inscriptions: Vec::new(),
        code: s.chars(),
        line: 0,
        col: 0,
    };
    parser.parse_inscription(true)?;
    Ok(parser.inscriptions)
}
