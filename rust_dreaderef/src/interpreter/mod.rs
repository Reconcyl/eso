use std::collections::HashMap;
use std::char;
use std::string::ToString;
use std::io::{prelude::*, Bytes};

use num::BigInt;
use num::ToPrimitive;

mod lex;
use self::lex::Token;

#[derive(Debug)]
struct DreaderefError { mes: String }

impl ToString for DreaderefError {
    fn to_string(&self) -> String {
        format!("Error: {}", self.mes)
    }
}

struct Dreaderef<I: Read, H, F> {
    state: HashMap<BigInt, Token>,
    holes: H,
    input_stream: Bytes<I>,
    output_fn: F,
}

fn vec_to_hashmap<T>(vec: Vec<T>) -> HashMap<BigInt, T> {
    vec.into_iter().enumerate().map(|(i, x)| (BigInt::from(i), x)).collect()
}

impl<I, H, F> Dreaderef<I, H, F>
where I: Read, H: Iterator<Item=BigInt>, F: FnMut(char)
{
    fn new(tokens: Vec<Token>, holes: H, input_stream: Bytes<I>, output_fn: F) -> Self {
        Dreaderef { state: vec_to_hashmap(tokens), holes, input_stream, output_fn }
    }
    fn get_cell(&mut self, cell: BigInt) -> Result<BigInt, DreaderefError> {
        let token = self.state.entry(cell.clone()).or_insert(Token::Literal(BigInt::from(0))).clone();
        match token {
            Token::Literal(ref n) => Ok(n.clone()),
            Token::Unset => Err(DreaderefError {
                mes: format!("attempted to read unset value at position {}", cell)
            }),
            Token::Hole => {
                let hole = self.holes.next().ok_or_else(|| DreaderefError {
                    mes: String::from("attempted to read hole, but hole queue is empty")
                })?;
                self.state.insert(cell.clone(), Token::Literal(hole));
                Ok(cell)
            }
        }
    }
    fn set_cell(&mut self, source: BigInt, destination: BigInt) {
        self.state.insert(source, Token::Literal(destination));
    }
    fn get_ip(&mut self) -> BigInt {
        match *self.state.entry(BigInt::from(-1))
          .or_insert(Token::Literal(BigInt::from(0))) {
            Token::Literal(ref n) => n.clone(),
            _ => panic!("IP should never be an Unset or a Hole."),
        }
    }
    fn read_number(&mut self) -> Result<BigInt, DreaderefError> {
        let ip = self.get_ip();
        let value = self.get_cell(ip.clone());
        self.set_cell(BigInt::from(-1), ip + BigInt::from(1));
        value
    }
    fn tick(&mut self) -> Result<bool, DreaderefError> {
        if let Some(command) = self.read_number()?.to_u8() {
            match command {
                0 => return Ok(false),
                1 => {
                    let source = self.read_number()?;
                    let destination = self.read_number()?;
                    let cell_value = self.get_cell(source)?;
                    self.set_cell(destination, cell_value);
                },
                2 => {
                    let a = self.read_number()?;
                    let b = self.read_number()?;
                    let destination = self.read_number()?;
                    self.set_cell(destination, a + b);
                },
                3 => {
                    let a = self.read_number()?;
                    let b = self.read_number()?;
                    let destination = self.read_number()?;
                    self.set_cell(destination, a * b);
                },
                4 => {
                    let a = self.read_number()?;
                    let destination = self.read_number()?;
                    let result = if a == BigInt::from(0) {
                        0
                    } else {
                        1
                    };
                    self.set_cell(destination, BigInt::from(result));
                },
                5 => {
                    let a = self.read_number()?;
                    for c in format!("{}", a).chars() {
                        (self.output_fn)(c);
                    }
                },
                6 => {
                    let a = self.read_number()?;
                    if let Some(c) = a.to_u32().and_then(char::from_u32) {
                        (self.output_fn)(c);
                    }
                },
                7 => {
                    let destination = self.read_number()?;
                    let input = BigInt::from(self.input_stream.next().map_or(-1, |x| x.unwrap() as i16));
                    self.set_cell(destination, input);
                }
                _ => {},
            }
        }
        Ok(true)
    }
    fn run(&mut self) -> Result<(), DreaderefError> {
        while self.tick()? { }
        Ok(())
    }
}

pub fn run<I, H, F>(
    code: &str,
    holes: H,
    input: I,
    output_fn: F
) -> Result<(), String>
    where H: Iterator<Item=BigInt>,
          I: Read,
          F: FnMut(char)
{
    let tokens = lex::lex(code)?;
    Dreaderef::new(tokens, holes, input.bytes(), output_fn)
        .run()
        .map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn run(code: &str, input: &str, holes: Vec<BigInt>)
    -> Result<(HashMap<BigInt, Token>, String), DreaderefError> {
        let mut output = String::new();
        let state = {
            let mut interpreter = Dreaderef::new(
                lex::lex(code).unwrap(),
                holes.into_iter(),
                input.bytes(),
                |c| output.push(c)
            );
            interpreter.run()?;
            interpreter.state
        };
        Ok((state, output))
    }
    
    // Shortcut for creating BigInts
    fn b(n: i64) -> BigInt { BigInt::from(n) }
    
    macro_rules! assert_final_state {
        ($code:expr, $input:expr, $holes:expr, $expected:expr) => {
            assert_eq!(
                run($code, $input, $holes)
                    .unwrap_or_else(|error| ).0,
                $expected
                    .into_iter()
                    .map(|(k, v)| (BigInt::from(k), Token::Literal(v)))
                    .collect()
            );
        }
    }
    
    macro_rules! assert_output {
        ($code:expr, $input:expr, $holes:expr, $expected:expr) => {
            assert_eq!(
                run($code, $input, $holes).unwrap().1,
                String::from($expected)
            );
        }
    }
    
    #[test]
    fn test_commands() {
        assert_eq!(
            run("end", "", vec![]).unwrap().0,
            run("",    "", vec![]).unwrap().0
        );
        assert_final_state!(
            "deref 0 1  end", "", vec![],
            vec![
                (-1, b(4)),
                (0, b(1)),
                (1, b(1)),
                (2, b(1)),
                (3, b(0)),
            ]
        );
        assert_final_state!(
            "add 102 617 5", "", vec![],
            vec![
                (-1, b(5)),
                (0, b(2)),
                (1, b(102)),
                (2, b(617)),
                (3, b(5)),
                (4, b(0)),
                (5, b(719)),
            ]
        );
        assert_final_state!(
            "mul -7 6 18", "", vec![],
            vec![
                (-1, b(5)),
                (0, b(3)),
                (1, b(-7)),
                (2, b(6)),
                (3, b(18)),
                (4, b(0)),
                (18, b(-42)),
            ]
        );
        assert_final_state!(
            "bool -3 0  bool 0 3  bool 101 6", "", vec![],
            vec![
                (-1, b(10)),
                
                (0, b(1)),
                (1, b(-3)),
                (2, b(0)),
                
                (3, b(0)),
                (4, b(0)),
                (5, b(3)),
                
                (6, b(1)),
                (7, b(101)),
                (8, b(6)),
                
                (9, b(0))
            ]
        );
        assert_output!(
            "numo 1  numo 2  numo 102938472034958723049571032976029", "", vec![],
            "12102938472034958723049571032976029"
        );
        assert_output!(
            "chro 'h'  chro 'e'  chro 'l'  chro 'l'  chro 'o'", "", vec![],
            "hello"
        );
        assert_final_state!(
            "chri 1  chri 3", "hi <junk data that will not be read>", vec![],
            vec![
                (-1, b(5)),
                
                (0, b(7)),
                (1, b('h' as i64)),
                
                (2, b(7)),
                (3, b('i' as i64)),
                
                (4, b(0)),
            ]
        );
    }
    
    #[test]
    fn unset() {
        assert_error!(
        )
    }
}