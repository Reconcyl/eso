use std::str::Chars;
use std::mem;
use std::string::ToString;

use num::BigInt;
use num::bigint::Sign;

macro_rules! try_option {
    ($e: expr) => (match $e {
        Some(val) => val,
        None => return None,
    })
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Literal(BigInt),
    Unset,
    Hole,
}

#[derive(Debug, PartialEq, Eq)]
struct LexError {
    row: usize,
    col: usize,
    mes: String
}

impl ToString for LexError {
    fn to_string(&self) -> String {
        format!("Lex error ({}:{}): {}", self.row, self.col, &self.mes)
    }
}

struct LineLexer<'a> {
    row: usize,
    col: usize,
    chars: Chars<'a>,
    char_cache: Option<char>,
    tokens: Vec<Token>,
}

impl<'a> LineLexer<'a> {
    fn new(tokens: Vec<Token>, row: usize, chars: Chars<'a>) -> Self {
        LineLexer { tokens, row, col: 0, chars, char_cache: None }
    }
    fn update_cache(&mut self, c: char) {
        self.char_cache = Some(c);
    }
    fn error(&self, mes: String) -> Result<(), LexError> {
        Err(LexError {
            row: self.row,
            col: self.col,
            mes
        })
    }
    fn next_char(&mut self) -> Option<char> {
        self.col += 1;
        if let Some(c) = self.char_cache {
            self.char_cache = None;
            Some(c)
        } else {
            self.chars.next()
        }
    }
    fn push_literal(&mut self, literal: BigInt) {
        self.tokens.push(Token::Literal(literal));
    }
    
    fn read_digits(&mut self, digits: &mut Vec<u8>) {
        loop {
            let c = match self.next_char() {
                Some(c) => c,
                None => break,
            };
            if let Some(digit) = c.to_digit(10) {
                digits.push(digit as u8);
            } else {
                self.update_cache(c);
                break;
            }
        }
    }
    fn scan_positive(&mut self, first_digit: u8) -> Result<(), LexError> {
        let mut digits = vec![first_digit];
        self.read_digits(&mut digits);
        self.push_literal(BigInt::from_radix_be(Sign::Plus, &*digits, 10).unwrap());
        Ok(())
    }
    fn scan_negative(&mut self) -> Result<(), LexError> {
        let mut digits = Vec::new();
        self.read_digits(&mut digits);
        if digits.is_empty() {
            return self.error(format!("expected digit after '-', got {:?}", self.char_cache.unwrap()));
        }
        self.push_literal(BigInt::from_radix_be(Sign::Minus, &*digits, 10).unwrap());
        Ok(())
    }
    fn scan_name(&mut self, first_letter: char) -> Result<(), LexError> {
        let mut name = first_letter.to_string();
        loop {
            let c = match self.next_char() {
                Some(c) => c,
                None => break,
            };
            if c.is_lowercase() {
                name.push(c);
            } else {
                self.update_cache(c);
                break;
            }
        }
        let number = match &*name {
            "end" => 0,
            "deref" => 1,
            "add" => 2,
            "mul" => 3,
            "bool" => 4,
            "numo" => 5,
            "chro" => 6,
            "chri" => 7,
            other => return self.error(format!(
                "word must be one of {{end, deref, add, mul, bool, numo, chro, chri}}, \
                got {:?}", other))
        };
        self.push_literal(BigInt::from(number));
        Ok(())
    }
    fn scan_string(&mut self, quote: char) -> Result<(), LexError> {
        let mut escaped = false;
        loop {
            let c = match self.next_char() {
                Some(c) => c,
                None => return self.error(format!("expected closing quote before end-of-line"))
            };
            let c = if escaped {
                escaped = false;
                match c {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '0' => '\0',
                    q if q == quote => q,
                    '\\' => '\\',
                    other => return self.error(format!(
                        "unrecognized escape sequence '\\{}'", other)),
                }
            } else {
                match c {
                    q if q == quote => break,
                    '\\' => { escaped = true; continue },
                    other => other,
                }
            };
            self.push_literal(BigInt::from(c as u32));
        }
        Ok(())
    }
    fn unset(&mut self) -> Result<(), LexError> {
        self.tokens.push(Token::Unset);
        Ok(())
    }
    fn hole(&mut self) -> Result<(), LexError> {
        self.tokens.push(Token::Hole);
        Ok(())
    }
    fn scan_token(&mut self) -> Option<Result<(), LexError>> {
        let first_char = try_option!(self.next_char());
        Some(match first_char {
            c if c.is_whitespace() => Ok(()),
            c if c.is_digit(10) => self.scan_positive(c.to_digit(10).unwrap() as u8),
            '-' => self.scan_negative(),
            c if c.is_lowercase() => self.scan_name(c),
            '"' => self.scan_string('"'),
            '\'' => self.scan_string('\''),
            '?' => self.unset(),
            '*' => self.hole(),
            c => self.error(format!("unexpected character {:?}", c))
        })
    }
    fn lex(mut self) -> Result<Vec<Token>, LexError> {
        while let Some(e) = self.scan_token() {
            // If `e` is a `LexError`, propagate it.
            e?;
        }
        Ok(self.tokens)
    }
}

fn strip_comments(s: &mut String) {
    if let Some(pos) = s.find(';') {
        s.truncate(pos);
    }
    if let Some(pos) = s.find('.') {
        *s = s[pos + 1 ..].to_string();
    }
}

fn process_line(tokens: &mut Vec<Token>, row: usize, mut line: String) -> Result<(), LexError> {
    strip_comments(&mut line);
    // Temporarily steal `tokens` for use in the by the LineLexer.
    let stolen_tokens = mem::replace(tokens, Default::default());
    let stolen_tokens = LineLexer::new(stolen_tokens, row, line.chars()).lex()?;
    // Now put the `stolen_tokens` back into `tokens`.
    mem::replace(tokens, stolen_tokens);
    Ok(())
}

pub fn lex(s: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    for (row, line) in s.lines().enumerate() {
        process_line(&mut tokens, row, line.to_string())
            .map_err(|e| e.to_string())?;
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    
    use super::*;
    fn tokens(strings: &[&str]) -> Vec<Token> {
        strings.iter().map(|s| Token::Literal(BigInt::from_str(s).unwrap())).collect()
    }
    
    #[test]
    fn literals() {
        assert_eq!(lex("1 2 3 99 12314"),
            Ok(tokens(&["1", "2", "3", "99", "12314"])));
        assert_eq!(lex("10000000000 999999999999999"),
            Ok(tokens(&["10000000000", "999999999999999"])));
        assert_ne!(lex("1"), lex("2"));
        
        assert_eq!(lex("-1234"), Ok(tokens(&["-1234"])));
        assert_eq!(lex("1234-1234-1234"),
            Ok(tokens(&["1234", "-1234", "-1234"])));
        
        assert!(lex("-abc").is_err());
        assert!(lex("-1 -2 --3").is_err());
        
        assert_eq!(lex("?"), Ok(vec![Token::Unset]));
        assert_eq!(lex("*"), Ok(vec![Token::Hole]));
        
        assert!(lex("#").is_err())
    } 
    #[test]
    fn space() {
        assert!(lex("").is_ok());
        assert!(lex("  ").is_ok());
        assert_eq!(lex("    1   \n\n\n   2   \t\t\t      -1"), Ok(tokens(&["1", "2", "-1"])));
        assert!(lex("- 3").is_err());
    }    
    #[test]
    fn comments() {
        assert_eq!(lex("I like apples."), lex(""));
        assert_eq!(lex("; I like pears."), lex(""));
        assert_eq!(lex("DATA. 4 8 15 16 23 42 ; Lost numbers"), lex("4 8 15 16 23 42"));
        
        // Overlapping comments
        assert_eq!(lex("1 ; 2 . 3"), lex("1"));
    }
    #[test]
    fn string_literals() {
        assert_eq!(lex(r#" "abcdefg" "#), lex("97 98 99 100 101 102 103"));
        assert_eq!(lex(r#""""#), lex(""));
        assert_eq!(lex(r#" "abc" "def" "#), lex(r#" "abcdef" "#));
        assert_eq!(lex(r#" 'Waffles' "#), lex(r#" "Waffles" "#));
        assert_eq!(lex(r#" '\n\t\r\0' "#), lex("10 9 13 0"));
        
        assert!(lex(r#" ' "#).is_err());
        assert!(lex(r#" '\h' "#).is_err());
    }
    
    #[test]
    fn words() {
        assert_eq!(lex("end deref add mul bool numo chro chri"), lex("0 1 2 3 4 5 6 7"));
        assert!(lex("deferefedefer").is_err());
    }
    
    #[test]
    fn hello_world() {
        let code = r#"CODE.
                      ; Dereference the string pointer
                      0.  deref 24 4
                      ; End the program if the value pointed to is zero
                      3.  deref ? 7
                      6.  bool ? 11
                      9.  mul -1 ? 13
                      13. ? ; This will either be `end` or -1, which is a nop.
                      ; Otherwise, output the value
                      14. deref 7 18
                      17. chro ?
                      ; Increment the string pointer
                      19. deref 24 24
                      22. add 1 29 24
                      ; Go back to the beginning
                      26. deref 13 -1
                      DATA.
                      29. "Hello, World!\n"
        "#;
        assert_eq!(lex(code), lex(r#"1 24 4  1 ? 7  4 ? 11  3 -1 ? 13  ?  1 7 18
                                     6 ?  1 24 24  2 1 29 24  1 13 -1  "Hello, World!" 10"#));
    }
}