use num_bigint::{BigInt, Sign};

use num_traits::ToPrimitive;
use num_traits::identities::Zero;

use rand::prelude::*;
use rand::distributions::Standard;

use std::boxed::FnBox;

use std::collections::HashMap;
use std::io::prelude::*;
use std::char;

use std::fmt::{Debug, Formatter};

/// The value a blank cell is considered to have.
const BLANK: u32 = b' ' as u32;

/// If the `lower` and `upper` bounds do not contain `value`, then adjust them
/// such that they do.
fn adjust_bounds<T: Ord + Clone>(lower: &mut T, upper: &mut T, value: &T) {
    if value < lower {
        *lower = value.clone();
    } else if value > upper {
        *upper = value.clone();
    }
}

/// Parse a `u8` as an ASCII digit.
fn parse_digit(byte: u8) -> Option<u8> {
    if byte.is_ascii_digit() {
        Some(byte - b'0')
    } else {
        None
    }
}

/// Replace an `Option<T>` with a new value if a predicate holds on the current
/// and new values.
///
/// If the `Option` is `None`, always replace.
fn option_replace_if<T: Clone, F>(option: &mut Option<T>, new: &T, predicate: F)
    where F: FnOnce(&T, &T) -> bool
{
    let new_value = if let Some(current) = option.take() {
        if predicate(&current, new) { new.clone() } else { current }
    } else { new.clone() };
    *option = Some(new_value);
}

/// Acts like Python's `%` operator: the result is always positive if the
/// divisor is positive.
fn bigint_real_modulo(n: &BigInt, d: &BigInt) -> BigInt {
    let mut result = n % d;
    if &result < &BigInt::zero() {
        result += d;
    }
    result
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    pub fn opposite(self) -> Self {
        use self::Direction::*;
        match self {
            North => South,
            South => North,
            West => East,
            East => West,
        }
    }
}

impl Distribution<Direction> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Direction {
        match rng.gen_range::<u8>(0, 4) {
            0 => Direction::North,
            1 => Direction::South,
            2 => Direction::West,
            3 => Direction::East,
            _ => unreachable!()
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct Options {
    /// Determines whether attempting to output an invalid byte should wrap or
    /// cause an error.
    pub wrap_on_invalid_output: bool,
    /// Determines whether invalid characters should be treated as spaces or
    /// cause an error.
    pub ignore_invalid_terms: bool,
    /// Determines whether the proposed decimal I/O commands `.` and `&` should
    /// be supported.
    pub enable_decimal_io_extension: bool,
    /// Determines whether the result of the program should be printed.
    pub suppress_final_result: bool,
}

/// Rust doesn't have tail-call elimination. In order to properly implement
/// evaluation, we rely on a trampoline-like system where `Playfield::eval` can
/// return a request for another evaluation to be made and a function to
/// perform on the result of that evaluation.
enum EvalTrampoline<Rand, R, W> {
    /// Just return a value.
    Return(BigInt),
    /// Evaluate another cell.
    Become(EvalContext),
    /// Evaluate another cell and perform a function on the result.
    Eval(EvalContext, TrampolineFn<Rand, R, W>)
}

/// Represents the type of a trampolined function.
type TrampolineFn<Rand, R, W> =
    Box<FnBox(&mut Playfield<Rand, R, W>, BigInt)
        -> Result<EvalTrampoline<Rand, R, W>, String>>;

/// Represents a point on the grid.
type Point = (BigInt, BigInt);

/// A specification of the location and direction of an evaluation.
#[derive(Clone)]
struct EvalContext(Point, Direction);

impl Debug for EvalContext {
    fn fmt(&self, f: &mut Formatter) -> Result<(), ::std::fmt::Error> {
        write!(f, "({},{}) from {:?}", (self.0).0, (self.0).1, self.1)
    }
}

#[derive(Debug)]
struct Bounds {
    /// The lowest `y` coordinate.
    upper: BigInt,
    /// The highest `y` coordinate.
    lower: BigInt,
    /// The lowest `x` coordinate.
    left: BigInt,
    /// The higest `x` coordinate.
    right: BigInt,
}

impl Bounds {
    fn calculate_from_points<'a, I: Iterator<Item=&'a Point>>(points: I) -> Self {
        let mut lowest_y = None;
        let mut highest_y = None;
        let mut lowest_x = None;
        let mut highest_x = None;
        for &(ref x, ref y) in points {
            option_replace_if(&mut lowest_y, y, |c, n| n < c);
            option_replace_if(&mut highest_y, y, |c, n| n > c);
            option_replace_if(&mut lowest_x, x, |c, n| n < c);
            option_replace_if(&mut highest_x, x, |c, n| n > c);
        }
        Self {
            upper: lowest_y.unwrap_or_else(BigInt::zero),
            lower: highest_y.unwrap_or_else(BigInt::zero),
            left: lowest_x.unwrap_or_else(BigInt::zero),
            right: highest_x.unwrap_or_else(BigInt::zero),
        }
    }
}

struct Grid {
    data: HashMap<Point, BigInt>,
    bounds: Bounds
}

struct Playfield<Rand, R, W> {
    // Stores all the characters in the playfield. Spaces (`BigInt` value 32)
    // are not stored.
    //
    // This needs to be represented using `BigInt`s because each cell can hold
    // an arbitrary value and a program can read and write from arbitrary
    // locations on the grid.
    grid: Grid,
    
    input_stream: R,
    output_stream: W,
    
    rng: Rand,
    
    options: Options,
    
    argument_stack: Vec<BigInt>,
}

impl<Rand, R, W> Playfield<Rand, R, W> {
    /// Parse a program into a grid and return it, along with the coordinates
    /// of the `@`. If there is not exactly one `@`, return `None`.
    fn parse_grid(code: &str) -> Option<(Grid, Point)> {
        let to_bigints = |(x, y)| (BigInt::from(x), BigInt::from(y));
        let mut initial_pos = None;
        let mut data = HashMap::new();
        for (y, line) in code.lines().enumerate() {
            for (x, chr) in line.chars().enumerate() {
                let point = (x, y);
                if chr == '@' {
                    if initial_pos.is_some() {
                        return None;
                    }
                    initial_pos = Some(point.clone());
                }
                if chr != ' ' {
                    data.insert(to_bigints((x, y)), BigInt::from(chr as u32));
                }
            }
        }
        initial_pos.map(
            |pos| (Grid {
                bounds: Bounds::calculate_from_points(data.keys()),
                data,
            }, to_bigints(pos))
        )
    }
}

impl<Rand: Rng, R: Read, W: Write> Playfield<Rand, R, W> {
    /// Return the value of the cell at a given position, or 32 if no such cell
    /// exists.
    fn get_cell(&self, location: &Point) -> BigInt {
        self.grid.data.get(location).cloned().unwrap_or_else(|| BigInt::from(BLANK))
    }
    /// Set the cell at a given position to a given value. Adjust the bounds
    /// as necessary.
    fn set_cell(&mut self, (x, y): Point, target: BigInt) {
        if target == BigInt::from(BLANK) {
            self.grid.data.remove(&(x, y));
            self.update_bounding_box();
        } else {
            adjust_bounds(&mut self.grid.bounds.left, &mut self.grid.bounds.right, &x);
            adjust_bounds(&mut self.grid.bounds.upper, &mut self.grid.bounds.lower, &y);
            self.grid.data.insert((x, y), target);
        }
    }
    /// Shrink the bounding box if possible.
    ///
    /// This is implemented by going through all non-space characters in the
    /// grid. There's probably not going to be *too* many of those, so this
    /// shouldn't be super inefficient.
    fn update_bounding_box(&mut self) {
        self.grid.bounds = Bounds::calculate_from_points(
            self.grid.data.keys());
    }
    /// Output the `BigInt` as a byte. Error if the `BigInt` cannot be written
    /// for whatever reason.
    fn output_byte(&mut self, mut byte: BigInt) -> Result<(), String> {
        if self.options.wrap_on_invalid_output {
            byte = bigint_real_modulo(&byte, &BigInt::from(0x100))
        }
        let byte = byte.to_u8().ok_or_else(
            || format!("Failed to output: {} does not fit in a byte.\n\
                        You can pass the `--wrap-on-invalid-output` flag \
                        to truncate.", &byte))?;
        self.output_stream.write_all(&[byte]).map_err(
            |e| format!("Failed to output: {}", e))
    }
    /// Output the `BigInt` in decimal. Error if the `BigInt` cannot be written
    /// for whatever reason.
    fn output_decimal(&mut self, n: BigInt) -> Result<(), String> {
        self.output_stream.write_all(n.to_string().as_bytes())
            .map_err(|e| format!("Failed to output: {}", e))
    }
    /// Input a byte and return it. Return `None` on EOF. Return an error if an
    /// I/O error ocurred.
    fn input_byte(&mut self) -> Result<Option<u8>, String> {
        let mut buffer = [0];
        match self.input_stream.read(&mut buffer) {
            Err(e) => Err(format!("Failed to input: {}", e))?,
            Ok(bytes_read) if bytes_read == 0 => Ok(None),
            Ok(_)                             => Ok(Some(buffer[0])),
        }
    }
    /// Input a decimal value and return it as a `BigInt`. Error on EOF, or if
    /// the decimal cannot be read. Ignore all data until a literal is found,
    /// and ignore a single byte after.
    fn input_decimal(&mut self) -> Result<BigInt, String> {
        // Scan until a digit or `-` is found.
        let sign: Sign;
        let mut digits: Vec<u8>;
        
        /// Convenience macro for reading a byte.
        macro_rules! byte {
            () => {
                self.input_byte()?.ok_or_else(
                    || String::from("Failed to input: reached EOF"))?
            }
        }
        
        // Discard bytes until we find a `-` or a digit.
        loop {
            match byte!() {
                b'-' => {
                    sign = Sign::Minus;
                    digits = Vec::new();
                    break;
                },
                b => if let Some(digit) = parse_digit(b) {
                    sign = Sign::Plus;
                    digits = vec![digit];
                    break;
                }
            }
        }
        // Read bytes until we find something that's not a digit.
        while let Some(digit) = parse_digit(byte!()) {
            digits.push(digit);
        }
        Ok(BigInt::from_radix_be(sign, &digits, 10).unwrap())
    }
    /// Push a value to the argument stack.
    fn push_argument(&mut self, argument: BigInt) {
        self.argument_stack.push(argument);
    }
    /// Return the top value from the argument stack, or 0 if there is no argument.
    fn get_argument(&mut self) -> BigInt {
        self.argument_stack.last().cloned().unwrap_or_else(BigInt::zero)
    }
    /// Remove the top value from the argument stack.
    fn pop_argument(&mut self) -> Option<BigInt> {
        self.argument_stack.pop()
    }
    /// Take a set of coordinates and cycle them so that they point to cells
    /// that are in bounds.
    fn cycle(&self, (mut x, mut y): Point) -> Point {
        x -= &self.grid.bounds.left;
        x = bigint_real_modulo(&x, &(&self.grid.bounds.right - &self.grid.bounds.left + 1));
        x += &self.grid.bounds.left;
        
        y -= &self.grid.bounds.upper;
        y = bigint_real_modulo(&y, &(&self.grid.bounds.lower - &self.grid.bounds.upper + 1));
        y += &self.grid.bounds.upper;
        
        (x, y)
    }
    /// Return the neighbor of a position to a given direction, respecting
    /// wrapping rules.
    fn neighbor(&self, (mut x, mut y): Point, direction: Direction, n: u32) -> Point {
        match direction {
            Direction::North => y -= n,
            Direction::South => y += n,
            Direction::West => x -= n,
            Direction::East => x += n,
        }
        self.cycle((x, y))
    }
    /// Return the `EvalContext` for a neighbor.
    fn neighbor_context(&self, location: Point, direction: Direction, n: u32)
        -> EvalContext
    {
        EvalContext(
            self.neighbor(location, direction, n),
            direction.opposite())
    }
    /// Evaluate a cell at a given location from a given direction.
    fn eval(&mut self, context: EvalContext)
        -> Result<EvalTrampoline<Rand, R, W>, String>
    {
        use self::EvalTrampoline::*;
        let EvalContext(location, direction) = context;
        
        /// Convenience macro to construct a `Eval` variant.
        macro_rules! tail_call {
            ($context:expr, $function:expr) => {
                Ok(Eval($context, Box::new($function)))
            };
            ($context:expr) => {
                Ok(Become($context))
            }
        }
        /// Convenience macro to create a nested tail call.
        macro_rules! binary_operator {
            ($loc_1:expr, $dir_1:expr, $loc_2:expr, $dir_2:expr, $function:expr) => {
                tail_call!(
                    self.neighbor_context($loc_1, $dir_1, 1),
                    move |self_: &mut Self, a: BigInt|
                        tail_call!(
                            self_.neighbor_context($loc_2, $dir_2, 1),
                            move |self_: &mut Self, b: BigInt| $function(self_, a, b)
                        )
                )
            }
        }
        
        let cell_value = self.get_cell(&location);
        let char_value = cell_value.to_u32();
        let char_value = match char_value.and_then(char::from_u32) {
            Some(chr) => chr,
            None => {
                if self.options.ignore_invalid_terms {
                    ' '
                } else {
                    return Err(format!(
                        "There is no character with code point {}", cell_value))
                }
            }
        };
        
        match char_value {
            '<' | '@'
                => tail_call!(self.neighbor_context(location, Direction::West, 1)),
            '>' => tail_call!(self.neighbor_context(location, Direction::East, 1)),
            'v' => tail_call!(self.neighbor_context(location, Direction::South, 1)),
            '^' => tail_call!(self.neighbor_context(location, Direction::North, 1)),
            ' ' => tail_call!(self.neighbor_context(location, direction.opposite(), 1)),
            '#' => tail_call!(self.neighbor_context(location, direction.opposite(), 2)),
            '!' => {
                tail_call!(
                    self.neighbor_context(location, direction.opposite(), 1),
                    |_: &mut Self, a: BigInt| Ok(Return(BigInt::from(
                        if a.is_zero() { 1 } else { 0 }
                    )))
                )
            }
            '+' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location,         Direction::South,
                    |_, a, b| Ok(Return(a + b))
                )
            }
            '-' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location,         Direction::South,
                    |_, a, b| Ok(Return(a - b))
                )
            }
            '*' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location,         Direction::South,
                    |_, a, b| Ok(Return(a * b))
                )
            }
            '/' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location.clone(), Direction::South,
                    |self_: &mut Self, a: BigInt, b: BigInt| {
                        if b.is_zero() {
                            tail_call!(self_.neighbor_context(location, direction.opposite(), 1))
                        } else {
                            Ok(Return(a / b))
                        }
                    }
                )
            }
            '%' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location.clone(), Direction::South,
                    |self_: &mut Self, a: BigInt, b: BigInt| {
                        if b.is_zero() {
                            tail_call!(self_.neighbor_context(location, direction.opposite(), 1))
                        } else {
                            // Rust's `%` operator happens to have the exact
                            // semantics we want with regard to negative numbers.
                            Ok(Return(a % b))
                        }
                    }
                )
            }
            '_' => {
                tail_call!(
                    self.neighbor_context(location.clone(), direction.opposite(), 1),
                    |self_: &mut Self, a: BigInt| {
                        let direction = if a.is_zero() {
                            Direction::East
                        } else {
                            Direction::West
                        };
                        tail_call!(self_.neighbor_context(location, direction, 1))
                    }
                )
            }
            '|' => {
                tail_call!(
                    self.neighbor_context(location.clone(), direction.opposite(), 1),
                    |self_: &mut Self, a: BigInt| {
                        let direction = if a.is_zero() {
                            Direction::South
                        } else {
                            Direction::North
                        };
                        tail_call!(self_.neighbor_context(location, direction, 1))
                    }
                )
            }
            '`' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location,         Direction::South,
                    |_, a, b| Ok(Return(BigInt::from((a > b) as u8)))
                )
            }
            '?' => {
                let direction = self.rng.gen();
                tail_call!(self.neighbor_context(location, direction, 1))
            }
            'g' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location, Direction::South,
                    |self_: &mut Self, x: BigInt, y: BigInt| Ok(Return(self_.get_cell(&(x, y))))
                )
            }
            'p' => {
                binary_operator!(
                    location.clone(), Direction::North,
                    location.clone(), Direction::South,
                    |self_: &mut Self, x: BigInt, y: BigInt| {
                        tail_call!(
                            self_.neighbor_context(location, direction.opposite(), 1),
                            |self_: &mut Self, target: BigInt| {
                                self_.set_cell((x, y), target);
                                Ok(Return(BigInt::zero()))
                            }
                        )
                    }
                )
            }
            // The argument-stack manipulation commands modify the stack
            // in-place rather than making copies of it. This should not cause
            // an observable difference in behavior, because a function can
            // only inspect the top of the stack at any given time, and every
            // pop is always paired with a push and vice versa.
            '\\' => {
                tail_call!(
                    self.neighbor_context(location.clone(), Direction::South, 1),
                    move |self_: &mut Self, argument: BigInt| {
                        self_.push_argument(argument);
                        tail_call!(
                            self_.neighbor_context(location, direction.opposite(), 1),
                            |self_: &mut Self, result: BigInt| {
                                self_.pop_argument();
                                Ok(Return(result))
                            }
                        )
                    }
                )
            }
            ':' => Ok(Return(self.get_argument())),
            '$' => {
                let argument = self.pop_argument().unwrap_or_else(BigInt::zero);
                tail_call!(
                    self.neighbor_context(location, direction.opposite(), 1),
                    |self_: &mut Self, result: BigInt| {
                        self_.push_argument(argument);
                        Ok(Return(result))
                    }
                )
            }
            ',' => {
                tail_call!(
                    self.neighbor_context(location, direction.opposite(), 1),
                    |self_: &mut Self, a: BigInt| {
                        self_.output_byte(a)?;
                        Ok(Return(BigInt::zero()))
                    }
                )
            }
            '~' => {
                Ok(Return(
                    self.input_byte()?
                        .map(BigInt::from)
                        .unwrap_or_else(|| BigInt::from(-1))
                ))
            }
            '.' if self.options.enable_decimal_io_extension => {
                tail_call!(
                    self.neighbor_context(location, direction.opposite(), 1),
                    |self_: &mut Self, a: BigInt| {
                        self_.output_decimal(a)?;
                        Ok(Return(BigInt::zero()))
                    }
                )
            }
            '&' if self.options.enable_decimal_io_extension => {
                self.input_decimal().map(Return)
            }
            c => {
                if let Some(digit) = c.to_digit(10) {
                    Ok(Return(BigInt::from(digit)))
                } else if self.options.ignore_invalid_terms {
                    tail_call!(self.neighbor_context(location, direction.opposite(), 1))
                } else {
                    Err(format!("{:?} is not a valid term.", c))
                }
            }
        }
    }
    /// Run the program from a given starting location.
    fn run(&mut self, location: Point) -> Result<BigInt, String> {
        let mut function_stack = Vec::new();
        enum Task<Rand, R, W> {
            Eval(EvalContext),
            Apply(TrampolineFn<Rand, R, W>, BigInt)
        }
        let mut current_task = Task::Eval(EvalContext(location, Direction::East));
        loop {
            let result = match current_task {
                Task::Eval(context) => self.eval(context),
                
                // This line is the only reason we need nightly.
                //
                // It's currently impossible to call a `Box<FnOnce>` due to DST
                // restrictions, so we need to use the unstable `FnBox` instead.
                // You still can't use the sugar to call it because it takes a
                // reference argument, which is desugared to a higher-rank
                // lifetime that causes problems. As such, we need to use the
                // `call_box` desugaring instead.
                //
                // In addition, another issue with `FnBox` required me to
                // annotate most of the closure parameters within `eval()`.
                Task::Apply(func, input) => func.call_box((self, input)),
            }?;
            current_task = match result {
                EvalTrampoline::Return(i) => {
                    if let Some(func) = function_stack.pop() {
                        Task::Apply(func, i)
                    } else {
                        return Ok(i)
                    }
                }
                EvalTrampoline::Become(context) => Task::Eval(context),
                EvalTrampoline::Eval(context, func) => {
                    function_stack.push(func);
                    Task::Eval(context)
                }
            };
        }
    }
}

pub fn run<Rand: Rng, R: Read, W: Write>(
    code: &str,
    input_stream: R,
    output_stream: W,
    rng: Rand,
    options: Options,
) -> Result<(), String> {
    let (grid, start) = Playfield::<Rand, R, W>::parse_grid(code)
        .ok_or_else(|| String::from("Program does not contain exactly one @"))?;
    let mut playfield = Playfield {
        grid,
        input_stream,
        output_stream,
        rng,
        options,
        argument_stack: Vec::new(),
    };
    playfield.run(start).and_then(|result| {
        if options.suppress_final_result {
            Ok(())
        } else {
            playfield.output_stream.write_all(
                result.to_string().as_bytes()
            ).map_err(|e| e.to_string())
        }
    })
}