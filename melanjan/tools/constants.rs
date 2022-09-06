pub type Reg = i16;

const MAX: Reg = 116;
const MIN: Reg = -115;

#[repr(align(4))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct State(Reg, Reg, Reg);

impl State {
    fn valid(self) -> bool {
        let Self(a, b, c) = self;
        let r = MIN..=MAX;
        r.contains(&a) && r.contains(&b) && r.contains(&c)
    }

    fn neighbors(self) -> [Self; 12] {
        let Self(a, b, c) = self;
        [
            // v = v - v
            [0, b, c],
            [a, 0, c],
            [a, b, 0],
            // v = 1 - v
            [1 - a, b, c],
            [a, 1 - b, c],
            [a, b, 1 - c],
            // v = u - v
            [a, a - b, c],
            [a, b, a - c],
            [b - a, b, c],
            [a, b, b - c],
            [c - a, b, c],
            [a, c - b, c],
        ]
        .map(|[x, y, z]| Self(x, y, z))
    }
}

static INSTRUCTIONS: [[u8; 2]; 12] = [
    *b"aa", *b"bb", *b"cc", *b"a1", *b"b1", *b"c1", *b"ba", *b"ca", *b"ab", *b"cb", *b"ac", *b"bc",
];

struct Record {
    derived_from: State,
}

fn go(start: State, mut is_end: impl FnMut(State) -> bool) -> Option<(String, State)> {
    use std::collections::hash_map::Entry;
    use std::collections::{HashMap, VecDeque};

    let mut map = HashMap::from([(
        start,
        Record {
            derived_from: start,
        },
    )]);
    let mut frontier = VecDeque::from([start]);

    let (last, penultimate) = 'bfs: loop {
        let state = frontier.pop_front()?;
        for neighbor in state.neighbors() {
            if !neighbor.valid() {
                continue;
            }
            if is_end(neighbor) {
                break 'bfs (neighbor, state);
            }
            if let Entry::Vacant(entry) = map.entry(neighbor) {
                entry.insert(Record {
                    derived_from: state,
                });
                frontier.push_back(neighbor);
            }
        }
    };

    let (mut current, mut prev) = (last, penultimate);
    let mut chars = Vec::new();
    while current != start {
        for (cmd, n) in INSTRUCTIONS.iter().zip(prev.neighbors()) {
            if n == current {
                // push in reverse order
                chars.push(cmd[1]);
                chars.push(cmd[0]);
                break;
            }
        }
        current = prev;
        prev = map[&current].derived_from;
    }

    chars.reverse();
    Some((String::from_utf8(chars).unwrap(), last))
}

fn main() {
    let args = std::env::args()
        .skip(1)
        .map(|arg| arg.parse::<Reg>().unwrap())
        .collect::<Vec<_>>();
    let (start, target);
    match args.len() {
        1 => {
            start = State(0, 0, 0);
            target = args[0];
        }
        4 => {
            start = State(args[1], args[2], args[3]);
            target = args[0];
        }
        _ => {
            eprintln!("usage: ./constants [target] [optional starting register values]");
            return;
        }
    }
    let is_end = |st: State| st.0 == target || st.1 == target || st.2 == target;
    if let Some((commands, final_state)) = go(start, is_end) {
        let State(a, b, c) = final_state;
        println!("{commands}\na={a}, b={b}, c={c}");
    } else {
        println!("No solution found.");
    }
}
