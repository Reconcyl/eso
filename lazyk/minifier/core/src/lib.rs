use indexmap::IndexSet;

use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::fmt;

type TermId = u32;

const S_ID: TermId = 0;
const K_ID: TermId = 1;
const I_ID: TermId = 2;
const NCOMB: u32 = 3;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Application {
    lhs: TermId,
    rhs: TermId,
}

enum Tm {
    S,
    K,
    I,
    Ap(Application),
}

struct TermCache(IndexSet<Application>);

impl TermCache {
    fn new() -> Self {
        Self(IndexSet::new())
    }

    fn add(&mut self, t: Tm) -> TermId {
        match t {
            Tm::S => S_ID,
            Tm::K => K_ID,
            Tm::I => I_ID,
            Tm::Ap(appl) => {
                let (id, _added) = self.0.insert_full(appl);
                u32::try_from(id + (NCOMB as usize)).unwrap()
            }
        }
    }

    fn lookup(&self, id: TermId) -> Tm {
        match id {
            S_ID => Tm::S,
            K_ID => Tm::K,
            I_ID => Tm::I,
            NCOMB.. => Tm::Ap(self.0[(id - NCOMB) as usize]),
        }
    }
}

#[derive(Clone)]
struct Code(im::Vector<u8>);

impl From<&'static str> for Code {
    fn from(s: &'static str) -> Self {
        Self(im::Vector::from_iter(s.bytes()))
    }
}

impl Code {
    fn score(&self) -> impl Ord {
        cmp::Reverse(self.0.len())
    }

    fn to_vec(self) -> Vec<u8> {
        self.0.into_iter().collect()
    }
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let utf8: Vec<_> = self.0.iter().copied().collect();
        String::from_utf8_lossy(&utf8).fmt(f)
    }
}

trait FindBest<Rhs> {
    type Output;
    fn of(self, rhs: Rhs) -> Self::Output;
}

struct Best();

impl<T> FindBest<T> for Best {
    type Output = T;
    fn of(self, rhs: T) -> T {
        rhs
    }
}

impl FindBest<Code> for Code {
    type Output = Code;
    fn of(self, other: Code) -> Code {
        cmp::max_by_key(other, self, Code::score)
    }
}

impl FindBest<Option<Code>> for Code {
    type Output = Code;
    fn of(self, other: Option<Code>) -> Code {
        match other {
            Some(o) => <Code as FindBest<Code>>::of(self, o),
            None => self,
        }
    }
}

impl FindBest<Code> for Option<Code> {
    type Output = Code;
    fn of(self, other: Code) -> Code {
        match self {
            Some(o) => <Code as FindBest<Code>>::of(o, other),
            None => other,
        }
    }
}

struct DpCache<T>(RefCell<HashMap<TermId, T>>);

impl<T: Clone> DpCache<T> {
    fn new() -> Self {
        Self(RefCell::new(HashMap::new()))
    }

    fn get(&self, id: TermId, f: impl FnOnce() -> T) -> T {
        if let Some(o) = self.0.borrow().get(&id) {
            return o.clone();
        }
        let value = f();
        let old_value = self.0.borrow_mut().insert(id, value.clone());
        assert!(
            old_value.is_none(),
            "reëntrant computation of a key in the DpCache?"
        );
        value
    }
}

fn is_jot(b: u8) -> bool {
    b == b'0' || b == b'1'
}

struct Memo {
    terms: TermCache,
    cache_jot: DpCache<Option<Code>>,
    cache_lazyk_atom: DpCache<Code>,
    cache_lazyk_seq: DpCache<Code>,
    cache_iota: DpCache<Code>,
}

impl Memo {
    fn new(terms: TermCache) -> Self {
        Self {
            terms,
            cache_jot: DpCache::new(),
            cache_lazyk_atom: DpCache::new(),
            cache_lazyk_seq: DpCache::new(),
            cache_iota: DpCache::new(),
        }
    }

    fn lookup(&self, t: TermId) -> Tm {
        self.terms.lookup(t)
    }

    fn lookap(&self, a: Application) -> (Tm, Tm) {
        (self.terms.lookup(a.lhs), self.terms.lookup(a.rhs))
    }

    fn best_jot(&self, t: TermId) -> Option<Code> {
        self.cache_jot.get(t, || self.compute_best_jot(t))
    }

    fn compute_best_jot(&self, t: TermId) -> Option<Code> {
        match self.lookup(t) {
            Tm::K => Some("00".into()),
            Tm::S => Some("000".into()),
            Tm::Ap(ap) => match self.lookap(ap) {
                (Tm::S, Tm::K) | (Tm::K, Tm::I) => Some("0".into()),
                (Tm::Ap(ap2), Tm::K) if ap2.rhs == S_ID => {
                    let mut inner = self.best_jot(ap2.lhs)?;
                    inner.0.push_back(b'0');
                    Some(inner)
                }
                (Tm::S, Tm::Ap(ap2)) if ap2.lhs == K_ID => {
                    let mut inner = self.best_jot(ap2.rhs)?;
                    inner.0.push_back(b'1');
                    Some(inner)
                }
                (Tm::K, Tm::Ap(ap2)) if ap2.lhs == K_ID => {
                    // N.B. this is ap.rhs, not ap2.rhs!
                    let mut inner = self.best_jot(ap.rhs)?;
                    inner.0.push_back(b'1');
                    Some(inner)
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn best_lazyk_atom(&self, t: TermId) -> Code {
        self.cache_lazyk_atom
            .get(t, || self.compute_best_lazyk_atom(t))
    }

    fn compute_best_lazyk_atom(&self, t: TermId) -> Code {
        if self.is_iota_combinator(t) {
            return "*Ii".into();
        }
        Best()
            .of(match self.lookup(t) {
                Tm::I => "i".into(),
                Tm::K => "k".into(),
                Tm::S => "s".into(),
                Tm::Ap(ap) => Best()
                    .of({
                        let mut s = self.best_lazyk_atom(ap.lhs);
                        let t = self.best_lazyk_atom(ap.rhs);
                        s.0.push_front(b'`');
                        if is_jot(*s.0.back().unwrap()) && is_jot(*t.0.front().unwrap()) {
                            s.0.push_back(b' ');
                        }
                        s.0.append(t.0);
                        s
                    })
                    .of({
                        let mut s = self.best_iota(ap.lhs);
                        let t = self.best_iota(ap.rhs);
                        s.0.push_front(b'*');
                        if is_jot(*s.0.back().unwrap()) && is_jot(*t.0.front().unwrap()) {
                            s.0.push_back(b' ');
                        }
                        s.0.append(t.0);
                        s
                    }),
            })
            .of(match self.lookup(t) {
                Tm::Ap(ap) => {
                    let mut s = self.best_lazyk_seq(ap.lhs);
                    let mut t = self.best_lazyk_atom(ap.rhs);
                    s.0.push_front(b'(');
                    if is_jot(*s.0.back().unwrap()) && is_jot(*t.0.front().unwrap()) {
                        t.0.push_front(b' ');
                    }
                    s.0.append(t.0);
                    s.0.push_back(b')');
                    Some(s)
                }
                _ => None,
            })
            .of(self.best_jot(t))
    }

    fn best_lazyk_seq(&self, t: TermId) -> Code {
        self.cache_lazyk_seq
            .get(t, || self.compute_best_lazyk_seq(t))
    }

    fn compute_best_lazyk_seq(&self, t: TermId) -> Code {
        Best()
            .of(match self.lookup(t) {
                Tm::Ap(ap) => {
                    let mut s = self.best_lazyk_seq(ap.lhs);
                    let mut t = self.best_lazyk_atom(ap.rhs);
                    if is_jot(*s.0.back().unwrap()) && is_jot(*t.0.front().unwrap()) {
                        t.0.push_front(b' ');
                    }
                    s.0.append(t.0);
                    Some(s)
                }
                _ => None,
            })
            .of(self.best_lazyk_atom(t))
    }

    fn best_iota(&self, t: TermId) -> Code {
        self.cache_iota.get(t, || self.compute_best_iota(t))
    }

    fn is_iota_combinator(&self, t: TermId) -> bool {
        match self.lookup(t) {
            Tm::Ap(ap) => match self.lookap(ap) {
                (Tm::Ap(ap2), Tm::Ap(ap3)) => match (self.lookap(ap2), self.lookap(ap3)) {
                    ((Tm::S, Tm::Ap(ap4)), (Tm::K, Tm::K)) => match self.lookap(ap4) {
                        (Tm::Ap(ap5), Tm::Ap(ap6)) => match (self.lookap(ap5), self.lookap(ap6)) {
                            ((Tm::S, Tm::I), (Tm::K, Tm::S)) => true,
                            _ => false,
                        },
                        _ => false,
                    },
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        }
    }

    fn compute_best_iota(&self, t: TermId) -> Code {
        if self.is_iota_combinator(t) {
            return "i".into();
        }

        match self.lookup(t) {
            Tm::I => "I".into(),
            Tm::K => "k".into(),
            Tm::S => "s".into(),
            Tm::Ap(ap) => match self.lookap(ap) {
                (Tm::Ap(ap2), Tm::K) if ap2.rhs == S_ID => {
                    let mut inner = self.best_iota(ap2.lhs);
                    inner.0.push_front(b'i');
                    inner.0.push_front(b'*');
                    inner
                }
                _ => {
                    let mut s = self.best_lazyk_atom(t);
                    if s.0.len() == 1 {
                        s.0[0] = b'I';
                    }
                    s
                }
            },
        }
    }
}

fn parse(code: &[u8]) -> (TermCache, TermId) {
    let mut cache = TermCache::new();
    let mut parse_stack = Vec::new();
    for &b in code.iter().rev() {
        match b {
            b'i' => parse_stack.push(I_ID),
            b'k' => parse_stack.push(K_ID),
            b's' => parse_stack.push(S_ID),
            b'`' => {
                let lhs = parse_stack.pop().expect("invalid syntax");
                let rhs = parse_stack.pop().expect("invalid syntax");
                parse_stack.push(cache.add(Tm::Ap(Application { lhs, rhs })));
            }
            b' ' | b'\n' | b'(' | b'0'..=b'9' | b')' => {}
            _ => panic!("invalid byte: {}", std::ascii::escape_default(b)),
        }
    }

    assert_eq!(parse_stack.len(), 1, "code must consist of a single term");
    (cache, parse_stack[0])
}

pub fn minify(code: &[u8]) -> Vec<u8> {
    let (cache, tm) = parse(code);
    Memo::new(cache).best_lazyk_seq(tm).to_vec()
}
