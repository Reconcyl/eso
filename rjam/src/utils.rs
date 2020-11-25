use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive as _;

use std::collections::VecDeque;
use std::cmp::Ordering;
use std::iter::{self, FromIterator};
use std::ops::Index;

/// Reverse an `im::Vector`.
pub fn reverse_vector<T: Clone>(vec: &mut im::Vector<T>) {
    for i in 0 .. vec.len() / 2 {
        vec.swap(i, vec.len() - 1 - i);
    }
}

pub fn get_wrapping<T>(vals: &[T], idx: BigInt) -> Option<&T> {
    let div = vals.len();
    if div == 0 {
        return None;
    }
    let result = idx % div;
    let wrapped_idx = if result.sign() == Sign::Minus {
        result + div
    } else {
        result
    }.to_usize().unwrap();
    Some(&vals[wrapped_idx])
}

/// Compare `a` and `b` in a total ordering. The implementation of
/// this is taken from `f64::total_cmp`, which is not yet stable.
pub fn f64_total_cmp(a: f64, b: f64) -> Ordering {
    let mut a = a.to_bits() as i64;
    let mut b = b.to_bits() as i64;

    a ^= (((a >> 63) as u64) >> 1) as i64;
    b ^= (((b >> 63) as u64) >> 1) as i64;
    a.cmp(&b)
}

/// Compare a `u32` and a `BigInt`.
pub fn bigint_u32_cmp(a: u32, b: &BigInt) -> Ordering {
    if let Some(b) = b.to_i64() {
        (a as i64).cmp(&b)
    } else if b.sign() == Sign::Minus {
        Ordering::Greater
    } else {
        Ordering::Less
    }
}

/// Compare a `f64` and a `BigInt`. Non-finite float values are
/// considered lesser or greater than any integer, depending on
/// their sign.
pub fn bigint_f64_cmp(a: f64, b: &BigInt) -> Ordering {
    // uses the same ordering scheme as `f64::total_cmp`
    if a.is_finite() {
        let b = b.to_f64().unwrap(); // will never panic
        // the bigint `to_f64()` implementation uses +/-inf
        // to indicate numbers that can't fit in a float
        if b.is_finite() {
            f64_total_cmp(a, b)
        } else if b.is_sign_positive() {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    } else if a.is_sign_positive() {
        // positive NaN/inf are considered more than any finite number
        Ordering::Greater
    } else {
        // negative NaN/inf are considered less than any finite number
        Ordering::Less
    }
}

/// Convert a `BigInt` to its value mod 2^32.
pub fn bigint_to_u32_wrapping(n: &BigInt) -> u32 {
    let (sign, digits) = n.to_u32_digits();
    if sign == Sign::Minus {
        !digits[0]
    } else {
        digits[0]
    }
}

pub fn try_position<T, E>(
    items: impl Iterator<Item=T>,
    mut test: impl FnMut(T) -> Result<bool, E>
) -> Result<Option<usize>, E> {
    for (i, t) in items.enumerate() {
        if test(t)? {
            return Ok(Some(i));
        }
    }
    Ok(None)
}

/// Split an iterator of elements on a particular
/// element and return a new iterator of components.
pub fn split_iter_one<
    'a,
    T: PartialEq,
    I: Iterator<Item=T>,
    C: FromIterator<T>,
>(
    ts: &'a mut I,
    needle: &'a T
) -> impl Iterator<Item=C> + 'a {
    let mut finished = false;
    iter::from_fn(move || {
        if finished {
            None
        } else {
            let segment = iter::from_fn(|| {
                if let Some(t) = ts.next() {
                    if &t == needle {
                        None
                    } else {
                        Some(t)
                    }
                } else {
                    finished = true;
                    None
                }
            });
            Some(segment.collect())
        }
    })
}

/// An iterator that allows indexing by lazily computing
/// elements up to the index requested.
pub struct Indexable<I: Iterator> {
    cache: VecDeque<I::Item>,
    iter: I,
}

impl<I: Iterator> Indexable<I> {
    pub fn new(iter: I) -> Self {
        Self { cache: VecDeque::new(), iter }
    }

    pub fn get(&mut self, idx: usize) -> Option<&I::Item> {
        while idx >= self.cache.len() {
            self.cache.push_back(self.iter.next()?);
        }
        self.cache.get(idx)
    }
}

impl<I: Iterator> Iterator for Indexable<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.cache.pop_front().or_else(|| self.iter.next())
    }
}

/// Return each segment of `haystack`, delimited
/// by occurences of the subsequence `needle`.
pub fn split_iter_many<
    'a,
    T: PartialEq + 'a,
    I: Iterator<Item=T>,
    N: Index<usize, Output=T>,
    C: FromIterator<T>,
>(
    haystack: &'a mut I,
    needle: &'a N,
    needle_len: usize,
) -> impl Iterator<Item=C> + 'a {
    // TODO: if we're willing to specialize the interface,
    // we can reduce the complexity by using `take` instead
    // of trying to implement everything with iterators, and
    // possibly also switch to a faster algorithm
    use std::mem;
    let mut haystack = Indexable::new(haystack);
    // elements that are queued to be returned
    // in the next call to the inner closure
    let mut ret_cache = VecDeque::new();
    let mut finished = false;
    iter::from_fn(move || {
        if finished {
            None
        } else {
            let segment = iter::from_fn(|| {
                // return any queued elements
                if let Some(t) = ret_cache.pop_front() {
                    return Some(t)
                }
                // take elements from the iterator until it diverges with the needle
                for i in 0..needle_len {
                    if let Some(t) = haystack.get(i) {
                        if t != &needle[i] {
                            // we found the point of divergence; this means
                            // that the first element was definitely not part
                            // of the needle and can be queued
                            return haystack.cache.pop_front()
                        }
                    } else {
                        // the haystack is empty, so queue any remaining elements
                        // and set a flag telling the outer closure to stop
                        finished = true;
                        mem::swap(&mut ret_cache, &mut haystack.cache);
                        return ret_cache.pop_front()
                    }
                }
                // there was no point of divergence; we found an copy of the needle
                haystack.cache.clear();
                None
            });
            Some(segment.collect())
        }
    })
}
