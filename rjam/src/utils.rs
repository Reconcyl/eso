use std::collections::VecDeque;
use std::cmp::Ordering;
use std::iter::{self, FromIterator};
use std::ops::Index;

pub fn get_wrapping<T>(vals: &[T], idx: i64) -> Option<&T> {
    idx.checked_rem_euclid(vals.len() as i64)
        .map(|i| &vals[i as usize])
}

pub fn f64_total_cmp(a: f64, b: f64) -> Ordering {
    a.partial_cmp(&b).unwrap_or_else(||
        if !a.is_nan() {
            Ordering::Less
        } else if !b.is_nan() {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    )
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
