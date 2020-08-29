use num_bigint::{BigInt, Sign};

pub fn assign_from_usize(a: &mut BigInt, b: usize) {
    let b = b as u64;
    let high = (b >> 32) as u32;
    let low = b as u32;
    a.assign_from_slice(Sign::Plus, &[low, high]);
}

pub enum Option2<T> {
    One(T),
    Two(T, T),
}