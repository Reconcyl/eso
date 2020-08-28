use num_bigint::{BigInt, Sign};

pub fn assign_from_u8(a: &mut BigInt, b: u8) {
    a.assign_from_slice(Sign::Plus, &[b as u32]);
}

pub fn assign_from_usize(a: &mut BigInt, b: usize) {
    let b = b as u64;
    let high = (b >> 32) as u32;
    let low = b as u32;
    a.assign_from_slice(Sign::Plus, &[low, high]);
}

pub fn get_two_mut<T>(vals: &mut [T], idx_a: usize, idx_b: usize)
    -> Option<(&mut T, &mut T)>
{
    if idx_a == idx_b { return None; }
    let ref_a = vals.get_mut(idx_a)? as *mut T;
    let ref_b = vals.get_mut(idx_b)?;

    unsafe {
        Some((ref_a.as_mut()?, ref_b))
    }
}
