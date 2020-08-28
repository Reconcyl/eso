use num_bigint::{BigInt, Sign};

pub fn assign_from_u8(a: &mut BigInt, b: u8) {
    a.assign_from_slice(Sign::Plus, &[b as u32]);
}
