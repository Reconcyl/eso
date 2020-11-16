pub fn get_wrapping<T>(vals: &[T], idx: i64) -> Option<&T> {
    idx.checked_rem_euclid(vals.len() as i64)
        .map(|i| &vals[i as usize])
}
