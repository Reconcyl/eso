use std::cmp::Ordering;

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
