pub(crate) const fn slice(bytes: &[u8], idx_start: usize, idx_curr: usize) -> &[u8] {
    let first_split = &bytes.split_at(idx_start).1;
    let line = first_split.split_at(idx_curr - idx_start).0;
    line
}

pub(crate) const fn count_segments<const PAT: u8, const TRAILING: bool>(
    bz: &'static [u8],
) -> usize {
    let len = bz.len();

    let mut segments = 0;
    let mut i = 0;

    while i < len {
        if bz[i] == PAT {
            segments += 1;
        }
        i += 1;
    }

    segments + (!TRAILING as usize)
}

pub(crate) const fn split_with_len<const LEN: usize, const PAT: u8, const TRAILING: bool>(
    bytes: &'static [u8],
) -> [&'static [u8]; LEN] {
    let mut res: [&[u8]; LEN] = [b""; LEN];

    let mut idx_start = 0;
    let mut idx_curr = 0;

    let mut i = 0;

    while i < LEN {
        while idx_curr < bytes.len() && bytes[idx_curr] != PAT {
            idx_curr += 1;
        }

        res[i] = slice(bytes, idx_start, idx_curr);

        idx_curr += 1;
        idx_start = idx_curr;
        i += 1;
    }

    res
}

macro_rules! split {
    ($input:expr, $pat:expr, $trailing:expr) => {
        $crate::const_helpers::split_with_len::<
            { $crate::const_helpers::count_segments::<$pat, $trailing>($input) },
            $pat,
            $trailing,
        >($input)
    };
}
pub(crate) use split;
