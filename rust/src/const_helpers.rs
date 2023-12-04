pub(crate) const fn slice<T>(bytes: &[T], idx_start: usize, idx_curr: usize) -> &[T] {
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

pub(crate) const fn read_until(bytes: &'static [u8], start: usize, char: u8) -> &'static [u8] {
    let mut i = start;

    while i < bytes.len() && bytes[i] != char {
        i += 1;
    }

    slice(bytes, start, i)
}

pub(crate) const fn bytes_to_array<const LEN: usize>(bz: &'static [u8]) -> [u8; LEN] {
    assert!(LEN == bz.len());

    let mut out = [0; LEN];

    iter! {
        for (i, item) in enumerate(bz) {
            out[i] = item;
        }
    }

    out
}

#[allow(clippy::cast_possible_truncation)]
pub(crate) const fn parse_int(bz: &[u8]) -> u32 {
    let mut res = 0;

    iter! {
        for (i, digit) in enumerate(bz) {
            assert!(digit.is_ascii_digit());
            res += (digit - 48) as usize * 10_usize.pow((bz.len() - i - 1) as u32);
        }
    }

    res as u32
}

pub(crate) const fn max(a: u32, b: u32) -> u32 {
    if a >= b {
        a
    } else {
        b
    }
}

#[allow(clippy::cast_possible_truncation)]
pub(crate) const fn int_to_str<const LEN: usize>(n: u32) -> [u8; LEN] {
    // would be nice, but generic_const_exprs is still incomplete
    // let bz = [0; N.ilog10()];

    assert!(LEN as u32 == n.ilog10() + 1);

    let mut bz = [0; LEN];

    iter! {
        for i in range(0, LEN) {
            bz[i] = digit_at_place(n, (LEN - i - 1) as u32) as u8 + 48;
        }
    }

    bz
}

pub(crate) const fn digit_at_place(n: u32, place: u32) -> u32 {
    (n % 10u32.pow(place + 1) - n % 10u32.pow(place)) / 10u32.pow(place)
}

pub(crate) const fn utf8(bz: &[u8]) -> &str {
    match core::str::from_utf8(bz) {
        Ok(ok) => ok,
        Err(_) => {
            panic!()
        }
    }
}

pub(crate) const fn array_concat<const LEN_1: usize, const LEN_2: usize, const OUT: usize>(
    bz1: [u8; LEN_1],
    bz2: [u8; LEN_2],
) -> [u8; OUT] {
    assert!(LEN_1 + LEN_2 == OUT);

    let mut res = [0; OUT];

    iter! {
        for (i, item) in enumerate(bz1) {
            res[i] = item;
        }
    }

    iter! {
        for (i, item) in enumerate(bz2) {
            res[i + LEN_1] = item;
        }
    }

    res
}

macro_rules! itoa {
    ($i:expr) => {{
        const A: [u8; { (($i).ilog10() + 1) as usize }] =
            $crate::const_helpers::int_to_str::<{ (($i).ilog10() + 1) as usize }>($i);
        A
    }};
}

pub(crate) use itoa;

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

macro_rules! arr {
    ($i:expr) => {{
        const ARR: [u8; { ($i).len() }] =
            $crate::const_helpers::bytes_to_array::<{ ($i).len() }>($i);
        ARR
    }};
}
pub(crate) use arr;

macro_rules! iter {
    (for $item:pat in $slice:ident
        $body:block
    ) => {
        let mut i = 0;
        while i < $slice.len() {
            let $item = $slice[i];
            $body
            i += 1;
        }
    };

    (for ($i:ident, $item:pat) in enumerate($slice:ident)
        $body:block
    ) => {
        let mut $i = 0;
        while $i < $slice.len() {
            let $item = $slice[$i];
            $body
            $i += 1;
        }
    };

    (for $i:ident in range($start:expr, $end:expr)
        $body:block
    ) => {
        let mut $i = $start;
        while $i < $end {
            $body
            $i += 1;
        }
    };
}
pub(crate) use iter;

/// Pulled from <https://www.reddit.com/r/rust/comments/lz0xsl/concatenating_arrays_at_compiletime_in_rust_151/>
macro_rules! concat_array_const {
    (
        $(const $IDENT:ident: [$T:ident; _] = $($arr:expr),*;)+
    ) => {
        $(
            const $IDENT: [$T; { 0 $( + $arr.len() )* }] = concat_array_const!(@concat
                $( [$arr ; $arr.len()] )*
            );
        )+
    };

    (@concat [$a:expr; $a_len:expr]) => {
        $a
    };

    (@concat [$a:expr; $a_len:expr] [$b:expr; $b_len:expr] $($tail:tt)*) => {
        concat_array_const!(
            @concat
            [$crate::const_helpers::array_concat::<{ $a_len }, { $b_len }, { $a_len + $b_len }>($a, $b); $a_len + $b_len]
            $($tail)*
        )
    };
}
pub(crate) use concat_array_const;

#[test]
fn concat() {
    concat_array_const! {
        const CONST: [u8; _] = [1, 2, 3], [4, 5, 6], [7, 8, 9];
    }

    assert_eq!(CONST, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
}
