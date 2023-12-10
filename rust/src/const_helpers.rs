pub const fn slice<T>(bytes: &[T], idx_start: usize, idx_curr: usize) -> &[T] {
    let first_split = &bytes.split_at(idx_start).1;
    let line = first_split.split_at(idx_curr - idx_start).0;
    line
}

pub const fn slice_mut<T>(bytes: &mut [T], idx_start: usize, idx_curr: usize) -> &mut [T] {
    let first_split = bytes.split_at_mut(idx_start).1;
    let line = first_split.split_at_mut(idx_curr - idx_start).0;
    line
}

pub const fn count_segments<const PAT: u8, const TRAILING: bool>(bz: &'static [u8]) -> usize {
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

pub const fn split_with_len<const LEN: usize, const PAT: u8, const TRAILING: bool>(
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

pub const fn read_until<'bz>(bytes: &'bz [u8], start: usize, separator: &[u8]) -> &'bz [u8] {
    let sep_len = separator.len();
    let bytes_len = bytes.len();

    assert!(sep_len <= bytes_len, "separator is longer than input");
    assert!(sep_len > 0, "separator must be > 0");

    let mut i = start;

    while i < bytes_len {
        if sep_len - 1 <= i - start {
            let mut j = sep_len;
            while j != 0 {
                if bytes[i - (sep_len - j)] != separator[j - 1] {
                    break;
                }
                j -= 1;
            }

            if j == 0 {
                // `i` can be 1 less than sep_len here, so add to it before subtracting
                return slice(bytes, start, i + 1 - sep_len);
            }
        }

        i += 1;
    }

    // separator not found, read until end
    slice(bytes, start, i)
}

// pub const fn eat<'bz>(bytes: &'bz [u8], start: usize, separator:
// &[u8]) -> &'bz [u8] {     let sep_len = separator.len();
//     let bytes_len = bytes.len();

//     assert!(sep_len <= bytes_len, "separator is longer than input");
//     assert!(sep_len > 0, "separator must be > 0");

//     let mut i = start;

//     while i < bytes_len {
//         if sep_len - 1 <= i - start {
//             let mut j = sep_len;
//             while j != 0 {
//                 if bytes[i - (sep_len - j)] != separator[j - 1] {
//                     break;
//                 }
//                 j -= 1;
//             }

//             if j == 0 {
//                 // `i` can be 1 less than sep_len here, so add to it before
// subtracting                 return slice(bytes, start, i + 1 - sep_len);
//             }
//         }

//         i += 1;
//     }

//     // separator not found, read until end
//     slice(bytes, start, i)
// }

#[test]
fn test_read_until() {
    let bz = b"hello,ABworldAB";
    assert_eq!(b"hello,", read_until(bz, 0, b"AB"));
    assert_eq!(
        b"world",
        read_until(bz, b"hello,".len() + b"AB".len(), b"AB")
    );

    let bz = b"no separators here!";
    assert_eq!(bz, read_until(bz, 0, b"foo"));

    let bz = b"only separator";
    assert_eq!(b"", read_until(bz, 0, bz));

    let bz = b"trailing separatorFOO";
    assert_eq!(b"trailing separator", read_until(bz, 0, b"FOO"));

    let bz = b"FOOleading separator";
    assert_eq!(b"", read_until(bz, 0, b"FOO"));
}

pub const fn bytes_to_array<const LEN: usize>(bz: &'static [u8]) -> [u8; LEN] {
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
pub const fn parse_int(bz: &[u8]) -> u32 {
    let mut res = 0;

    iter! {
        for (i, digit) in enumerate(bz) {
            assert!(digit.is_ascii_digit());
            res += (digit - 48) as usize * 10_usize.pow((bz.len() - i - 1) as u32);
        }
    }

    res as u32
}

pub const fn max(a: u32, b: u32) -> u32 {
    if a >= b {
        a
    } else {
        b
    }
}

pub const fn min(a: usize, b: usize) -> usize {
    if a <= b {
        a
    } else {
        b
    }
}

#[allow(clippy::cast_possible_truncation)]
pub const fn int_to_str<const LEN: usize>(n: u128) -> [u8; LEN] {
    // would be nice, but generic_const_exprs is still incomplete
    // let bz = [0; N.ilog10()];

    let mut bz = [0; LEN];

    if n == 0 {
        assert!(LEN as u32 == 1);
        bz[0] = b'0';
        return bz;
    }

    assert!(LEN as u32 == n.ilog10() + 1);

    let mut n = n;

    iter! {
        for i in range(0, LEN) {
            let digit = n % 10;

            assert!(digit <= 9);

            bz[LEN - i - 1] = digit as u8 + 48;

            n /= 10;
        }
    }

    bz
}

#[test]
fn test_int_to_str() {
    assert_eq!(utf8(&itoa!(0)), "0");
    assert_eq!(utf8(&itoa!(1)), "1");
    assert_eq!(utf8(&itoa!(12345)), "12345");
    assert_eq!(utf8(&itoa!(u128::MAX)), u128::MAX.to_string());
}

pub const fn utf8(bz: &[u8]) -> &str {
    match core::str::from_utf8(bz) {
        Ok(ok) => ok,
        Err(_) => {
            panic!()
        }
    }
}

pub const fn array_concat<const LEN_1: usize, const LEN_2: usize, const OUT: usize>(
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
        const I: u128 = $i as u128;
        const LEN: usize = if I == 0 { 1 } else { (I.ilog10() + 1) as usize };
        const A: [u8; LEN] = $crate::const_helpers::int_to_str::<LEN>(I);
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

#[test]
fn split_works() {
    const INPUT: &[u8] = b"hello\nworld";

    assert_eq!(split!(INPUT, b'\n', false), [b"hello", b"world"]);
}

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

    (for $line:ident in lines($slice:ident)
        $body:block
    ) => {
        let mut i = 0;
        while i < $slice.len() {
            let $line = $crate::const_helpers::read_until($slice, i, b"\n");
            $body

            i += $line.len() + 1;
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
