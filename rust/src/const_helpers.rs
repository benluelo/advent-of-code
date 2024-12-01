use cfg_proc::apply;

use crate::const_helpers::array::ArrayVec;

pub mod array;

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

#[must_use]
pub const fn count_segments<const PAT: u8, const TRAILING: bool>(bz: &[u8]) -> usize {
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

#[must_use]
pub const fn split_with_len<const LEN: usize, const PAT: u8, const TRAILING: bool>(
    bytes: &[u8],
) -> [&[u8]; LEN] {
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

#[must_use]
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

#[must_use]
pub const fn bytes_to_array<const LEN: usize>(bz: &[u8]) -> [u8; LEN] {
    assert!(LEN == bz.len());

    let mut out = [0; LEN];

    #[apply(iter)]
    for (i, item) in enumerate(bz) {
        out[i] = item;
    }

    out
}

#[allow(clippy::cast_possible_truncation)]
#[must_use]
pub const fn parse_int(bz: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for (i, digit) in enumerate(bz) {
        assert!(digit.is_ascii_digit());
        res += (digit - 48) as usize * 10_usize.pow((bz.len() - i - 1) as u32);
    }

    res as u32
}

#[must_use]
#[allow(clippy::cast_possible_truncation)]
pub const fn parse_sint(bz: &[u8]) -> i32 {
    let mut res = 0;

    let is_negative = bz[0] == b'-';
    let bz = if is_negative { bz.split_at(1).1 } else { bz };

    #[apply(iter)]
    for (i, digit) in enumerate(bz) {
        assert!(digit.is_ascii_digit());
        res += (digit - 48) as i32
            * (if is_negative { -1 } else { 1 })
            * 10_i32.pow((bz.len() - i - 1) as u32);
    }

    res
}

#[test]
fn parse_sint_works() {
    assert_eq!(parse_sint(b"0"), 0);
    assert_eq!(parse_sint(b"1"), 1);
    assert_eq!(parse_sint(b"12345"), 12345);
    assert_eq!(parse_sint(b"-1"), -1);
    assert_eq!(parse_sint(b"-1234"), -1234);
    assert_eq!(parse_sint(i32::MAX.to_string().as_bytes()), i32::MAX);
    assert_eq!(parse_sint(i32::MIN.to_string().as_bytes()), i32::MIN);
}

#[must_use]
pub const fn max(a: u32, b: u32) -> u32 {
    if a >= b {
        a
    } else {
        b
    }
}

#[must_use]
pub const fn min_usize(a: usize, b: usize) -> usize {
    if a <= b {
        a
    } else {
        b
    }
}

#[must_use]
pub const fn min_u64(a: u64, b: u64) -> u64 {
    if a <= b {
        a
    } else {
        b
    }
}

macro_rules! min {
    ($a:expr, $b:expr) => {
        if $a <= $b {
            $a
        } else {
            $b
        }
    };
}
pub(crate) use min;

#[test]
fn test_itoa() {
    assert_eq!(itoa!(0).as_str(), "0");
    assert_eq!(itoa!(1).as_str(), "1");
    assert_eq!(itoa!(12345).as_str(), "12345");
    assert_eq!(itoa!(u128::MAX).as_str(), u128::MAX.to_string());
}

#[must_use]
pub const fn utf8(bz: &[u8]) -> &str {
    match core::str::from_utf8(bz) {
        Ok(ok) => ok,
        Err(_) => {
            panic!()
        }
    }
}

#[must_use]
pub const fn array_concat<const LEN_1: usize, const LEN_2: usize, const OUT: usize>(
    bz1: [u8; LEN_1],
    bz2: [u8; LEN_2],
) -> [u8; OUT] {
    assert!(LEN_1 + LEN_2 == OUT);

    let mut res = [0; OUT];

    #[apply(iter)]
    for (i, item) in enumerate(bz1) {
        res[i] = item;
    }

    #[apply(iter)]
    for (i, item) in enumerate(bz2) {
        res[i + LEN_1] = item;
    }

    res
}

pub struct Num<T>(pub T);

macro_rules! impl_num {
    ($($T:ty)+) => {
        $(
            impl Num<$T> {
                #[allow(unused_comparisons)]
                pub const STR_LEN: usize = (<$T>::MAX.ilog10() + 1 + ((<$T>::MIN < 0) as u32)) as usize;

                // pub const fn max_value(&self) -> $T {
                //     <$T>::MAX
                // }

                #[must_use]
                #[allow(clippy::cast_possible_truncation)]
                pub const fn to_str(&self) -> ArrayVec<u8, { Self::STR_LEN }>
                {
                    let mut n = self.0;

                    let mut arr = ArrayVec::new();

                    if n == 0 {
                        arr.append(b'0');
                    } else {
                        while n != 0 {
                            let digit = n % 10;

                            assert!(digit <= 9);

                            #[allow(clippy::cast_sign_loss)]
                            arr.push(digit as u8 + 48);

                            n /= 10;
                        }
                    }

                    arr
                }
            }
        )+
    };
}
impl_num!(u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize);

macro_rules! itoa {
    ($i:expr) => {
        $crate::const_helpers::Num($i as u128).to_str()
    };
}
pub(crate) use itoa;

macro_rules! arr {
    ($i:expr) => {{
        const ARR: [u8; { ($i).len() }] =
            $crate::const_helpers::bytes_to_array::<{ ($i).len() }>($i);
        ARR
    }};
}
pub(crate) use arr;

macro_rules! iter {
    ($($label:lifetime:)? for $item:pat in $slice:ident
        $body:block
    ) => {
    #[allow(clippy::semicolon_if_nothing_returned)]
    {
        let mut __i = 0;
        $($label:)? while __i < $slice.len() {
            let $item = $slice[__i];
            __i += 1;
            $body;
        }
    }};

    ($($label:lifetime:)? for ($i:ident, $item:pat) in enumerate($slice:expr)
        $body:block
    ) => {{
        let __slice = $slice;
        let mut __i = 0;
        $($label:)? while __i < __slice.len() {
            #[allow(clippy::toplevel_ref_arg)]
            let $item = __slice[__i];
            __i += 1;
            let $i = __i - 1;
            $body;
        }
    }};

    ($($label:lifetime:)? for $i:pat in range($start:expr, $end:expr)
        $body:block
    ) => {{
        iter! {
            $($label:)? for $i in range($start, $end, 1)
                $body

        }
    }};

    ($($label:lifetime:)? for $i:pat in range($start:expr, $end:expr, $step:expr)
        $body:block
    ) => {{
        let __step = $step;
        let mut __i = $start;
        let __end = $end;
        $($label:)? while __i < __end {
            __i += __step;
            let $i = __i - __step;
            $body;
        }
    }};

    ($($label:lifetime:)? for $line:ident in lines($slice:ident)
        $body:block
    ) => {
        iter! {
            for $line in split($slice, b"\n") $body
        }
    };

    ($($label:lifetime:)? for $segment:ident in split($slice:ident, $delimiter:expr)
        $body:block
    ) => {{
        let mut i = 0;
        let __delimiter = $delimiter;
        $($label:)? while i < $slice.len() {
            let $segment = $crate::const_helpers::read_until($slice, i, __delimiter);
            i += $segment.len() + __delimiter.len();
            $body;
        }
    }};
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

#[must_use]
pub const fn strip_prefix<'a>(input: &'a [u8], prefix: &[u8]) -> &'a [u8] {
    assert!(
        input.len() >= prefix.len(),
        "prefix cannot be longer than input"
    );

    #[apply(iter)]
    for (i, c) in enumerate(prefix) {
        assert!(input[i] == c, "input not prefixed by prefix");
    }

    slice(input, prefix.len(), input.len())
}

#[test]
fn strip_prefix_works() {
    assert_eq!(strip_prefix(b"abcde", b"abc"), b"de");
    assert_eq!(strip_prefix(b"abcde", b"abcde"), b"");
    assert_eq!(strip_prefix(b"abcde", b""), b"abcde");
}

#[test]
#[should_panic = "prefix cannot be longer than input"]
fn strip_prefix_prefix_too_long() {
    let _ = strip_prefix(b"abc", b"abcde");
}

#[test]
#[should_panic = "input not prefixed by prefix"]
fn strip_prefix_not_prefixed() {
    let _ = strip_prefix(b"abcde", b"edcba");
}

macro_rules! cmp {
    ($l:expr, $r:expr) => {{
        let l = $l;
        let r = $r;
        if l < r {
            ::core::cmp::Ordering::Less
        } else if l > r {
            ::core::cmp::Ordering::Greater
        } else {
            ::core::cmp::Ordering::Equal
        }
    }};
}
pub(crate) use cmp;

#[must_use]
pub const fn slice_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() == b.len() {
        #[apply(iter)]
        for i in range(0, a.len()) {
            if a[i] != b[i] {
                return false;
            }
        }

        true
    } else {
        false
    }
}

// macro_rules! set_bit {
//     ($number:expr, $n:expr) => {
//         $number | (1 << $n)
//     };
// }

// macro_rules! set_bit_to {
//     ($number:expr, $n:expr, ($ty:ty)$x:expr) => {{
//         let _: bool = $x;
//         $number & !(1 << $n) | ($x as $ty << $n)
//     }};
// }

// macro_rules! clear_bit {
//     ($number:expr, $n:expr) => {
//         $number & !(1 << $n)
//     };
// }

// macro_rules! toggle_bit {
//     ($number:expr, $n:expr) => {
//         $number ^ (1 << $n)
//     };
// }

// macro_rules! check_bit {
//     ($number:expr, $n:expr) => {
//         (($number >> $n) & 1) == 1
//     };
// }

#[must_use]
pub const fn line_len(line: &[u8]) -> usize {
    read_until(line, 0, b"\n").len() + 1
}

macro_rules! option_try {
    ($expr:expr) => {
        match $expr {
            Some(some) => some,
            None => return None,
        }
    };
}
pub(crate) use option_try;
