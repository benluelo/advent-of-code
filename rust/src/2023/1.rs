use core::fmt::Display;

use crate::{
    const_helpers::{slice, split},
    Day, DaySolution, Input,
};

impl DaySolution for Day<2023, 1> {
    fn part_1() -> impl Display {
        SOLUTION_PART_1
    }

    fn part_2() -> impl Display {
        SOLUTION_PART_2
    }
}

const SOLUTION_PART_1: u32 =
    parse(split!(Day::<2023, 1>::INPUT.as_bytes(), b'\n', true).as_slice());

const SOLUTION_PART_2: u32 =
    parse2(split!(Day::<2023, 1>::INPUT.as_bytes(), b'\n', true).as_slice());

const fn parse(bytes: &'static [&'static [u8]]) -> u32 {
    let mut res = 0;

    let mut i = 0;
    while i < bytes.len() {
        let left_digit = parse_line::<false>(bytes[i]);
        let right_digit = parse_line::<true>(bytes[i]);

        res += (left_digit * 10) + right_digit;

        i += 1;
    }

    res
}

const fn parse_line<const REVERSE: bool>(bz: &[u8]) -> u32 {
    let mut i = if REVERSE { bz.len() - 1 } else { 0 };

    loop {
        if let n @ (b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' | b'0') = bz[i] {
            return (n - 48) as _;
        }

        if if REVERSE { i == 0 } else { i >= bz.len() } {
            break;
        }
        if REVERSE {
            i -= 1;
        } else {
            i += 1;
        }
    }

    panic!()
}

const fn parse2(bytes: &'static [&'static [u8]]) -> u32 {
    let mut res = 0;

    let mut i = 0;
    while i < bytes.len() {
        let left_digit = parse_line2::<false>(bytes[i]);
        let right_digit = parse_line2::<true>(bytes[i]);

        res += (left_digit * 10) + right_digit;

        i += 1;
    }

    res
}

const fn parse_line2<const REVERSE: bool>(bz: &[u8]) -> u32 {
    let mut i = if REVERSE { bz.len() - 1 } else { 0 };

    let idx_start = i;

    loop {
        if let n @ (b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' | b'0') = bz[i] {
            return (n - 48) as _;
        }

        let slice = if REVERSE {
            slice(bz, i, idx_start + 1)
        } else {
            slice(bz, idx_start, i + 1)
        };

        // this would be a lot nicer if starts_with/ends_with were const, but alas
        if REVERSE {
            match slice {
                [b'o', b'n', b'e', ..] => return 1,
                [b't', b'w', b'o', ..] => return 2,
                [b't', b'h', b'r', b'e', b'e', ..] => return 3,
                [b'f', b'o', b'u', b'r', ..] => return 4,
                [b'f', b'i', b'v', b'e', ..] => return 5,
                [b's', b'i', b'x', ..] => return 6,
                [b's', b'e', b'v', b'e', b'n', ..] => return 7,
                [b'e', b'i', b'g', b'h', b't', ..] => return 8,
                [b'n', b'i', b'n', b'e', ..] => return 9,
                _ => {}
            }
        } else {
            match slice {
                [.., b'o', b'n', b'e'] => return 1,
                [.., b't', b'w', b'o'] => return 2,
                [.., b't', b'h', b'r', b'e', b'e'] => return 3,
                [.., b'f', b'o', b'u', b'r'] => return 4,
                [.., b'f', b'i', b'v', b'e'] => return 5,
                [.., b's', b'i', b'x'] => return 6,
                [.., b's', b'e', b'v', b'e', b'n'] => return 7,
                [.., b'e', b'i', b'g', b'h', b't'] => return 8,
                [.., b'n', b'i', b'n', b'e'] => return 9,
                _ => {}
            }
        }

        // dbg!(String::from_utf8_lossy(slice));

        // if that was the last char, break
        if if REVERSE { i == 0 } else { i >= bz.len() } {
            break;
        }

        // progress cursor
        if REVERSE {
            i -= 1;
        } else {
            i += 1;
        }
    }

    panic!()
}

#[test]
fn split_works() {
    const INPUT: &[u8] = b"hello\nworld";

    assert_eq!(split!(INPUT, b'\n', false), [b"hello", b"world"]);
}
