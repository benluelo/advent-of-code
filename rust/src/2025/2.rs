//! Part 1 can probably be made more efficient with a range check rather than
//! iterating over all of the values, and similar logic can likely be applied to
//! part 2. However, part 1 compiles in ~3 seconds, and part 2 in ~20 seconds,
//! which I deem acceptable, so I can't be bothered to optimize it any further
//! for now.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, parse_u64, split_once},
};

#[apply(day)]
impl Day<2025, 2> {
    pub const fn parse(input: &[u8]) -> u64 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u64 {
    const fn calc_id(half_magnitude: u32, half: u64) -> u64 {
        if half_magnitude == 0 {
            (half * 10) + half
        } else {
            (half * (10_u64.pow(half_magnitude))) + half
        }
    }

    let mut total = 0;

    #[apply(iter)]
    for range in split(input.trim_ascii(), b",") {
        let Some((from, to)) = split_once(range, b"-") else {
            panic!()
        };

        let from = parse_u64(from);
        let to = parse_u64(to);

        let half_magnitude_from = magnitude(from).div_ceil(2);

        #[apply(iter)]
        for half in range(from / 10_u64.pow(half_magnitude_from), u64::MAX) {
            let id = calc_id(half_magnitude_from, half);

            if id < from {
                continue;
            }

            if id > to {
                break;
            }

            if magnitude(id).is_multiple_of(2) {
                total += id;
            }
        }
    }

    total
}

const fn parse2(input: &[u8]) -> u64 {
    let mut total = 0;

    #[apply(iter)]
    for range in split(input.trim_ascii(), b",") {
        let Some((from, to)) = split_once(range, b"-") else {
            panic!()
        };

        let from = parse_u64(from);
        let to = parse_u64(to);

        #[apply(iter)]
        for id in range(from, to + 1) {
            // dbg!(id);
            if is_invalid_id(id) {
                total += id;
            }
        }
    }

    total
}

const fn magnitude(n: u64) -> u32 {
    let log10 = n.ilog10();
    if n == 10_u64.pow(log10) {
        log10
    } else {
        log10 + 1
    }
}

const fn is_invalid_id(id: u64) -> bool {
    let magnitude = magnitude(id + 1);

    #[allow(
        clippy::unreadable_literal,
        clippy::zero_prefixed_literal,
        clippy::large_digit_groups
    )]
    match magnitude {
        // 1 digit: cannot have repeated digits by definition
        1 => false,
        // 2 digits: only invalid if both digits are equal
        2 => id.is_multiple_of(11),
        // 3 digits: only invalid if all 3 digits are equal
        3 => id.is_multiple_of(111),
        // 4 digits: 2 digits twice
        4 => id / 1_00 == id % 1_00,
        // 5 digits: only invalid if all 5 digits are equal
        5 => id.is_multiple_of(11111),
        // 6 digits: either 2 digits 3 times, or 3 digits twice
        6 => id.is_multiple_of(01_01_01) || id.is_multiple_of(001_001),
        // 7 digits: only invalid if all 7 digits are equal
        7 => id.is_multiple_of(1_111_111),
        // 8 digits: either 2 digits 4 times, or 4 digits twice
        8 => id.is_multiple_of(01_01_01_01) || id.is_multiple_of(0001_0001),
        // 9 digits: 3 digits 3 times
        9 => id.is_multiple_of(001_001_001),
        // 10 digits: either 2 digits 5 times, or 5 digits twice
        10 => id.is_multiple_of(01_01_01_01_01) || id.is_multiple_of(00001_00001),
        // 11 digits: only invalid if all 11 digits are equal
        11 => id.is_multiple_of(11111111111),
        // thank god my input doesn't have numbers with 12 digits
        _ => unimplemented!(),
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

    // dbg!(parse2(Today::INPUT.as_bytes()));
    dbg!(parse2(input.as_bytes()));
}

#[test]
fn test_is_invalid_id() {
    assert!(is_invalid_id(2_121_212_121));
    assert!(!is_invalid_id(1_000_000));
}
