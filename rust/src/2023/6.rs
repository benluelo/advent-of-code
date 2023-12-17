use crate::{
    const_helpers::{iter, itoa, parse_int, read_until, slice, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 6> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
}

const SOLUTION_PART_1: u128 = parse(Day::<2023, 6>::INPUT.as_bytes());
const SOLUTION_PART_2: u128 = parse2(Day::<2023, 6>::INPUT.as_bytes());

#[test]
fn parse_test() {
    let input = "\
Time:      7  15   30
Distance:  9  40  200
";

    dbg!(parse2(Day::<2023, 6>::INPUT.as_bytes()));
}

const fn parse(input: &[u8]) -> u128 {
    let times = read_until(input, 0, b"\n").split_at(10).1;
    let distances = read_until(input, times.len() + 11, b"\n").split_at(10).1;

    let mut res = 1;

    let mut idx = 0;
    while idx < times.len() {
        let remaining_times = times.split_at(idx).1;
        let remaining_times_trimmed = remaining_times.trim_ascii_start();
        let raw_time = read_until(remaining_times_trimmed, 0, b" ");
        let padded_time_len =
            raw_time.len() + (remaining_times.len() - remaining_times_trimmed.len());
        let slice = slice(distances, idx, idx + padded_time_len);
        let distance = slice.trim_ascii_start();
        idx += padded_time_len;
        let time = parse_int(raw_time);
        let distance = parse_int(distance);

        let (min, max) = roots(time as u128, distance as u128);

        res *= (max - min) + 1;
    }
    res
}

const fn parse2(input: &[u8]) -> u128 {
    let times = read_until(input, 0, b"\n").split_at(10).1;
    let distances = read_until(input, times.len() + 11, b"\n").split_at(10).1;

    let time = parse_int_with_spaces(times);
    let distance = parse_int_with_spaces(distances);
    let (min, max) = roots(time as u128, distance as u128);

    (max - min) + 1
}

const fn roots(t: u128, d: u128) -> (u128, u128) {
    let t = t as i128;
    let d = d as i128;

    let root = ((t * t) - (4 * d)).isqrt();
    let lower = (root - t).div_ceil(-2) as u128;
    let upper = (-root - t).div_floor(-2) as u128;

    const fn y(t: u128, x: u128) -> u128 {
        (t * x) - (x * x)
    }

    (
        lower + (y(t as u128, lower) == d as u128) as u128,
        upper - (y(t as u128, upper) == d as u128) as u128,
    )
}

#[test]
fn roots_works() {
    assert_eq!(roots(7, 9), (2, 5));
    assert_eq!(roots(30, 200), (11, 19));
}

pub const fn parse_int_with_spaces(bz: &[u8]) -> u128 {
    let mut res = 0;

    iter! {
        for digit in bz {
            if digit != b' ' {
                assert!(digit.is_ascii_digit());

                res *= 10;
                res += (digit - 48) as u128;
            }
        }
    }

    res
}

#[test]
fn parse_int_with_spaces_works() {
    assert_eq!(parse_int_with_spaces(b"1 2 3"), 123);
}
