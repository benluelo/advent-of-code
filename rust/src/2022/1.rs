use core::fmt::Display;

use crate::{
    const_helpers::{iter, max, parse_int, slice, split},
    Day, DaySolution, Input,
};

impl DaySolution for Day<2022, 1> {
    fn part_1() -> impl Display {
        SOLUTION_PART_1
    }

    fn part_2() -> impl Display {
        SOLUTION_PART_2
    }
}

pub(crate) const fn count_segments(bz: &'static [&'static [u8]]) -> usize {
    let mut segments = 0;

    iter! {
        for item in bz {
            if item.is_empty() {
                segments += 1;
            }
        }
    }

    segments + 1
}

const SPLIT: [&[u8]; 2252] = split!(Day::<2022, 1>::INPUT.as_bytes(), b'\n', true);

#[allow(long_running_const_eval)]
const PARSED: [u32; 251] = {
    let mut res: [u32; count_segments(&SPLIT)] = [0; count_segments(&SPLIT)];

    let mut idx_start = 0;
    let mut idx_curr = 0;

    let mut i = 0;

    while i < { count_segments(&SPLIT) } {
        while idx_curr < SPLIT.len() && !SPLIT[idx_curr].is_empty() {
            idx_curr += 1;
        }

        res[i] = parse_and_sum_calories(slice(&SPLIT, idx_start, idx_curr));

        idx_curr += 1;
        idx_start = idx_curr;
        i += 1;
    }

    res
};

const SOLUTION_PART_1: u32 = {
    let mut res = 0;

    iter! {
        for cals in PARSED {
            res = max(res, cals);
        }
    }

    res
};

const SOLUTION_PART_2: u32 = {
    let mut first = 0;
    let mut second = 0;
    let mut third = 0;

    iter! {
        for cals in PARSED {
            match (cals > first, cals > second, cals > third) {
                // new largest, shift 1 -> 2, 2 -> 3
                (true, true, true) => {
                    third = second;
                    second = first;
                    first = cals;
                },
                // greater than 2 and 3, shift 2 -> 3
                (false, true, true) => {
                    third = second;
                    second = cals;
                },
                // greater than 3
                (false, false, true) => {
                    third = cals;
                },
                (false, false, false) => {},
                _ => unreachable!()
            }
        }
    }

    first + second + third
};

#[test]
fn parse() {
    dbg!(SOLUTION_PART_1);
}

const fn parse_and_sum_calories(bz: &'static [&'static [u8]]) -> u32 {
    let mut res = 0;

    iter! {
        for arr in bz {
            res += parse_int(arr);
        }
    }

    res
}
