use cfg_proc::apply;

use crate::{
    utils::{iter, max, parse_u32},
    day, Day,
};

#[apply(day)]
impl Day<2022, 1> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for section in split(input, b"\n\n") {
        res = max(res, parse_and_sum_calories(input));
    }

    res
}

const fn parse2(input: &[u8]) -> u32 {
    let mut first = 0;
    let mut second = 0;
    let mut third = 0;

    #[apply(iter)]
    for section in split(input, b"\n\n") {
        let cals = parse_and_sum_calories(input);

        match (cals > first, cals > second, cals > third) {
            // new largest, shift 1 -> 2, 2 -> 3
            (true, true, true) => {
                third = second;
                second = first;
                first = cals;
            }
            // greater than 2 and 3, shift 2 -> 3
            (false, true, true) => {
                third = second;
                second = cals;
            }
            // greater than 3
            (false, false, true) => {
                third = cals;
            }
            (false, false, false) => {}
            _ => unreachable!(),
        }
    }

    first + second + third
}

const fn parse_and_sum_calories(bz: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for arr in lines(bz) {
        res += parse_u32(arr);
    }

    res
}
