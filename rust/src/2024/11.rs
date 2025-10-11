#![allow(clippy::match_same_arms, clippy::large_stack_arrays)]

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, parse_u64},
};

#[apply(day)]
impl Day<2024, 11> {
    pub const fn parse(input: &[u8]) -> u64 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u64 {
    solve(input, 25)
}

const fn parse2(input: &[u8]) -> u64 {
    solve(input, 75)
}

const fn solve(input: &[u8], depth: u64) -> u64 {
    let mut total = 0;

    #[apply(iter)]
    for i in split(input.trim_ascii(), b" ") {
        let i = parse_u64(i);

        total += blink(i, depth, &mut [[None; 75]; MAX_CACHED_NUM])
    }

    total
}

const MAX_CACHED_NUM: usize = 1000;

// i => d => count
type Cache = [[Option<u64>; 75]; MAX_CACHED_NUM];

#[allow(clippy::cast_possible_truncation)]
const fn blink(i: u64, d: u64, cache: &mut Cache) -> u64 {
    let Some(d) = d.checked_sub(1) else {
        return 1;
    };

    if i < MAX_CACHED_NUM as u64
        && let Some(res) = cache[i as usize][d as usize]
    {
        return res;
    }

    let res = match i {
        0 => blink(1, d, cache),
        n => {
            let width = n.ilog10() + 1;
            if width % 2 == 0 {
                let l = n / 10_u64.pow(width / 2);
                let r = n % 10_u64.pow(width / 2);

                blink(l, d, cache) + blink(r, d, cache)
            } else {
                blink(n * 2024, d, cache)
            }
        }
    };

    if i < MAX_CACHED_NUM as u64 {
        cache[i as usize][d as usize] = Some(res);
    }

    res
}

#[cfg(test)]
#[test]
fn test() {
    let input = "125 17";

    dbg!(parse(input.as_bytes()));
    dbg!(parse2(Today::INPUT.as_bytes()));
}
