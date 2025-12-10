//! This takes quite a while to compile (nearly a minute on my M5 macbook), with
//! the majority of that being spent in re-parsing the ids on every iteration.
//! This could be optimized by parsing all of the ids into a list first, but
//! that goes against the spirit of these solutions, so I'm leaving it as it.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, parse_u64, read_until, split_once},
};

#[apply(day)]
impl Day<2025, 5> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u32 {
    let (ranges, ids) = split_once(input, b"\n\n").unwrap();

    let mut total_fresh = 0;

    #[apply(iter)]
    'outer: for id in lines(ids) {
        let id = parse_u64(id);

        #[apply(iter)]
        for range in lines(ranges) {
            let (min, max) = split_once(range, b"-").unwrap();

            if id >= parse_u64(min) && id <= parse_u64(max) {
                total_fresh += 1;
                continue 'outer;
            }
        }
    }

    total_fresh
}

const fn parse2(input: &[u8]) -> u64 {
    let (ranges, _) = split_once(input, b"\n\n").unwrap();

    let mut total_fresh = 0;

    let mut i = 0;
    while i < ranges.len() {
        let curr_range = read_until(ranges, i, b"\n");
        i += curr_range.len() + b"\n".len();

        let (curr_min, curr_max) = split_once(curr_range, b"-").unwrap();

        let curr_max = parse_u64(curr_max);
        let curr_min = parse_u64(curr_min);

        check_range(0, ranges, &mut total_fresh, i, curr_min, curr_max);
    }

    total_fresh
}

const fn check_range(
    mut i: usize,
    ranges: &[u8],
    total_fresh: &mut u64,
    curr_range_idx: usize,
    mut curr_min: u64,
    mut curr_max: u64,
) {
    if curr_max < curr_min {
        return;
    }

    'block: {
        while i < ranges.len() {
            let range = read_until(ranges, i, b"\n");
            i += range.len() + b"\n".len();

            if i == curr_range_idx {
                break;
            }

            let (min, max) = split_once(range, b"-").unwrap();

            let max = parse_u64(max);
            let min = parse_u64(min);

            // curr: |-----|
            // this:     |-----|
            // now:  |--|
            if curr_min < min && curr_max < max && curr_max >= min {
                curr_max = min - 1;
            }

            // curr:     |-----|
            // this: |-----|
            // now:         |--|
            if curr_min > min && curr_max > max && curr_min <= max {
                curr_min = max + 1;
            }

            // curr:    |-----|
            // this: |-----------|
            // now:  <empty>
            if curr_min >= min && curr_max <= max {
                break 'block;
            }

            // curr: |-----------|
            // this:     |---|
            // now:  |--|     |--|
            if curr_min <= min && curr_max >= max {
                // recurse for each side
                check_range(i, ranges, total_fresh, curr_range_idx, curr_min, min - 1);
                check_range(i, ranges, total_fresh, curr_range_idx, max + 1, curr_max);
                break 'block;
            }
        }

        if curr_max >= curr_min {
            *total_fresh += (curr_max - curr_min) + 1;
        }
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "3-5
10-14
16-20
12-18

1
5
8
11
17
32
";

    assert_eq!(parse(input.as_bytes()), 3);
    assert_eq!(parse2(input.as_bytes()), 14);

    assert_eq!(parse(Today::INPUT.as_bytes()), 517);
    assert_eq!(parse2(Today::INPUT.as_bytes()), 336_173_027_056_994);
}
