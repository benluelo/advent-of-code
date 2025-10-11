#![allow(clippy::large_stack_arrays)]

use core::fmt::Write;

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, read_until, slice},
};

#[apply(day)]
impl Day<2023, 12> {
    pub const fn parse(input: &[u8]) -> u128 {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> u128 {
        parse2(input)
    }
}

// who needs a heap
const MEMOIZATION_LEN: usize = 15000;

type Memoization = [Option<u128>; MEMOIZATION_LEN];

#[test]
#[cfg(test)]
fn parse_test() {
    //     let input = b"\
    // ???.### 1,1,3
    // .??..??...?##. 1,1,3
    // ?#?#?#?#?#?#?#? 1,3,1,6
    // ????.#...#... 4,1,1
    // ????.######..#####. 1,6,5
    // ?###???????? 3,2,1
    // ";

    let mut nums = std::collections::HashSet::new();
    for i in 0..=55 {
        for j in 0..=100 {
            assert!(nums.insert(pair(i, j)));
        }
    }

    dbg!(nums.iter().max());

    let input = Day::<2023, 12>::INPUT.as_bytes();

    dbg!(parse_generic::<5>(input));
}

const fn parse(input: &[u8]) -> u128 {
    parse_generic::<1>(input)
}

const fn parse2(input: &[u8]) -> u128 {
    parse_generic::<5>(input)
}

const fn parse_generic<const UNFOLDING_FACTOR: usize>(input: &[u8]) -> u128 {
    let mut res = 0;

    #[apply(iter)]
    for line in lines(input) {
        res += parse_line::<UNFOLDING_FACTOR>(line);
    }

    res
}

const fn parse_line<const UNFOLDING_FACTOR: usize>(line: &[u8]) -> u128 {
    let space_idx = read_until(line, 0, b" ").len();
    let (springs, groups) = line.split_at(space_idx);
    let groups = groups.trim_ascii_start();

    let mut memoization: Memoization = [None; MEMOIZATION_LEN];

    let mut springs_inner = [[b"?".as_slice(); UNFOLDING_FACTOR]; 2];
    let mut groups_inner = [[b",".as_slice(); UNFOLDING_FACTOR]; 2];

    #[apply(iter)]
    for i in range(0, UNFOLDING_FACTOR) {
        let bytes_idx = i * 2;

        springs_inner[bytes_idx.div_floor(UNFOLDING_FACTOR)][bytes_idx % UNFOLDING_FACTOR] =
            springs;
        groups_inner[bytes_idx.div_floor(UNFOLDING_FACTOR)][bytes_idx % UNFOLDING_FACTOR] = groups;
    }

    let springs_: &[&[u8]] = springs_inner.as_flattened();
    let springs = MultiSlice::new(slice(springs_, 0, springs_.len() - 1));

    let groups_: &[&[u8]] = groups_inner.as_flattened();
    let groups = MultiSlice::new(slice(groups_, 0, groups_.len() - 1));

    calculate_solutions(springs, groups, &mut memoization)
}

const fn calculate_solutions(
    mut strings: MultiSlice,
    groups: MultiSlice,
    memoization: &mut Memoization,
) -> u128 {
    if let Some(result) = memoization[pair(strings.cursor, groups.cursor)] {
        return result;
    }

    if strings.is_empty() {
        return groups.is_empty() as u128;
    }

    match strings.get(0) {
        b'.' => {
            strings.progress_cursor(1);
            calculate_solutions(strings, groups, memoization)
        }
        b'#' => calculate_hash_solutions(strings, groups, memoization),
        b'?' => {
            let hash_solutions =
                calculate_hash_solutions(strings.clone(), groups.clone(), memoization);
            strings.progress_cursor(1);
            let solutions = calculate_solutions(strings, groups, memoization);

            solutions + hash_solutions
        }
        _ => panic!(">.> WHAT DID YOU DO?"),
    }
}

const fn calculate_hash_solutions(
    mut strings: MultiSlice,
    mut groups: MultiSlice,
    memoization: &mut Memoization,
) -> u128 {
    if groups.is_empty() {
        return 0;
    }

    let x = {
        let mut res = 0;

        let mut i = 0;
        loop {
            let b = match groups.try_get(i) {
                None => {
                    groups.progress_cursor(i);
                    break;
                }
                Some(b',') => {
                    groups.progress_cursor(i + 1);
                    break;
                }
                Some(b) => b,
            };

            assert!(b.is_ascii_digit());
            res *= 10;
            res += (b - 48) as usize;

            i += 1;
        }

        res
    };

    // if there aren't enough characters left, not a match
    if strings.len() < x {
        return 0;
    }

    // if any of the next `x` characters are a `.`, group is broken; not a match
    #[apply(iter)]
    for i in range(0, x) {
        if strings.get(i) == b'.' {
            return 0;
        }
    }

    // if the springs and groups are both finished, it's a match
    if strings.len() == x {
        return groups.is_empty() as u128;
    }

    // if the next character after the group is a broken spring, not a match
    if strings.get(x) == b'#' {
        return 0;
    }

    strings.progress_cursor(x + 1);

    let pairing = pair(strings.cursor, groups.cursor);

    let res = calculate_solutions(strings, groups, memoization);

    memoization[pairing] = Some(res);

    res
}

struct MultiSlice<'a> {
    slice: &'a [&'a [u8]],
    len: usize,
    cursor: usize,
}

impl core::fmt::Debug for MultiSlice<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for i in 0..self.len() {
            f.write_char(self.get(i) as char)?;
        }

        Ok(())
    }
}

impl<'a> MultiSlice<'a> {
    const fn new(slice: &'a [&'a [u8]]) -> Self {
        let mut len = 0;
        #[apply(iter)]
        for s in iter(slice) {
            len += s.len();
        }

        Self {
            slice,
            len,
            cursor: 0,
        }
    }

    const fn len(&self) -> usize {
        self.len - self.cursor
    }

    const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    const fn try_get(&self, i: usize) -> Option<u8> {
        if i >= self.len() {
            None
        } else {
            let mut i = i + self.cursor;

            let slice = self.slice;

            #[apply(iter)]
            for s in iter(slice) {
                if i >= s.len() {
                    i -= s.len();
                } else {
                    return Some(s[i]);
                }
            }

            panic!("???")
        }
    }

    // fn any_eq(&self, b: u8) -> bool {
    //     let mut i = 0;
    //     let slices = self.slice;

    //     iter! {
    //         for slice in slices {
    //             iter! {
    //                 for c in slice {
    //                     if i >= self.cursor && c == b {
    //                         return true;
    //                     }

    //                     i += 1;
    //                 }
    //             }
    //         }
    //     }

    //     false
    // }

    const fn get(&self, i: usize) -> u8 {
        let Some(b) = self.try_get(i) else {
            panic!("out of bounds")
        };
        b
    }

    const fn progress_cursor(&mut self, i: usize) {
        self.cursor += i;
        debug_assert!(self.cursor <= self.len);
    }

    // who needs traits
    const fn clone(&self) -> Self {
        Self {
            slice: self.slice,
            len: self.len,
            cursor: self.cursor,
        }
    }
}

const fn pair(x: usize, y: usize) -> usize {
    (((x + y) * (x + y + 1)) / 2) + y
}
