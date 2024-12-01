//! NOTE: This solution does not work with inputs > 1000 lines. For larger
//! inputs, increase [`LEN`].

use cfg_proc::apply;

use crate::{
    const_helpers::{iter, parse_int, read_until, slice},
    day, Day,
};

#[apply(day)]
impl Day<2024, 1> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

const LEN: usize = 1000;

const fn parse(input: &[u8]) -> u32 {
    let (mut left, mut right) = parse_lists::<LEN>(input);

    sort_array(&mut left);
    sort_array(&mut right);

    let mut total = 0;

    #[apply(iter)]
    for idx in range(0, LEN) {
        total += left[idx].abs_diff(right[idx]);
    }

    total
}

const fn parse2(input: &[u8]) -> u32 {
    let (left, mut right) = parse_lists::<LEN>(input);

    // sorting the array improves the performance by ~16 seconds (~50%)
    sort_array(&mut right);

    let mut total = 0;

    #[apply(iter)]
    for elem in left {
        let mut occurrences: u32 = 0;

        #[apply(iter)]
        for item in right {
            if item > elem {
                break;
            }

            if item == elem {
                occurrences += 1;
            }
        }

        total += occurrences * elem;
    }

    total
}

#[allow(clippy::cast_possible_wrap)]
const fn parse_lists<const N: usize>(input: &[u8]) -> ([u32; N], [u32; N]) {
    let mut left = [0_u32; N];
    let mut right = [0_u32; N];

    let mut line_num = 0;

    #[apply(iter)]
    for line in lines(input) {
        let l = read_until(line, 0, b"   ");
        let r = slice(line, l.len() + 3, line.len());

        let l = parse_int(l) as u32;
        let r = parse_int(r) as u32;

        left[line_num] = l;
        right[line_num] = r;

        line_num += 1;
    }
    (left, right)
}

const fn sort_array<const N: usize>(arr: &mut [u32; N]) {
    let mut swapped = true;

    while swapped {
        swapped = false;
        #[apply(iter)]
        for i in range(0, arr.len() - 1) {
            if arr[i] > arr[i + 1] {
                arr.swap(i, i + 1);
                swapped = true;
            }
        }
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "3   4
4   3
2   5
1   3
3   9
3   3";

    dbg!(parse2(input.as_bytes()));
}
