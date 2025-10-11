//! NOTE: This solution does not work with inputs > 1000 lines. For larger
//! inputs, increase [`LEN`].
//!
//! A possible non-constrained solution would do a full scan and re-parse of the
//! entire raw input for every line, which I really don't feel like doing.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, parse_u32, read_until, slice, slice_mut},
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

    quicksort(&mut left);
    quicksort(&mut right);

    let mut total = 0;

    #[apply(iter)]
    for idx in range(0, LEN) {
        total += left[idx].abs_diff(right[idx]);
    }

    total
}

const fn parse2(input: &[u8]) -> u32 {
    let (left, mut right) = parse_lists::<LEN>(input);

    // sorting the array improves the performance by ~2 seconds (~28%)
    quicksort(&mut right);

    let mut total = 0;

    // if the left column contained duplicates, it may be advantageous to sort the
    // left array and only calculate the occurrences once for each number (i.e. if 3
    // existed twice in the list and it was sorted, then the occurrences would
    // already be known when we hit the second 3). however, my input does not
    // contain any duplicates, so I did not include this optimization.
    #[apply(iter)]
    for elem in iter(left) {
        let mut occurrences: u32 = 0;

        #[apply(iter)]
        for item in iter(right) {
            if *item > *elem {
                break;
            }

            if *item == *elem {
                occurrences += 1;
            }
        }

        total += occurrences * *elem;
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

        let l = parse_u32(l) as u32;
        let r = parse_u32(r) as u32;

        left[line_num] = l;
        right[line_num] = r;

        line_num += 1;
    }
    (left, right)
}

const fn partition(a: &mut [u32]) -> usize {
    let mut i = 0;
    let right = a.len() - 1;

    #[apply(iter)]
    for j in range(0, right) {
        if a[j] <= a[right] {
            a.swap(j, i);
            i += 1;
        }
    }

    a.swap(i, right);
    i
}

const fn quicksort(a: &mut [u32]) {
    if a.len() > 1 {
        let q = partition(a);
        quicksort(slice_mut(a, 0, q));
        quicksort(slice_mut(a, q + 1, a.len()));
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
