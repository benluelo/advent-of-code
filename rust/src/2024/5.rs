//! An extremely brute-force solution. Many assumptions about the input are
//! made:
//!
//! - all page numbers are 2 digits
//! - the ordering rules provide a correct total ordering
//! - the update page number sequences have an odd length
//!
//! Given these assumptions, we can optimize the ordering comparisons to simply
//! compare the 2-byte sequences that represent the page numbers, without any
//! sanity checks for ordering consistency, and parsing the middle element of
//! the sequence can be done by simply reading the middle two characters of the
//! line.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, parse_u32, read_until, slice, slice_mut, split_once, split_once_mut},
};

#[cfg(yolo)]
#[apply(day)]
impl Day<2024, 5> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> u32 {
        parse2(input)
    }
}

#[cfg(not(yolo))]
#[apply(day)]
impl Day<2024, 5> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(_: &[u8]) -> &'static str {
        let _ = || parse2;
        "can't recommend doing this in const"
    }
}

const fn parse(input: &[u8]) -> u32 {
    let mut total = 0;

    let input = input.trim_ascii();

    let (page_ordering_rules, update_page_numbers) = split_once(input, b"\n\n").unwrap();

    let orderings = Orderings(page_ordering_rules);

    #[apply(iter)]
    for line in lines(update_page_numbers) {
        let line = line.trim_ascii();

        let mut valid = true;

        let len = (line.len() + 1) / 3;

        #[apply(iter)]
        for i in range(0, len) {
            #[apply(iter)]
            for j in range(i + 1, len) {
                let idx_i = i * 3;
                let idx_j = j * 3;

                let page_i = PageNum::new(line[idx_i], line[idx_i + 1]);
                let page_j = PageNum::new(line[idx_j], line[idx_j + 1]);

                if let Some(Ordering::After) = orderings.cmp(&page_i, &page_j) {
                    valid = false;
                    break;
                }
            }
        }

        if valid {
            let middle_bz = slice(line, (line.len() / 2) - 1, (line.len() / 2) + 1);
            let middle = parse_u32(middle_bz);

            total += middle;
        }
    }

    total
}

const fn parse2(input: &mut [u8]) -> u32 {
    let mut total = 0;

    let (page_ordering_rules, update_page_numbers) = split_once_mut(input, b"\n\n").unwrap();

    let orderings = Orderings(page_ordering_rules.trim_ascii());

    let mut cursor = 0;

    while cursor < update_page_numbers.len() {
        let line_len = read_until(update_page_numbers, cursor, b"\n").len();
        // +1 for newline
        let line: &mut [u8] = slice_mut(update_page_numbers, cursor, cursor + line_len + 1);
        cursor += line_len + 1;

        let mut valid = true;

        let len = (line.len() + 1) / 3;

        let mut restart = true;

        'outer: while restart {
            #[apply(iter)]
            for i in range(0, len) {
                #[apply(iter)]
                for j in range(i + 1, len) {
                    let idx_i = i * 3;
                    let idx_j = j * 3;

                    let page_i = PageNum::new(line[idx_i], line[idx_i + 1]);
                    let page_j = PageNum::new(line[idx_j], line[idx_j + 1]);

                    if let Some(Ordering::After) = orderings.cmp(&page_i, &page_j) {
                        valid = false;
                        line.swap(idx_i, idx_j);
                        line.swap(idx_i + 1, idx_j + 1);
                        restart = true;
                        continue 'outer;
                    }
                }
            }

            restart = false;
        }

        if !valid {
            let middle_bz = slice(line, (line.len() / 2) - 1, (line.len() / 2) + 1);

            let middle = parse_u32(middle_bz);

            total += middle;
        }
    }

    total
}

struct Orderings<'a>(&'a [u8]);

impl Orderings<'_> {
    const fn cmp(&self, lhs: &PageNum, rhs: &PageNum) -> Option<Ordering> {
        let bz = self.0;

        #[apply(iter)]
        for line in lines(bz) {
            let l = PageNum::new(line[0], line[1]);
            let r = PageNum::new(line[3], line[4]);

            if l.eq(lhs) && r.eq(rhs) {
                return Some(Ordering::Before);
            } else if r.eq(lhs) && l.eq(rhs) {
                return Some(Ordering::After);
            }
        }

        None
    }
}

struct PageNum(u16);

impl PageNum {
    const fn new(a: u8, b: u8) -> Self {
        Self(u16::from_be_bytes([a, b]))
    }

    const fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

enum Ordering {
    Before,
    After,
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
";

    // dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
    dbg!(parse2(&mut input.as_bytes().to_owned()));
}
