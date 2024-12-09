use cfg_proc::apply;

use crate::{
    day,
    utils::{iter, parse_u32, split_once},
    Day,
};

#[apply(day)]
impl Day<2022, 4> {
    pub const fn parse(input: &[u8]) -> usize {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> usize {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> usize {
    let mut total = 0;

    #[apply(iter)]
    for line in lines(input) {
        let (range_a, range_b) = parse_line(line);

        if (range_a.contains(range_b.start()) && range_a.contains(range_b.end()))
            || (range_b.contains(range_a.start()) && range_b.contains(range_a.end()))
        {
            total += 1;
        }
    }

    total
}

const fn parse2(input: &[u8]) -> usize {
    let mut total = 0;

    #[apply(iter)]
    for line in lines(input) {
        let (range_a, range_b) = parse_line(line);

        if range_a.contains(range_b.start())
            || range_a.contains(range_b.end())
            || range_b.contains(range_a.start())
            || range_b.contains(range_a.end())
        {
            total += 1;
        }
    }

    total
}

const fn parse_line(line: &[u8]) -> (Range, Range) {
    let (a, b) = split_once(line, b",").unwrap();

    let (a1, a2) = split_once(a, b"-").unwrap();
    let (b1, b2) = split_once(b, b"-").unwrap();

    (
        Range(parse_u32(a1), parse_u32(a2)),
        Range(parse_u32(b1), parse_u32(b2)),
    )
}

pub struct Range(u32, u32);

impl Range {
    const fn contains(&self, n: u32) -> bool {
        self.0 <= n && n <= self.1
    }

    const fn start(&self) -> u32 {
        self.0
    }

    const fn end(&self) -> u32 {
        self.1
    }
}
