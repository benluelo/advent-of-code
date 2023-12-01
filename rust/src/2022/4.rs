use std::{fmt::Display, ops::RangeInclusive};

use crate::{Day, DaySolution, Input};

impl DaySolution for Day<2022, 4> {
    fn part_1() -> impl Display {
        parse(Self::INPUT, |[range_a, range_b]| {
            (range_a.contains(range_b.start()) && range_a.contains(range_b.end()))
                || (range_b.contains(range_a.start()) && range_b.contains(range_a.end()))
        })
    }

    fn part_2() -> impl Display {
        parse(Self::INPUT, |[range_a, range_b]| {
            range_a.contains(range_b.start())
                || range_a.contains(range_b.end())
                || range_b.contains(range_a.start())
                || range_b.contains(range_a.end())
        })
    }
}

fn parse(input: &str, filter_fn: fn(&[RangeInclusive<u32>; 2]) -> bool) -> usize {
    input
        .lines()
        .map(|line| {
            line.split(',')
                .next_chunk::<2>()
                .unwrap()
                .map(|range| {
                    range
                        .split('-')
                        .next_chunk::<2>()
                        .unwrap()
                        .map(|n| str::parse::<u32>(n).unwrap())
                })
                .map(|[a, b]| a..=b)
        })
        .filter(filter_fn)
        .count()
}
