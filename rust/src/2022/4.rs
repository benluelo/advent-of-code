use std::ops::RangeInclusive;

use crate::{Day, DaySolution};

impl DaySolution for Day<2022, 4> {
    type Part1Output = usize;
    type Part2Output = usize;

    fn part_1(input: &str) -> Self::Part1Output {
        parse(input, |[range_a, range_b]| {
            (range_a.contains(range_b.start()) && range_a.contains(range_b.end()))
                || (range_b.contains(range_a.start()) && range_b.contains(range_a.end()))
        })
    }

    fn part_2(input: &str) -> Self::Part2Output {
        parse(input, |[range_a, range_b]| {
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
