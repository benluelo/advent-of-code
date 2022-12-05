use std::ops::RangeInclusive;

pub fn solution(input: String) -> usize {
    parse(&input, |[range_a, range_b]| {
        (range_a.contains(range_b.start()) && range_a.contains(range_b.end()))
            || (range_b.contains(range_a.start()) && range_b.contains(range_a.end()))
    })
}

pub fn solution_part_2(input: String) -> usize {
    parse(&input, |[range_a, range_b]| {
        range_a.contains(range_b.start())
            || range_a.contains(range_b.end())
            || range_b.contains(range_a.start())
            || range_b.contains(range_a.end())
    })
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
