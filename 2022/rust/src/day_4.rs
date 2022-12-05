pub fn solution(input: String) -> usize {
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
        .filter(|[range_a, range_b]| {
            (range_a.contains(range_b.start()) && range_a.contains(range_b.end()))
                || (range_b.contains(range_a.start()) && range_b.contains(range_a.end()))
        })
        .count()
}
