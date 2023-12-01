use std::fmt::Display;

use crate::{Day, DaySolution, Input};

impl DaySolution for Day<2022, 1> {
    fn part_1() -> impl Display {
        parse_input(Self::INPUT)
            .into_iter()
            .max()
            .expect("input contained no numbers")
    }

    fn part_2() -> impl Display {
        let mut parsed_input = parse_input(Self::INPUT);

        parsed_input.sort_by(|a, b| b.cmp(a));

        parsed_input[0..3].iter().sum::<u32>()
    }
}

fn parse_input(input: &str) -> Vec<u32> {
    input
        .split("\n\n")
        .map(|s| s.parse::<u32>().ok())
        .fold(vec![], |acc: Vec<u32>, curr| match (&*acc, curr) {
            ([], Some(curr)) => vec![curr],
            ([], None) => acc,
            ([finished @ .., acc], Some(curr)) => finished
                .iter()
                .copied()
                .chain(std::iter::once(acc + curr))
                .collect(),
            (accumulated, None) => accumulated
                .iter()
                .copied()
                .chain(std::iter::once(0))
                .collect(),
        })
}
