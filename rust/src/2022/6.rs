use crate::{Day, DaySolution};

const START_OF_PACKET_MARKER_LENGTH: usize = 4;
const START_OF_MESSAGE_MARKER_LENGTH: usize = 14;

impl DaySolution for Day<2022, 6> {
    type Part1Output = usize;
    type Part2Output = usize;

    fn part_1(input: &str) -> Self::Part1Output {
        parse::<START_OF_PACKET_MARKER_LENGTH>(input)
    }

    fn part_2(input: &str) -> Self::Part2Output {
        parse::<START_OF_MESSAGE_MARKER_LENGTH>(input)
    }
}

fn parse<const MARKER_LENGTH: usize>(input: &str) -> usize {
    input
        .trim()
        .as_bytes()
        .array_windows::<MARKER_LENGTH>()
        .zip(MARKER_LENGTH..)
        .find_map(|(arr, i_window)| {
            (1..MARKER_LENGTH)
                .all(|i| !arr[i..].contains(&arr[i - 1]))
                .then_some(i_window)
        })
        .unwrap()
}
