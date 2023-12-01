use std::fmt::Display;

use crate::{Day, DaySolution, Input};

const START_OF_PACKET_MARKER_LENGTH: usize = 4;
const START_OF_MESSAGE_MARKER_LENGTH: usize = 14;

impl DaySolution for Day<2022, 6> {
    fn part_1() -> impl Display {
        parse::<START_OF_PACKET_MARKER_LENGTH>(Self::INPUT)
    }

    fn part_2() -> impl Display {
        parse::<START_OF_MESSAGE_MARKER_LENGTH>(Self::INPUT)
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
