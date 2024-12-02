use cfg_proc::apply;

use crate::{utils::utf8, day, Day};

const START_OF_PACKET_MARKER_LENGTH: usize = 4;
const START_OF_MESSAGE_MARKER_LENGTH: usize = 14;

#[apply(day)]
impl Day<2022, 6> {
    pub fn parse(input: &[u8]) -> usize {
        parse::<START_OF_PACKET_MARKER_LENGTH>(utf8(input))
    }

    pub fn parse2(input: &[u8]) -> usize {
        parse::<START_OF_MESSAGE_MARKER_LENGTH>(utf8(input))
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
