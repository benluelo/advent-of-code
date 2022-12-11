const START_OF_PACKET_MARKER_LENGTH: usize = 4;
const START_OF_MESSAGE_MARKER_LENGTH: usize = 14;

pub fn solution(input: &str) -> usize {
    parse::<START_OF_PACKET_MARKER_LENGTH>(input)
}

pub fn solution_part_2(input: &str) -> usize {
    parse::<START_OF_MESSAGE_MARKER_LENGTH>(input)
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
