use crate::{
    const_helpers::{arr, iter, itoa, max, read_until, slice, slice_mut, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 4> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
    // const PART_1: &'static str = "";
    // const PART_2: &'static str = "";
}

#[allow(long_running_const_eval)]
const SOLUTION_PART_1: u32 = parse(Day::<2023, 4>::INPUT.as_bytes());
#[allow(long_running_const_eval)]
const SOLUTION_PART_2: u64 = parse2(&mut arr!(Day::<2023, 4>::INPUT.as_bytes()));

#[test]
fn parse_test() {
    let input = b"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
";

    dbg!(parse2(&mut input.clone()));
}

const COLON_SPACE: &[u8; 2] = b": ";
const PIPE_SPACE: &[u8; 2] = b"| ";

const fn parse(input: &[u8]) -> u32 {
    let mut res = 0;

    let first_line = read_until(input, 0, b"\n");
    let index_of_colon_space = read_until(first_line, 0, COLON_SPACE).len();
    let index_of_pipe_space = read_until(first_line, 0, PIPE_SPACE).len();

    iter! {
        for line in lines(input) {
            let winning_numbers = slice(line, index_of_colon_space + COLON_SPACE.len(), index_of_pipe_space);
            let numbers = slice(line, index_of_pipe_space + PIPE_SPACE.len(), line.len());

            let matches = count_matches(winning_numbers, numbers);

            res += if matches == 0 { 0 } else { 2_u32.pow(matches - 1) };
        }
    }

    res
}

#[allow(clippy::many_single_char_names)]
const fn parse2(input: &mut [u8]) -> u64 {
    let mut res: u64 = 0;

    let first_line = read_until(input, 0, b"\n");
    let index_of_colon_space = read_until(first_line, 0, COLON_SPACE).len();
    let index_of_pipe_space = read_until(first_line, 0, PIPE_SPACE).len();

    let line_len = first_line.len() + 1;

    let mut highest_line_reached = 0;

    let mut i = 0;
    while (i * line_len) < input.len() {
        let (matches, copies) = {
            let line = slice(input, i * line_len, (i + 1) * line_len);

            // if this line has been marked as copied, read it, otherwise only one copy
            let copies = if highest_line_reached <= i {
                1
            } else {
                read_repetitions(line, index_of_colon_space, index_of_pipe_space)
            };

            let winning_numbers = slice(
                line,
                index_of_colon_space + COLON_SPACE.len(),
                index_of_pipe_space,
            );
            let numbers = slice(line, index_of_pipe_space + PIPE_SPACE.len(), line.len());

            let matches = count_matches(winning_numbers, numbers);

            (matches, copies)
        };

        res += copies;

        iter! {
            for n in range(0, matches) {
                let card = i + n as usize + 1;

                let line = slice_mut(input, card * line_len, (card * line_len) + line_len);

                let mut line_copies = if highest_line_reached <= card {
                    1
                } else {
                    read_repetitions(line, index_of_colon_space, index_of_pipe_space)
                };

                line_copies += copies;

                write_repetitions(line, index_of_colon_space, index_of_pipe_space, line_copies);
            }
        }

        highest_line_reached = max(highest_line_reached as u32, (i + 1) as u32 + matches) as usize;

        i += 1;
    }

    res
}

#[cfg(test)]
fn mk_str(bz: &[u8]) -> String {
    String::from_utf8_lossy(bz).to_string()
}

#[allow(long_running_const_eval)]
const fn count_matches(winning_numbers: &[u8], numbers: &[u8]) -> u32 {
    let mut matches = 0;

    let mut idx = 0;
    while idx < numbers.len() {
        let num = slice(numbers, idx, idx + 2);

        let mut w_idx = 0;
        while w_idx < winning_numbers.len() {
            let w_num = slice(winning_numbers, w_idx, w_idx + 2);

            let is_winner = num[0] == w_num[0] && num[1] == w_num[1];

            matches += is_winner as u32;

            w_idx += 3; // eat the space delimiter as well
        }

        idx += 3; // eat the space delimiter as well
    }

    matches
}

#[allow(clippy::many_single_char_names)]
const fn read_repetitions(line: &[u8], colon_space_idx: usize, pipe_space_idx: usize) -> u64 {
    let [a, b, c, d] = slice(line, 0, 4) else {
        panic!()
    };
    let [e, f] = slice(line, colon_space_idx, colon_space_idx + COLON_SPACE.len()) else {
        panic!()
    };
    let [g, h] = slice(line, pipe_space_idx, pipe_space_idx + PIPE_SPACE.len()) else {
        panic!()
    };

    u64::from_be_bytes([*a, *b, *c, *d, *e, *f, *g, *h])
}

#[allow(clippy::many_single_char_names)]
const fn write_repetitions(
    line: &mut [u8],
    colon_space_idx: usize,
    pipe_space_idx: usize,
    value: u64,
) {
    let be_bytes = value.to_be_bytes();

    line[0] = be_bytes[0];
    line[1] = be_bytes[1];
    line[2] = be_bytes[2];
    line[3] = be_bytes[3];
    line[colon_space_idx] = be_bytes[4];
    line[colon_space_idx + 1] = be_bytes[5];
    line[pipe_space_idx] = be_bytes[6];
    line[pipe_space_idx + 1] = be_bytes[7];
}
