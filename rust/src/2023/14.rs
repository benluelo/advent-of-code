#![allow(dead_code)]

use cfg_proc::apply;

use crate::{
    day,
    utils::{count_segments, iter, line_len},
    Day,
};

#[apply(day)]
#[cfg(not(yolo))]
impl Day<2023, 14> {
    pub const fn parse(input: &mut [u8]) -> usize {
        parse(input)
    }

    pub const fn parse2(_: &mut [u8]) -> &'static str {
        "can't reccomend running this"
    }
}

#[apply(day)]
#[cfg(yolo)]
impl Day<2023, 14> {
    pub const fn parse(input: &mut [u8]) -> usize {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> usize {
        parse2(input)
    }
}

#[test]
#[cfg(test)]
fn parse_test() {
    use crate::utils::utf8;

    //     let input = *b"\
    // O....#....
    // O.OO#....#
    // .....##...
    // OO.#O....O
    // .O.....O#.
    // O.#..O.#.#
    // ..O..#O..O
    // .......O..
    // #....###..
    // #OO..#....
    // ";

    let mut input = Day::<2023, 14>::INPUT.as_bytes().to_vec();

    let score = parse(&mut input);

    println!("{}\n{score}", utf8(&input));
}

const fn calculate_score(input: &[u8], total_rows: usize) -> usize {
    let mut row = 0;
    let mut score = 0;

    #[apply(iter)]
    for char in input {
        match char {
            b'O' => score += total_rows - row,
            b'\n' => row += 1,
            _ => {}
        }
    }

    score
}

const fn cycle(input: &mut [u8], line_len: usize, total_rows: usize) {
    tilt_north(input, line_len, total_rows);
    tilt_west(input, line_len, total_rows);
    tilt_south(input, line_len, total_rows);
    tilt_east(input, line_len, total_rows);
}

const fn tilt_east(input: &mut [u8], line_len: usize, total_rows: usize) {
    #[apply(iter)]
    for i in range(0, total_rows) {
        let mut i = i;

        i *= line_len;
        i += line_len - 2;

        let mut barrier: Option<usize> = None;

        loop {
            match input[i] {
                b'O' => {
                    if let Some(b) = barrier {
                        input.swap(i, b - 1);
                        barrier = Some(b - 1);
                    } else {
                        let first_row = (i - (i % line_len)) + (line_len - 2);
                        input.swap(i, first_row);
                        barrier = Some(first_row);
                    }
                }
                b'#' => barrier = Some(i),
                b'.' => {}
                _ => panic!(),
            }

            if i == 0 {
                break;
            }

            i -= 1;

            if input[i] == b'\n' {
                break;
            }
        }
    }
}

const fn tilt_west(input: &mut [u8], line_len: usize, total_rows: usize) {
    #[apply(iter)]
    for i in range(0, total_rows) {
        let mut i = i;

        i *= line_len;

        let mut barrier: Option<usize> = None;

        loop {
            match input[i] {
                b'O' => {
                    if let Some(b) = barrier {
                        input.swap(i, b + 1);
                        barrier = Some(b + 1);
                    } else {
                        let first_row = i - (i % line_len);
                        input.swap(i, first_row);
                        barrier = Some(first_row);
                    }
                }
                b'#' => barrier = Some(i),
                b'.' => {}
                _ => panic!(),
            }

            i += 1;

            if i >= input.len() || input[i] == b'\n' {
                break;
            }
        }
    }
}

const fn tilt_south(input: &mut [u8], line_len: usize, total_rows: usize) {
    #[apply(iter)]
    for i in range(0, line_len - 1) {
        let mut i = (total_rows * line_len) - (i + 2);

        let mut barrier: Option<usize> = None;

        loop {
            match input[i] {
                b'O' => {
                    if let Some(b) = barrier {
                        input.swap(i, b - line_len);
                        barrier = Some(b - line_len);
                    } else {
                        let first_row = (total_rows * line_len) - (line_len - (i % line_len));
                        input.swap(i, first_row);
                        barrier = Some(first_row);
                    }
                }
                b'#' => barrier = Some(i),
                b'.' => {}
                _ => panic!(),
            }

            let Some(i2) = i.checked_sub(line_len) else {
                break;
            };
            i = i2;
        }
    }
}

const fn tilt_north(input: &mut [u8], line_len: usize, _total_rows: usize) {
    #[apply(iter)]
    for i in range(0, line_len - 1) {
        let mut i = i;

        let mut barrier = None;

        while i < input.len() {
            match input[i] {
                b'O' => {
                    if let Some(b) = barrier {
                        input.swap(i, b + line_len);
                        barrier = Some(b + line_len);
                    } else {
                        input.swap(i, i % line_len);
                        barrier = Some(i % line_len);
                    }
                }
                b'#' => barrier = Some(i),
                b'.' => {}
                _ => panic!(),
            }

            i += line_len;
        }
    }
}

const fn parse(input: &mut [u8]) -> usize {
    let line_len = line_len(input);
    let total_rows = count_segments::<b'\n', true>(input);

    tilt_north(input, line_len, total_rows);

    calculate_score(input, total_rows)
}

const fn parse2(input: &mut [u8]) -> usize {
    let line_len = line_len(input);
    let total_rows = count_segments::<b'\n', true>(input);

    // let mut cache: HashMap<Vec<u8>, Vec<u8>> = HashMap::new();

    #[apply(iter)]
    // i'm not stupid, i'm just lazy
    // with the crappy caching, it took ~50 mins; without, it would take ~10 hours
    // ideal solution would be to build a graph and look for cycle(s?), but this technically works
    // lol
    for _ in range(0, 1_000_000_000) {
        // if i % 100_000 == 0 {
        //     println!("{i}");
        // }

        // let before = input.to_vec();

        // if let Some(cached) = cache.get(input.as_slice()) {
        //     input.copy_from_slice(cached);
        // } else {
        //     cycle(&mut input, line_len, total_rows);
        //     cache.insert(before, input.to_vec());
        // };

        cycle(input, line_len, total_rows);
    }

    calculate_score(input, total_rows)
}

#[cfg(test)]
fn dbg_input(input: &[u8], i: usize, barrier: Option<usize>) {
    use core::fmt::Write;

    let out = input
        .iter()
        .enumerate()
        .fold(String::new(), |mut acc, (i2, b)| {
            let i_color = "\u{001b}[46;1m";
            let barrier_color = "\u{001b}[43;1m";

            let color = if i2 == i {
                i_color
            } else if barrier == Some(i2) {
                barrier_color
            } else {
                return (*b as char).to_string();
            };

            let reset = "\u{001b}[0m";

            let _ = write!(&mut acc, "\u{001b}[30m{color}{c}{reset}", c = *b as char);

            acc
        });

    println!("{out}");
}
