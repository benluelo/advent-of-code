use crate::{
    const_helpers::{arr, count_segments, iter, itoa, line_len, read_until, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 14> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    // const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
    // const PART_1: &'static str = "";
    const PART_2: &'static str = "";
}

#[allow(long_running_const_eval)]
const SOLUTION_PART_1: u32 = parse(Day::<2023, 13>::INPUT.as_bytes());

// can't reccomend uncommenting this
// #[allow(long_running_const_eval)]
// const SOLUTION_PART_2: u32 = parse2(Day::<2023, 13>::INPUT.as_bytes());

#[test]
fn parse_test() {
    let mut input = *b"\
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
";

    let mut input = arr!(Day::<2023, 14>::INPUT.as_bytes());

    let score = parse(&mut input);

    println!("{}\n{score}", utf8(&input));
}

const fn calculate_score(input: &[u8], total_rows: usize) -> u32 {
    let mut row = 0;
    let mut score = 0;

    iter! {
        for char in input {
            match char {
                b'O' => score += (total_rows - row) as u32,
                b'\n' => row += 1,
                _ => {}
            }
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
    iter! {
        for i in range(0, total_rows) {
            let mut i = i;

            i *= line_len;
            i += line_len - 2;

            let mut barrier: Option<usize> = None;

            loop {
                match input[i] {
                    b'O' => match barrier {
                        Some(b) => {
                            input.swap(i, b - 1);
                            barrier = Some(b - 1);
                        }
                        None => {
                            let first_row = (i - (i % line_len)) + (line_len - 2);
                            input.swap(i, first_row);
                            barrier = Some(first_row);
                        }
                    },
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
}

const fn tilt_west(input: &mut [u8], line_len: usize, total_rows: usize) {
    iter! {
        for i in range(0, total_rows) {
            let mut i = i;

            i *= line_len;

            let mut barrier: Option<usize> = None;

            loop {
                match input[i] {
                    b'O' => match barrier {
                        Some(b) => {
                            input.swap(i, b + 1);
                            barrier = Some(b + 1);
                        }
                        None => {
                            let first_row = i - (i % line_len);
                            input.swap(i, first_row);
                            barrier = Some(first_row);
                        }
                    },
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
}

const fn tilt_south(input: &mut [u8], line_len: usize, total_rows: usize) {
    iter! {
        for i in range(0, line_len - 1) {
            let mut i = (total_rows * line_len) - (i + 2);

            let mut barrier: Option<usize> = None;

            loop {
                match input[i] {
                    b'O' => match barrier {
                        Some(b) => {
                            input.swap(i, b - line_len);
                            barrier = Some(b - line_len);
                        }
                        None => {
                            let first_row = (total_rows * line_len) - (line_len - (i % line_len));
                            input.swap(i, first_row);
                            barrier = Some(first_row);
                        }
                    },
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
}

const fn tilt_north(input: &mut [u8], line_len: usize, total_rows: usize) {
    iter! {
        for i in range(0, line_len - 1) {
            let mut i = i;

            let mut barrier = None;

            while i < input.len() {
                match input[i] {
                    b'O' => match barrier {
                        Some(b) => {
                            input.swap(i, b + line_len);
                            barrier = Some(b + line_len);
                        }
                        None => {
                            input.swap(i, i % line_len);
                            barrier = Some(i % line_len);
                        }
                    },
                    b'#' => barrier = Some(i),
                    b'.' => {}
                    _ => panic!(),
                }

                i += line_len;
            }
        }
    }
}

const fn parse(input: &mut [u8]) -> u32 {
    let line_len = line_len(input);
    let total_rows = count_segments::<b'\n', true>(input);

    tilt_north(input, line_len, total_rows);

    calculate_score(&input, total_rows)
}

const fn parse2(input: &mut [u8]) -> u32 {
    let line_len = line_len(&input);
    let total_rows = count_segments::<b'\n', true>(&input);

    // let mut cache: HashMap<Vec<u8>, Vec<u8>> = HashMap::new();

    iter! {
        // i'm not stupid, i'm just lazy
        // with the crappy caching, it took ~50 mins; without, it would take ~10 hours
        // ideal solution would be to build a graph and look for cycle(s?), but this technically works lol
        for i in range(0, 1_000_000_000) {
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
    }

    calculate_score(&input, total_rows)
}

#[cfg(test)]
fn dbg_input(input: &[u8], i: usize, barrier: Option<usize>) {
    let line_len = read_until(input, 0, b"\n").len() + 1;

    let out = input
        .iter()
        .enumerate()
        .map(|(i2, b)| {
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

            format!("\u{001b}[30m{color}{c}{reset}", c = *b as char)
        })
        .collect::<Vec<String>>()
        .join("");

    println!("{out}");
}
