use cfg_proc::apply;

use crate::{
    utils::{iter, read_until},
    day, Day,
};

#[apply(day)]
impl Day<2023, 11> {
    pub const fn parse(input: &mut [u8]) -> u64 {
        parse(input)
    }
    pub const fn parse2(input: &mut [u8]) -> u64 {
        parse2(input)
    }
}

const OCCUPIED_ROW_MASK: u8 = 0b1000_0000;
const OCCUPIED_COL_MASK: u8 = 0b0100_0000;

#[test]
#[cfg(test)]
fn parse_test() {
    let mut input = *b"\
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";

    let input = &mut input;

    let res = parse(input);

    dbg!(res);
}

const fn parse_generic<const EXPANSION_FACTOR: u64>(input: &mut [u8]) -> u64 {
    let line_len = read_until(input, 0, b"\n").len() + 1;

    // first pass to mark the rows and cols

    #[apply(iter)]
    for i in range(0, input.len()) {
        if is_galaxy(input[i]) {
            input[row_of(i, line_len)] |= OCCUPIED_ROW_MASK;
            input[col_of(i, line_len)] |= OCCUPIED_COL_MASK;
        }
    }

    let mut res = 0;

    let input = &input;

    #[apply(iter)]
    for (i, c) in enumerate(input) {
        if is_galaxy(c) {
            let remaining_input = input.split_at(i).1;

            #[apply(iter)]
            for (j, c2) in enumerate(remaining_input) {
                if is_galaxy(c2) {
                    let distance = traverse::<EXPANSION_FACTOR>(input, line_len, i, i + j);
                    res += distance;
                }
            }
        }
    }

    res
}

const fn is_galaxy(c: u8) -> bool {
    c & !(OCCUPIED_ROW_MASK | OCCUPIED_COL_MASK) == b'#'
}

const fn parse(input: &mut [u8]) -> u64 {
    parse_generic::<2>(input)
}

const fn parse2(input: &mut [u8]) -> u64 {
    parse_generic::<1_000_000>(input)
}

const fn traverse<const EXPANSION_FACTOR: u64>(
    input: &[u8],
    line_len: usize,
    a: usize,
    b: usize,
) -> u64 {
    let a_row = row_of(a, line_len);
    let a_col = col_of(a, line_len);

    let b_row = row_of(b, line_len);
    let b_col = col_of(b, line_len);

    let (south_row, north_row) = if a_row > b_row {
        (a_row, b_row)
    } else {
        (b_row, a_row)
    };

    let (east_col, west_col) = if a_col > b_col {
        (a_col, b_col)
    } else {
        (b_col, a_col)
    };

    let mut count = 0;
    #[apply(iter)]
    for row in range(north_row + line_len, south_row, line_len) {
        let row = input[row_of(row, line_len)];
        count += 1;
        count += (EXPANSION_FACTOR - 1) * ((row & OCCUPIED_ROW_MASK == 0) as u64);
    }

    #[apply(iter)]
    for col in range(west_col + 1, east_col) {
        let col = input[col];
        count += 1;
        count += (EXPANSION_FACTOR - 1) * ((col & OCCUPIED_COL_MASK == 0) as u64);
    }

    count + (east_col != west_col) as u64 + (north_row != south_row) as u64
}

const fn row_of(b: usize, line_len: usize) -> usize {
    b - (b % line_len)
}

const fn col_of(b: usize, line_len: usize) -> usize {
    b % line_len
}

#[cfg(test)]
#[allow(dead_code)]
fn dbg_input(input: &[u8]) {
    let line_len = read_until(input, 0, b"\n").len() + 1;

    let out = input
        .iter()
        .enumerate()
        .map(|(i, b)| {
            let row_empty_color = "\u{001b}[46;1m";
            let col_empty_color = "\u{001b}[43;1m";
            let both_empty_color = "\u{001b}[42;1m";
            let reset = "\u{001b}[0m";

            let row_occupied = input[i - (i % line_len)] & OCCUPIED_ROW_MASK == OCCUPIED_ROW_MASK;
            let col_occupied = input[i % line_len] & OCCUPIED_COL_MASK == OCCUPIED_COL_MASK;

            let c = (b & !(OCCUPIED_COL_MASK | OCCUPIED_ROW_MASK)) as char;
            println!("{c}, {b}");

            match (row_occupied, col_occupied) {
                (true, true) => format!("{c}"),
                (true, false) => format!("\u{001b}[30m{col_empty_color}{c}{reset}"),
                (false, true) => format!("\u{001b}[30m{row_empty_color}{c}{reset}"),
                (false, false) => format!("\u{001b}[30m{both_empty_color}{c}{reset}"),
            }
        })
        .collect::<String>();

    println!("{out}");
}
