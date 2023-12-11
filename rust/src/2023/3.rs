use crate::{
    const_helpers::{iter, itoa, min_usize, parse_int, read_until, slice, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 3> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
    // const PART_1: &'static str = "";
    // const PART_2: &'static str = "";
}

const SOLUTION_PART_1: u32 = parse(Day::<2023, 3>::INPUT.as_bytes());
const SOLUTION_PART_2: u32 = parse2(Day::<2023, 3>::INPUT.as_bytes());

#[test]
fn parse_test() {
    let input = b"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";

    let input = b"..........
..129.....
....*.....
113.827...
..........
.1000.....
...99.....
..........
..123.....
";

    dbg!(parse2(input));
}

const fn parse(input: &[u8]) -> u32 {
    let line_len = read_until(input, 0, b"\n").len() + 1;

    let mut x = 0;
    let mut y = 0;

    let mut in_number = false;
    let mut number_start_idx = 0;

    let mut res = 0;

    iter! {
        for char in input {
            match char {
                b'0'..=b'9' => {
                    if !in_number {
                        in_number = true;
                        number_start_idx = x;
                    }
                    x += 1;
                }
                non_numeric => {
                    if in_number {
                        let mut is_part_number = false;

                        iter! {
                            for num_x in range(number_start_idx, x) {
                                is_part_number =
                                    any_neighbour_is_symbol(is_part_number, input, num_x, y, line_len);
                            }
                        }

                        if is_part_number {
                            let bz = slice(input, number_start_idx + (y * line_len), x + (y * line_len));
                            res += parse_int(bz);
                        }

                        in_number = false;
                    }

                    match non_numeric {
                        b'\n' => {
                            x = 0;
                            y += 1;
                        }
                        _ => {
                            x += 1;
                        }
                    }
                }
            }
        }
    }

    res
}

const fn parse2(input: &[u8]) -> u32 {
    let line_len = read_until(input, 0, b"\n").len() + 1;

    let mut x = 0;
    let mut y = 0;

    let mut res = 0;

    iter! {
        for char in input {
            match char {
                b'*' => {
                    let mut gn = GearNumbers::None;

                    let max_x = line_len;
                    let max_y = (input.len() / line_len) - 1;

                    let mut previous_char_was_digit = false;

                    // top left
                    if x > 0 && y > 0 {
                        let idx = mk_idx(x - 1, y - 1, line_len);
                        let is_ascii_digit = input[idx].is_ascii_digit();
                        if is_ascii_digit {
                            gn = gn.with(idx);
                        }
                        previous_char_was_digit = is_ascii_digit;
                    }
                    // top middle
                    if y > 0 {
                        let idx = mk_idx(x, y - 1, line_len);
                        let is_ascii_digit = input[idx].is_ascii_digit();
                        if !previous_char_was_digit && is_ascii_digit {
                            gn = gn.with(idx);
                        }
                        previous_char_was_digit = is_ascii_digit;
                    }
                    // top right
                    if x < max_x && y > 0 {
                        let idx = mk_idx(x + 1, y - 1, line_len);
                        let is_ascii_digit = input[idx].is_ascii_digit();
                        if !previous_char_was_digit && is_ascii_digit {
                            gn = gn.with(idx);
                        }
                    }

                    // right
                    if x < max_x {
                        let idx = mk_idx(x + 1, y, line_len);
                        if input[idx].is_ascii_digit() {
                            gn = gn.with(idx);
                        }
                    }

                    previous_char_was_digit = false;

                    // bottom right
                    if x < max_x && y < max_y {
                        let idx = mk_idx(x + 1, y + 1, line_len);
                        let is_ascii_digit = input[idx].is_ascii_digit();
                        if is_ascii_digit {
                            gn = gn.with(idx);
                        }
                        previous_char_was_digit = is_ascii_digit;
                    }
                    // bottom middle
                    if y < max_y {
                        let idx = mk_idx(x, y + 1, line_len);
                        let is_ascii_digit = input[idx].is_ascii_digit();
                        if !previous_char_was_digit && is_ascii_digit {
                            gn = gn.with(idx);
                        }
                        previous_char_was_digit = is_ascii_digit;
                    }
                    // bottom left
                    if x > 0 && y < max_y {
                        let idx = mk_idx(x - 1, y + 1, line_len);
                        let is_ascii_digit = input[idx].is_ascii_digit();
                        if !previous_char_was_digit && is_ascii_digit {
                            gn = gn.with(idx);
                        }
                    }

                    // left
                    if x > 0 {
                        let idx = mk_idx(x - 1, y, line_len);
                        if input[idx].is_ascii_digit() {
                            gn = gn.with(idx);
                        }
                    }

                    if let GearNumbers::Two(n, m) = gn {
                        res += find_number_in_line(n, line_len, input)
                            * find_number_in_line(m, line_len, input);
                    }

                    x += 1;
                }
                b'\n' => {
                    x = 0;
                    y += 1;
                }
                _ => {
                    x += 1;
                }
            }
        }
    }

    res
}

#[rustfmt::skip]
const fn any_neighbour_is_symbol(is_part_number: bool, input: &[u8], num_x: usize, y: usize, line_len: usize) -> bool {
    is_part_number
        || is_symbol(input[min_usize(input.len() - 1, num_x + 1               + ((y + 1) * line_len))             ])
        || is_symbol(input[min_usize(input.len() - 1, num_x + 1               +  (y.saturating_sub(1) * line_len))])
        || is_symbol(input[min_usize(input.len() - 1, num_x.saturating_sub(1) +  (y.saturating_sub(1) * line_len))])
        || is_symbol(input[min_usize(input.len() - 1, num_x.saturating_sub(1) + ((y + 1) * line_len))             ])
        || is_symbol(input[min_usize(input.len() - 1, num_x + 1               +  (y * line_len))                  ])
        || is_symbol(input[min_usize(input.len() - 1, num_x.saturating_sub(1) +  (y * line_len))                  ])
        || is_symbol(input[min_usize(input.len() - 1, num_x                   + ((y + 1) * line_len))             ])
        || is_symbol(input[min_usize(input.len() - 1, num_x                   +  (y.saturating_sub(1) * line_len))])
}

const fn is_symbol(b: u8) -> bool {
    !matches!(b, b'0'..=b'9' | b'.' | b'\n')
}

// fn mk_str(bz: &[u8]) -> String {
//     String::from_utf8_lossy(bz).to_string()
// }

const fn find_number_in_line(idx: usize, line_len: usize, input: &[u8]) -> u32 {
    let x = idx % line_len;

    let mut right = 0;
    while right < line_len {
        if !input[idx + right].is_ascii_digit() {
            break;
        }

        right += 1;
    }

    let mut left = x;
    while left != 0 {
        if !input[idx - (x - left) - 1].is_ascii_digit() {
            break;
        }

        left -= 1;
    }

    let bz = slice(input, idx - (x - left), idx + right);

    parse_int(bz)
}

#[derive(Debug)]
enum GearNumbers {
    None,
    One(usize),
    Two(usize, usize),
}

impl GearNumbers {
    const fn with(self, n: usize) -> Self {
        match self {
            GearNumbers::None => Self::One(n),
            GearNumbers::One(m) => Self::Two(m, n),
            GearNumbers::Two(_, _) => panic!(),
        }
    }
}

const fn mk_idx(x: usize, y: usize, line_len: usize) -> usize {
    x + (y * line_len)
}
