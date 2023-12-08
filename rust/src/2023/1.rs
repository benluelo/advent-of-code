use crate::{
    const_helpers::{iter, itoa, slice, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 1> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
}

const SOLUTION_PART_1: u32 = parse(Day::<2023, 1>::INPUT.as_bytes());
const SOLUTION_PART_2: u32 = parse2(Day::<2023, 1>::INPUT.as_bytes());

const fn parse(bytes: &[u8]) -> u32 {
    const fn parse_line<const REVERSE: bool>(bz: &[u8]) -> u32 {
        let mut i = if REVERSE { bz.len() - 1 } else { 0 };

        loop {
            if let n @ (b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' | b'0') = bz[i]
            {
                return (n - 48) as _;
            }

            if if REVERSE { i == 0 } else { i >= bz.len() - 1 } {
                break;
            }
            if REVERSE {
                i -= 1;
            } else {
                i += 1;
            }
        }

        panic!("invalid input")
    }

    let mut res = 0;

    iter! {
        for line in lines(bytes) {
            let left_digit = parse_line::<false>(line);
            let right_digit = parse_line::<true>(line);

            res += (left_digit * 10) + right_digit;
        }
    }

    res
}

const fn parse2(bytes: &[u8]) -> u32 {
    const fn parse_line<const REVERSE: bool>(bz: &[u8]) -> u32 {
        let mut i = if REVERSE { bz.len() - 1 } else { 0 };

        let idx_start = i;

        loop {
            if let n @ (b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' | b'0') = bz[i]
            {
                return (n - 48) as _;
            }

            let slice = if REVERSE {
                slice(bz, i, idx_start + 1)
            } else {
                slice(bz, idx_start, i + 1)
            };

            // this would be a lot nicer if starts_with/ends_with were const, but alas
            if REVERSE {
                match slice {
                    [b'o', b'n', b'e', ..] => return 1,
                    [b't', b'w', b'o', ..] => return 2,
                    [b't', b'h', b'r', b'e', b'e', ..] => return 3,
                    [b'f', b'o', b'u', b'r', ..] => return 4,
                    [b'f', b'i', b'v', b'e', ..] => return 5,
                    [b's', b'i', b'x', ..] => return 6,
                    [b's', b'e', b'v', b'e', b'n', ..] => return 7,
                    [b'e', b'i', b'g', b'h', b't', ..] => return 8,
                    [b'n', b'i', b'n', b'e', ..] => return 9,
                    _ => {}
                }
            } else {
                match slice {
                    [.., b'o', b'n', b'e'] => return 1,
                    [.., b't', b'w', b'o'] => return 2,
                    [.., b't', b'h', b'r', b'e', b'e'] => return 3,
                    [.., b'f', b'o', b'u', b'r'] => return 4,
                    [.., b'f', b'i', b'v', b'e'] => return 5,
                    [.., b's', b'i', b'x'] => return 6,
                    [.., b's', b'e', b'v', b'e', b'n'] => return 7,
                    [.., b'e', b'i', b'g', b'h', b't'] => return 8,
                    [.., b'n', b'i', b'n', b'e'] => return 9,
                    _ => {}
                }
            }

            // dbg!(String::from_utf8_lossy(slice));

            // if that was the last char, break
            if if REVERSE { i == 0 } else { i >= bz.len() - 1 } {
                break;
            }

            // progress cursor
            if REVERSE {
                i -= 1;
            } else {
                i += 1;
            }
        }

        panic!()
    }

    let mut res = 0;

    iter! {
        for line in lines(bytes) {
            let left_digit = parse_line::<false>(line);
            let right_digit = parse_line::<true>(line);

            res += (left_digit * 10) + right_digit;
        }
    }

    res
}
