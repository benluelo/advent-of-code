//! Part 2 was a bit annoying to solve, but both parts compile nearly instantly.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{count_segments, iter, line_len, parse_u64, slice},
};

#[apply(day)]
impl Day<2025, 6> {
    pub const fn parse(input: &[u8]) -> u64 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u64 {
    let lines = count_segments::<b'\n', true>(input.trim_ascii_start());

    let line_length = line_len(input);
    let ops = slice(input, (lines - 1) * line_length, input.len() - 1);

    let mut i = 0;
    let mut len = 0;
    let mut total = 0;

    let mut current_op = None;

    macro_rules! solve_equation {
        ($op:expr) => {
            let __op = $op;

            let mut answer = match __op {
                Op::Add => 0,
                Op::Mul => 1,
            };

            #[apply(iter)]
            for line in range(0, lines - 1) {
                let bz = slice(
                    input,
                    (line * line_length) + i,
                    (line * line_length) + i + len,
                )
                .trim_ascii();

                let n = parse_u64(bz);

                match __op {
                    Op::Add => answer += n,
                    Op::Mul => answer *= n,
                }
            }

            total += answer;
        };
    }

    loop {
        if i + len >= ops.len() {
            break;
        }

        let new_op = match ops[i + len] {
            b'+' => Op::Add,
            b'*' => Op::Mul,
            b' ' => {
                len += 1;
                continue;
            }
            _ => panic!(),
        };

        let old_op = current_op.replace(new_op);

        if let Some(op) = old_op {
            solve_equation!(op);

            i += len;
            len = 1;
        } else {
            len += 1;
        }
    }

    len += 1;

    solve_equation!(current_op.as_ref().unwrap());

    total
}

#[derive(Debug)]
enum Op {
    Add,
    Mul,
}

const fn parse2(input: &[u8]) -> u64 {
    let lines = count_segments::<b'\n', true>(input.trim_ascii_start());

    let line_length = line_len(input);
    let ops = slice(input, (lines - 1) * line_length, input.len() - 1);

    let mut i = 0;
    let mut len = 0;
    let mut total = 0;

    let mut current_op = None;

    macro_rules! solve_equation {
        ($op:expr) => {
            let __op = $op;

            let mut answer = match __op {
                Op::Add => 0,
                Op::Mul => 1,
            };

            #[apply(iter)]
            for len_idx in range(0, len - 1) {
                let mut num = 0;

                #[apply(iter)]
                for line in range(0, lines - 1) {
                    let digit = input[(line * line_length) + i + len_idx];

                    if digit == b' ' {
                        continue;
                    }

                    let n = (digit - b'0') as u64;

                    num *= 10;
                    num += n;
                }

                match __op {
                    Op::Add => answer += num,
                    Op::Mul => answer *= num,
                }
            }

            total += answer;
        };
    }

    loop {
        if i + len >= ops.len() {
            break;
        }

        let new_op = match ops[i + len] {
            b'+' => Op::Add,
            b'*' => Op::Mul,
            b' ' => {
                len += 1;
                continue;
            }
            _ => panic!(),
        };

        let old_op = current_op.replace(new_op);

        if let Some(op) = old_op {
            solve_equation!(op);

            i += len;
            len = 1;
        } else {
            len += 1;
        }
    }

    len += 1;

    solve_equation!(current_op.as_ref().unwrap());

    total
}

#[cfg(test)]
#[test]
fn test() {
    let input = "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
";

    assert_eq!(parse(input.as_bytes()), 4_277_556);
    assert_eq!(parse(Today::INPUT.as_bytes()), 6_299_564_383_938);

    assert_eq!(parse2(input.as_bytes()), 3_263_827);
    assert_eq!(parse2(Today::INPUT.as_bytes()), 11_950_004_808_442);
}
