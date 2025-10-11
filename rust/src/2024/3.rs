use core::ops::ControlFlow;

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, read_until, slice},
};

#[apply(day)]
impl Day<2024, 3> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

const MUL_WITH_OPEN_BRACKET: &[u8] = b"mul(";
const COMMA: &[u8] = b",";
const CLOSE_BRACKET: &[u8] = b")";

const DO: &[u8] = b"do()";
const DONT: &[u8] = b"don't()";

const fn parse(mut input: &[u8]) -> u32 {
    let mut total = 0;

    loop {
        let Some(new_input) = try_read_until(input, 0, MUL_WITH_OPEN_BRACKET) else {
            break;
        };

        input = slice(
            input,
            new_input.len() + MUL_WITH_OPEN_BRACKET.len(),
            input.len(),
        );

        let lhs = match try_parse_num(&mut input, COMMA) {
            ControlFlow::Continue(Some(lhs)) => lhs,
            ControlFlow::Continue(None) => continue,
            ControlFlow::Break(()) => break,
        };

        let rhs = match try_parse_num(&mut input, CLOSE_BRACKET) {
            ControlFlow::Continue(Some(lhs)) => lhs,
            ControlFlow::Continue(None) => continue,
            ControlFlow::Break(()) => break,
        };

        total += lhs * rhs;
    }

    total
}

const fn parse2(mut input: &[u8]) -> u32 {
    let mut total = 0;

    let mut enabled = true;

    loop {
        // if enabled, parse either don't() or mul(x,y)
        // otherwise, parse do()
        if enabled {
            let Some(new_input_mul) = try_read_until(input, 0, MUL_WITH_OPEN_BRACKET) else {
                break;
            };
            let new_input_dont = try_read_until(input, 0, DONT);

            // either no don't() was found, or it was but a mul( was found first
            #[expect(
                clippy::unnecessary_unwrap,
                reason = "this is the most concise way to do this check in a const context currently"
            )]
            if new_input_dont.is_none() || new_input_mul.len() < new_input_dont.unwrap().len() {
                input = slice(
                    input,
                    new_input_mul.len() + MUL_WITH_OPEN_BRACKET.len(),
                    input.len(),
                );

                let lhs = match try_parse_num(&mut input, COMMA) {
                    ControlFlow::Continue(Some(lhs)) => lhs,
                    ControlFlow::Continue(None) => continue,
                    ControlFlow::Break(()) => break,
                };

                let rhs = match try_parse_num(&mut input, CLOSE_BRACKET) {
                    ControlFlow::Continue(Some(lhs)) => lhs,
                    ControlFlow::Continue(None) => continue,
                    ControlFlow::Break(()) => break,
                };

                total += lhs * rhs;
            } else {
                input = slice(
                    input,
                    new_input_dont.unwrap().len() + DONT.len(),
                    input.len(),
                );

                enabled = false;
            }
        } else {
            let Some(new_input) = try_read_until(input, 0, DO) else {
                break;
            };

            input = slice(input, new_input.len() + DO.len(), input.len());

            enabled = true;
        }
    }

    total
}

const fn try_read_until<'bz>(
    bytes: &'bz [u8],
    start: usize,
    separator: &[u8],
) -> Option<&'bz [u8]> {
    let new_bytes = read_until(bytes, start, separator);

    // not found
    if new_bytes.len() == bytes.len() {
        None
    } else {
        Some(new_bytes)
    }
}

const fn try_parse_num(input: &mut &[u8], sep: &[u8]) -> ControlFlow<(), Option<u32>> {
    let Some(lhs_bz) = try_read_until(input, 0, sep) else {
        return ControlFlow::Break(());
    };

    // too long
    if lhs_bz.len() > 3 {
        return ControlFlow::Continue(None);
    }

    match try_parse_u32(lhs_bz) {
        Some(lhs) => {
            *input = slice(input, lhs_bz.len() + sep.len(), input.len());
            ControlFlow::Continue(Some(lhs))
        }
        None => ControlFlow::Continue(None),
    }
}

// fallible version of utils::parse_u32
#[allow(clippy::cast_possible_truncation)]
pub const fn try_parse_u32(bz: &[u8]) -> Option<u32> {
    let mut res = 0;

    #[apply(iter)]
    for (i, digit) in enumerate(bz) {
        if !digit.is_ascii_digit() {
            return None;
        }
        res += (digit - 48) as usize * 10_usize.pow((bz.len() - i - 1) as u32);
    }

    Some(res as u32)
}

#[cfg(test)]
#[test]
fn test() {
    let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    let input2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";

    dbg!(parse(input.as_bytes()));
    dbg!(parse2(input2.as_bytes()));
}
