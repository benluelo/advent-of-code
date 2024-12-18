use cfg_proc::apply;

use crate::{
    day,
    utils::{iter, parse_u64, slice, split_once},
    Day,
};

#[apply(day)]
impl Day<2024, 13> {
    pub const fn parse(input: &[u8]) -> u64 {
        solve_all(input, 0)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        solve_all(input, 10_000_000_000_000)
    }
}

const fn solve_all(input: &[u8], prize_offset: u64) -> u64 {
    let input = input.trim_ascii();

    let mut total = 0;

    #[apply(iter)]
    for machine in split(input, b"\n\n") {
        let (a, rest) = split_once(machine, b"\n").unwrap();
        let (b, c) = split_once(rest, b"\n").unwrap();

        // the input only contains 2-digit numbers for the button values
        let ax = parse_u64(slice(a, 12, 14));
        let ay = parse_u64(slice(a, 18, 20));

        let bx = parse_u64(slice(b, 12, 14));
        let by = parse_u64(slice(b, 18, 20));

        let (_, c) = split_once(c, b": ").unwrap();
        let (cx, cy) = split_once(c, b", ").unwrap();
        let cx = parse_u64(split_once(cx, b"=").unwrap().1) + prize_offset;
        let cy = parse_u64(split_once(cy, b"=").unwrap().1) + prize_offset;

        #[allow(clippy::cast_possible_wrap)]
        if let Some(solution) = solve(
            (ax as i64, ay as i64),
            (bx as i64, by as i64),
            (cx as i64, cy as i64),
        ) {
            total += solution;
        }
    }

    total
}

const fn solve((ax, ay): (i64, i64), (bx, by): (i64, i64), (cx, cy): (i64, i64)) -> Option<u64> {
    let b = ((ax * cy) - (cx * ay)) / ((ax * by) - (bx * ay));

    let a = (cx - (bx * b)) / ax;

    if (ax * a) + (bx * b) == cx && (ay * a) + (by * b) == cy {
        #[allow(clippy::cast_sign_loss)]
        Some((a as u64 * 3) + b as u64)
    } else {
        None
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
";

    dbg!(Today::parse(input.as_bytes()));
    dbg!(Today::parse(Today::INPUT.as_bytes()));
}
