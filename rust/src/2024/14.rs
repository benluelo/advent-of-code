// Part 1 was pretty trivial, but part 2 was quite a bit more difficult to
// figure out. A naive solution would be to render each iteration and look for
// patterns. However, due to the clustering caused by forming the christmas tree
// shape, most of the robots are likely to be in one or two quadrants, resulting
// in a low safety score.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, parse_i32, split_once},
};

#[apply(day)]
impl Day<2024, 14> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        // parse2(input)
        0
    }
}

// const SIZE: (i32, i32) = (11, 7);
const SIZE: (i32, i32) = (101, 103);
const HALF_SIZE: (i32, i32) = (SIZE.0 / 2, SIZE.1 / 2);

const fn parse(input: &[u8]) -> u32 {
    solve(input.trim_ascii(), 100)
}

#[allow(clippy::cast_sign_loss)]
const fn parse2(input: &[u8]) -> u32 {
    let input = input.trim_ascii();

    let mut min = (i32::MAX, u32::MAX);

    #[apply(iter)]
    for seconds in range(1, 150) {
        let score = solve(input, seconds);

        if score < min.1 {
            min = (seconds, score);
        }
    }

    min.0 as u32
}

const fn solve(input: &[u8], seconds: i32) -> u32 {
    let mut q1 = 0;
    let mut q2 = 0;
    let mut q3 = 0;
    let mut q4 = 0;

    #[apply(iter)]
    for line in lines(input) {
        let (p, v) = split_once(line, b" ").unwrap();

        let p = parse_coords(p);
        let v = parse_coords(v);

        let (x, y) = move_robot(p, v, SIZE, seconds);

        if x == HALF_SIZE.0 || y == HALF_SIZE.1 {
            continue;
        }

        match (x < HALF_SIZE.0, y < HALF_SIZE.1) {
            (true, true) => q1 += 1,
            (true, false) => q2 += 1,
            (false, true) => q3 += 1,
            (false, false) => q4 += 1,
        }
    }

    q1 * q2 * q3 * q4
}

const fn parse_coords(p: &[u8]) -> (i32, i32) {
    let (_, p) = split_once(p, b"=").unwrap();
    let (px, py) = split_once(p, b",").unwrap();

    (parse_i32(px), parse_i32(py))
}

const fn move_robot(
    (px, py): (i32, i32),
    (vx, vy): (i32, i32),
    (w, h): (i32, i32),
    steps: i32,
) -> (i32, i32) {
    (
        (px + (steps * vx)).rem_euclid(w),
        (py + (steps * vy)).rem_euclid(h),
    )
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
";

    dbg!(parse(input.as_bytes()));
    dbg!(parse2(Today::INPUT.as_bytes()));
}
