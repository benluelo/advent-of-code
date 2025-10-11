use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, parse_u64, split_once},
};

#[apply(day)]
impl Day<2024, 7> {
    pub const fn parse(input: &[u8]) -> u64 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u64 {
    solve(input, &[Op::Add, Op::Mul])
}

const fn parse2(input: &[u8]) -> u64 {
    solve(input, &[Op::Add, Op::Mul, Op::Concat])
}

const fn solve(input: &[u8], ops: &[Op]) -> u64 {
    let input = input.trim_ascii();

    let mut total = 0;

    #[apply(iter)]
    for line in lines(input) {
        let (target, nums) = split_once(line, b": ").unwrap();
        let target = parse_u64(target);

        let (first, nums) = split_once(nums, b" ").unwrap();
        let first = parse_u64(first);

        if check(target, first, nums, ops) {
            total += target;
        }
    }

    total
}

const fn check(target: u64, current: u64, nums: &[u8], ops: &[Op]) -> bool {
    let (next, nums) = match split_once(nums, b" ") {
        Some((next, nums)) => (next, Some(nums)),
        None => (nums, None),
    };

    let next = parse_u64(next);

    #[apply(iter)]
    for op in iter(ops) {
        let res = op.apply(current, next);
        let res_valid = if let Some(nums) = nums
            && res <= target
        {
            check(target, res, nums, ops)
        } else {
            nums.is_none() && res == target
        };

        if res_valid {
            return true;
        }
    }

    false
}

#[derive(Clone, Copy)]
enum Op {
    Add,
    Mul,
    Concat,
}

impl Op {
    const fn apply(self, lhs: u64, rhs: u64) -> u64 {
        match self {
            Self::Add => lhs + rhs,
            Self::Mul => lhs * rhs,
            Self::Concat => (lhs * 10_u64.pow(rhs.ilog10() + 1)) + rhs,
        }
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
";

    dbg!(parse(input.as_bytes()));
    dbg!(parse2(Today::INPUT.as_bytes()));
}
