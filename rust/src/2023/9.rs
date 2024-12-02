use cfg_proc::apply;

use crate::{
    day,
    utils::{count_segments, iter, parse_i32},
    Day,
};

#[apply(day)]
impl Day<2023, 9> {
    pub const fn parse(input: &[u8]) -> i32 {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> i32 {
        parse2(input)
    }
}

#[test]
#[cfg(test)]
fn parse_test() {
    let input = b"\
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
";

    dbg!(parse_generic::<true>(input));
    // dbg!(parse(Day::<2023, 9>::INPUT.as_bytes()));
    // dbg!(parse(input2));
    // dbg!(parse2(input));
}

const fn parse(input: &[u8]) -> i32 {
    parse_generic::<false>(input)
}

const fn parse2(input: &[u8]) -> i32 {
    parse_generic::<true>(input)
}

const fn parse_generic<const READ_LEFT: bool>(bytes: &[u8]) -> i32 {
    let mut res = 0;

    #[apply(iter)]
    for line in lines(bytes) {
        let len = count_segments::<b' ', false>(line);

        let mut line_res = 0;

        #[apply(iter)]
        for n in range(0, len) {
            line_res += calculate_edge::<READ_LEFT>(n, (len - 1) - n, Line { bz: line, len });
        }

        res += line_res;
    }

    res
}

#[derive(Clone, Copy)]
struct Line<'a> {
    bz: &'a [u8],
    // the amount of numbers on this line
    len: usize,
}

const fn calculate_edge<const READ_LEFT: bool>(i: usize, depth: usize, line: Line) -> i32 {
    if depth == 0 {
        read_n::<READ_LEFT>(i, line)
    } else {
        calculate_edge::<READ_LEFT>(i + 1, depth - 1, line)
            - calculate_edge::<READ_LEFT>(i, depth - 1, line)
    }
}

const fn read_n<const READ_LEFT: bool>(mut i: usize, line: Line) -> i32 {
    if READ_LEFT {
        i = (line.len - 1) - i;
    }

    let bz = line.bz;
    #[apply(iter)]
    for segment in split(bz, b" ") {
        if i == 0 {
            return parse_i32(segment);
        }
        i -= 1;
    }

    panic!()
}
