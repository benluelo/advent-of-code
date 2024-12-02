//! My input only has numbers within 1..=99, so we parse as an i8 for some extra
//! performance (about a 10-15% improvement over using i32, although it is
//! inconsistent).

use cfg_proc::apply;

use crate::{
    day,
    utils::{count_segments, iter},
    Day,
};

#[apply(day)]
impl Day<2024, 2> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

type Int = i8;

const fn parse_int(bz: &[u8]) -> Int {
    crate::utils::parse_i8(bz)
}

const fn parse(input: &[u8]) -> u32 {
    let mut safe_count = 0;

    let input = input.trim_ascii();

    #[apply(iter)]
    'lines: for line in lines(input) {
        let mut prev_diff = None::<Int>;
        let mut prev_n = None::<Int>;

        #[apply(iter)]
        for num in split(line, b" ") {
            let n = parse_int(num.trim_ascii());

            if let Some(prev_n) = prev_n.replace(n) {
                let diff = prev_n - n;
                let safe = is_safe(&mut prev_diff, diff);

                if !safe {
                    continue 'lines;
                }
            }
        }

        safe_count += 1;
    }

    safe_count
}

const fn parse2(input: &[u8]) -> u32 {
    let mut safe_count = 0;

    let input = input.trim_ascii();

    #[apply(iter)]
    'lines: for line in lines(input) {
        let mut remove_idx = None::<usize>;

        let len = count_segments::<b' ', false>(line);

        // who needs unwrap_or
        'dampen: while match remove_idx {
            Some(n) => n < len,
            None => true,
        } {
            let mut prev_diff = None::<Int>;
            let mut prev_n = None::<Int>;

            let mut idx = 0;

            #[apply(iter)]
            for num in split(line, b" ") {
                // always increase idx
                idx += 1;
                if let Some(remove_idx) = remove_idx
                    && remove_idx == idx - 1
                {
                    continue;
                }

                let n = parse_int(num.trim_ascii());

                if let Some(prev_n) = prev_n.replace(n) {
                    let diff = prev_n - n;
                    let safe = is_safe(&mut prev_diff, diff);

                    if !safe {
                        remove_idx = match remove_idx {
                            Some(n) => Some(n + 1),
                            None => Some(0),
                        };

                        continue 'dampen;
                    }
                }
            }

            // the row is safe, continue on to the next row
            safe_count += 1;
            continue 'lines;
        }
    }

    safe_count
}

const fn is_safe(prev_diff: &mut Option<Int>, diff: Int) -> bool {
    if let Some(prev_diff) = prev_diff.replace(diff) {
        // both diffs must be the same "direction", and the new diff must be within
        // 1..=3
        diff.signum() == prev_diff.signum() && is_valid_diff(diff)
    } else {
        // no previous diff, ensure this diff is valid (within 1..=3)
        is_valid_diff(diff)
    }
}

const fn is_valid_diff(diff: Int) -> bool {
    diff.abs() >= 1 && diff.abs() <= 3
}

#[cfg(test)]
#[test]
fn test() {
    let input = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
    ";

    dbg!(parse2(input.as_bytes()));
    // dbg!(parse2(Today::INPUT.as_bytes()));
}
