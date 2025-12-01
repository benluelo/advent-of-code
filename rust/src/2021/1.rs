use cfg_proc::apply;

use crate::{Day, day, utils::iter};

#[apply(day)]
impl Day<2021, 1> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u32 {
    let mut prev_val = None;
    let mut total = 0;

    #[apply(iter)]
    for line in lines(&input) {
        let Ok(val) = u16::from_ascii(line) else {
            panic!()
        };
        if let Some(prev_val) = prev_val
            && val > prev_val
        {
            total += 1;
        }

        prev_val = Some(val);
    }

    total
}

const fn parse2(input: &[u8]) -> u32 {
    let mut first = None;
    let mut second = None;

    let mut prev_sum = None;

    let mut total = 0;

    #[apply(iter)]
    for line in lines(&input) {
        let Ok(val) = u16::from_ascii(line) else {
            panic!()
        };

        match (first.as_mut(), second.as_mut()) {
            (None, None) => first = Some(val),
            (Some(_), None) => second = Some(val),
            (Some(first), Some(second)) => {
                let sum = *first + *second + val;

                if let Some(prev_sum) = prev_sum
                    && sum > prev_sum
                {
                    total += 1;
                }

                *first = *second;
                *second = val;
                prev_sum = Some(sum);
            }
            _ => unreachable!(),
        }
    }

    total
}

#[test]
fn test() {
    let input = b"199
200
208
210
200
207
240
269
260
263
";

    dbg!(parse(input));

    dbg!(parse2(Today::INPUT.as_bytes()));
}
