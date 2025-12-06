//! Part 1 and 2 are very similar, and abstracting between them is quite trivial
//! once part 2 is solved. I had originally solved part 1 using a manual/
//! unrolled version of the more general solution for part 2, and my solver for
//! part 2 worked for solving part 1 without modifications. I spent more time on
//! one if condition than anything else (the else-if in the `iter_mut` loop).
//! This solution is also very fast, compiling in <2s on my machine.

use cfg_proc::apply;

use crate::{Day, day, utils::iter};

#[apply(day)]
impl Day<2025, 3> {
    pub const fn parse(input: &[u8]) -> u64 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u64 {
    calculate_total_joltage::<2>(input)
}

const fn parse2(input: &[u8]) -> u64 {
    calculate_total_joltage::<12>(input)
}

const fn calculate_total_joltage<const DIGITS: usize>(input: &[u8]) -> u64 {
    let mut total = 0;

    #[apply(iter)]
    for bank in lines(&input.trim_ascii()) {
        let mut used_batteries = [0; DIGITS];

        let mut remaining = bank.len();

        #[apply(iter)]
        for battery in iter(bank) {
            remaining -= 1;

            let battery = *battery - 48;

            let mut found = false;
            let mut idx = 0;
            #[apply(iter)]
            for n in iter_mut(used_batteries.as_mut_slice()) {
                if found {
                    if *n == 0 {
                        break;
                    }
                    *n = 0;
                } else if remaining >= DIGITS - (idx + 1) && battery > *n {
                    *n = battery;
                    found = true;
                }

                idx += 1;
            }
        }

        #[allow(clippy::cast_possible_truncation)]
        let mut base = 10_u64.pow((DIGITS - 1) as u32);

        #[apply(iter)]
        for n in iter(used_batteries) {
            total += base * *n as u64;
            base /= 10;
        }
    }

    total
}

#[cfg(test)]
#[test]
fn test() {
    let input = "987654321111111
811111111111119
234234234234278
818181911112111
";

    assert_eq!(parse(input.as_bytes()), 357);
    assert_eq!(parse2(input.as_bytes()), 3_121_910_778_619);

    assert_eq!(parse(Today::INPUT.as_bytes()), 17535);
    assert_eq!(parse2(Today::INPUT.as_bytes()), 173_577_199_527_257);

    dbg!(parse2(Today::INPUT.as_bytes()));
    // dbg!(parse2(input.as_bytes()));
}
