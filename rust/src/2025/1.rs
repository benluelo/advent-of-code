//! Part 1 was easy, just simple modular arithmetic. For part 2 though, I just
//! could not get the math to work for counting the times modulated, so I
//! decided to just go for the inneficient naive solution of manually clicking
//! the dial one click at a time. It still compiles just under 2s, so it's not
//! too terrible. Maybe one day I'll revisit this problem and figure it out.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, slice},
};

#[apply(day)]
impl Day<2025, 1> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u32 {
    let mut total_zeroes: u32 = 0;

    let mut current = 50;

    #[apply(iter)]
    for line in lines(&input.trim_ascii()) {
        let Ok(clicks) = i64::from_ascii(slice(line, 1, line.len())) else {
            panic!()
        };

        match line[0] {
            b'L' => current -= clicks,
            b'R' => current += clicks,
            _ => unreachable!(),
        }

        if current % 100 == 0 {
            total_zeroes += 1;
        }
    }

    total_zeroes
}

const fn parse2(input: &[u8]) -> u32 {
    let mut total_zeroes: u32 = 0;

    let mut dial = Dial::new();

    #[apply(iter)]
    for line in lines(&input.trim_ascii()) {
        let Ok(clicks) = u32::from_ascii(slice(line, 1, line.len())) else {
            panic!()
        };

        let times_modulated = match line[0] {
            b'L' => dial.turn_left(clicks),
            b'R' => dial.turn_right(clicks),
            _ => unreachable!(),
        };

        total_zeroes += times_modulated;
    }

    total_zeroes
}

struct Dial(u8);

impl Dial {
    const fn new() -> Self {
        Self(50)
    }

    const fn is_zero(&self) -> bool {
        self.0 == 0
    }

    const fn turn_left(&mut self, clicks: u32) -> u32 {
        let mut total = 0;

        #[apply(iter)]
        for _ in range(0, clicks) {
            if self.is_zero() {
                // 100 - 1
                self.0 = 99;
            } else {
                self.0 -= 1;
            }

            if self.is_zero() {
                total += 1;
            }
        }

        total
    }

    const fn turn_right(&mut self, clicks: u32) -> u32 {
        let mut total = 0;

        #[apply(iter)]
        for _ in range(0, clicks) {
            self.0 = (self.0 + 1) % 100;
            if self.is_zero() {
                total += 1;
            }
        }

        total
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
";

    //     let input = "L50
    // L97
    // R715
    // ";

    //     let input = "L50
    // R1000";
    // let input = "L100";

    // dbg!(parse2(Today::INPUT.as_bytes()));
    dbg!(parse(input.as_bytes()));
}
