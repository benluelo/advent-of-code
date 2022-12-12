use std::{convert::Infallible, str::FromStr};

pub fn solution(input: &str) -> i32 {
    let checkpoint_intervals = [20_usize, 40, 40, 40, 40, 40];

    let mut cpu = Cpu::new(
        input
            .trim()
            .lines()
            .map(|line| line.parse::<Instruction>().unwrap()),
    );

    checkpoint_intervals
        .into_iter()
        .map(|interval| (interval, cpu.nth(interval - 1).unwrap()))
        .fold(
            (0, 0),
            |(total_cycles_passed, signal_strengths_sum),
             (interval, signal_strength_at_interval)| {
                let total_cycles_passed = total_cycles_passed + interval;

                #[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
                let score = total_cycles_passed as i32 * signal_strength_at_interval;

                (total_cycles_passed, signal_strengths_sum + score)
            },
        )
        .1
}

struct Cpu<I: Iterator<Item = Instruction>> {
    instructions: I,
    state: CpuState,
}

impl<I: Iterator<Item = Instruction>> Cpu<I> {
    fn new(instructions: I) -> Self {
        Self {
            instructions,
            state: CpuState::Noop { x: 1 },
        }
    }
}

impl<I: Iterator<Item = Instruction>> Iterator for Cpu<I> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            CpuState::Noop { x } => match self.instructions.next() {
                Some(Instruction::Noop) => Some(x),
                Some(Instruction::AddX(add_x)) => {
                    self.state = CpuState::Adding {
                        x,
                        add_next_cycle: add_x,
                    };
                    Some(x)
                }
                None => None,
            },
            CpuState::Adding { x, add_next_cycle } => {
                self.state = CpuState::Noop {
                    x: x + add_next_cycle,
                };

                Some(x)
            }
        }
    }
}

enum CpuState {
    Noop { x: i32 },
    Adding { x: i32, add_next_cycle: i32 },
}

enum Instruction {
    Noop,
    AddX(i32),
}

impl FromStr for Instruction {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.split_at(4) {
            ("noop", "") => Instruction::Noop,
            ("addx", x) => Instruction::AddX(x.trim().parse().unwrap()),
            _ => panic!("bad input"),
        })
    }
}
