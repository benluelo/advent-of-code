use alloc::string::String;
use core::{convert::Infallible, iter, str::FromStr};

use cfg_proc::apply;

use crate::{const_helpers::utf8, day, Day};

#[apply(day)]
impl Day<2022, 10> {
    pub fn parse(input: &[u8]) -> i32 {
        let checkpoint_intervals = [20_usize, 40, 40, 40, 40, 40];

        let mut cpu = Cpu::new(
            utf8(input)
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

    pub fn parse2(input: &[u8]) -> String {
        Cpu::new(
            utf8(input)
                .trim()
                .lines()
                .map(|line| line.parse::<Instruction>().unwrap()),
        )
        .array_chunks::<40>()
        .flat_map(|chunk| {
            iter::once('\n').chain(chunk.into_iter().enumerate().map(|(cycle, position)| {
                #[allow(
                    clippy::cast_possible_wrap,
                    clippy::cast_possible_truncation,
                    clippy::range_plus_one
                )]
                // cycle within [position + 1, position - 1]
                if (position - cycle as i32).abs() < 2 {
                    '#'
                } else {
                    '.'
                }
            }))
        })
        .collect::<String>()
    }
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
