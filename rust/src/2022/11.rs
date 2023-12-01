use std::{cmp::Ordering, collections::BTreeMap, ops::Rem};

use crate::{Day, DaySolution};

impl DaySolution for Day<2022, 11> {
    type Part1Output = u64;
    type Part2Output = u64;

    fn part_1(input: &str) -> Self::Part1Output {
        parse_and_solve(input, 20, Some(3))
    }

    fn part_2(input: &str) -> Self::Part2Output {
        parse_and_solve(input, 10_000, None)
    }
}

fn parse_and_solve(
    input: &str,
    iterations: u32,
    worry_level_reduction_after_inspection: Option<u32>,
) -> u64 {
    let mut monkeys = input
        .split("\n\n")
        .map(Monkey::from_str)
        .collect::<BTreeMap<_, _>>();

    let magic_number = monkeys
        .values()
        .fold(1, |acc, monkey| acc * monkey.test.divisible_by);

    for _ in 0..iterations {
        round(
            &mut monkeys,
            magic_number,
            worry_level_reduction_after_inspection,
        );
    }

    let (highest, second_highest) = get_2_most_active_monkeys(&monkeys);

    highest * second_highest
}

struct Monkey {
    items: Vec<u128>,
    operation: Operation,
    test: Test,
    total_inspections: u64,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct MonkeyId(u32);

impl Monkey {
    fn from_str(s: &str) -> (MonkeyId, Self) {
        let Ok([monkey, items, operation, test, if_true, if_false]) = s.lines().next_chunk() else {
            panic!("bad input")
        };

        let Some(("Monkey", monkey_id)) = monkey.trim_end_matches(':').split_once(' ') else {
            panic!("bad input")
        };

        let Some(("Starting items", items)) = items.trim().split_once(": ") else {
            panic!("bad input")
        };

        let Ok(["Operation:", "new", "=", "old", op, x]) = operation.trim().split(' ').next_chunk()
        else {
            panic!("bad input")
        };

        let Ok(["Test:", "divisible", "by", test_divisor]) = test.trim().split(' ').next_chunk()
        else {
            panic!("bad input")
        };

        let Ok(["If", "true:", "throw", "to", "monkey", if_true_monkey]) =
            if_true.trim().split(' ').next_chunk()
        else {
            panic!("bad input")
        };
        let Ok(["If", "false:", "throw", "to", "monkey", if_false_monkey]) =
            if_false.trim().split(' ').next_chunk()
        else {
            panic!("bad input")
        };

        (
            MonkeyId(monkey_id.parse().unwrap()),
            Monkey {
                items: items
                    .split(", ")
                    .map(|s| s.parse::<u128>().unwrap())
                    .collect(),
                operation: match op {
                    "+" => Operation::Add(x.parse().unwrap()),
                    "*" => match x {
                        "old" => Operation::Square,
                        _ => Operation::Mul(x.parse().unwrap()),
                    },
                    _ => panic!("bad input"),
                },
                test: Test {
                    divisible_by: test_divisor.parse().unwrap(),
                    if_true: MonkeyId(if_true_monkey.parse().unwrap()),
                    if_false: MonkeyId(if_false_monkey.parse().unwrap()),
                },
                total_inspections: 0,
            },
        )
    }

    fn monkey_around(
        &mut self,
        modulo: u128,
        worry_level_reduction_after_inspection: Option<u32>,
    ) -> impl Iterator<Item = (MonkeyId, u128)> + '_ {
        self.items
            .drain(..)
            .map(|item| {
                self.total_inspections += 1;

                self.operation.worry_increase(item)
            })
            .map(move |worry_level| {
                if let Some(reduction) = worry_level_reduction_after_inspection {
                    worry_level.div_floor(reduction.into())
                } else {
                    worry_level
                }
                .rem(modulo)
            })
            .map(|worry_level| (self.test.throw_to(worry_level), worry_level))
    }
}

fn get_2_most_active_monkeys(monkeys: &BTreeMap<MonkeyId, Monkey>) -> (u64, u64) {
    monkeys
        .values()
        .map(|m| m.total_inspections)
        .fold((0, 0), |(highest, second_highest), curr| {
            assert!(highest >= second_highest);

            match curr.cmp(&second_highest) {
                Ordering::Less | Ordering::Equal => (highest, second_highest),
                Ordering::Greater => match curr.cmp(&highest) {
                    Ordering::Less | Ordering::Equal => (highest, curr),
                    Ordering::Greater => (curr, highest),
                },
            }
        })
}

enum Operation {
    Add(u128),
    Mul(u128),
    Square,
}

impl Operation {
    fn worry_increase(&self, old: u128) -> u128 {
        match self {
            Operation::Add(x) => x + old,
            Operation::Mul(x) => x * old,
            Operation::Square => old * old,
        }
    }
}

#[derive(Clone)]
struct Test {
    divisible_by: u128,
    if_true: MonkeyId,
    if_false: MonkeyId,
}

impl Test {
    fn throw_to(&self, x: u128) -> MonkeyId {
        if x % self.divisible_by == 0 {
            self.if_true
        } else {
            self.if_false
        }
    }
}

fn round(
    monkeys: &mut BTreeMap<MonkeyId, Monkey>,
    modulo: u128,
    worry_level_reduction_after_inspection: Option<u32>,
) {
    let keys = monkeys.keys().copied().collect::<Vec<_>>();

    for key in keys {
        let mut monkey = monkeys.remove(&key).unwrap();

        for (destination_monkey, item) in
            monkey.monkey_around(modulo, worry_level_reduction_after_inspection)
        {
            monkeys
                .get_mut(&destination_monkey)
                .unwrap()
                .items
                .push(item);
        }

        monkeys.insert(key, monkey);
    }
}
