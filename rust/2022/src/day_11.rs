use std::{cmp::Ordering, collections::BTreeMap};

pub fn solution(input: &str) -> u32 {
    let mut monkeys = input
        .split("\n\n")
        .map(Monkey::from_str)
        .collect::<BTreeMap<_, _>>();

    for _ in 0..20 {
        round(&mut monkeys);
    }

    let (highest, second_highest) = monkeys.values().map(|m| m.total_inspections).fold(
        (0, 0),
        |(highest, second_highest), curr| {
            assert!(highest >= second_highest);

            match curr.cmp(&second_highest) {
                Ordering::Less | Ordering::Equal => (highest, second_highest),
                Ordering::Greater => match curr.cmp(&highest) {
                    Ordering::Less | Ordering::Equal => (highest, curr),
                    Ordering::Greater => (curr, highest),
                },
            }
        },
    );

    highest * second_highest
}

struct Monkey {
    items: Vec<u32>,
    operation: Operation,
    test: Test,
    total_inspections: u32,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct MonkeyId(u32);

impl Monkey {
    fn from_str(s: &str) -> (MonkeyId, Self) {
        let Ok([
            monkey,
            items,
            operation,
            test,
            if_true,
            if_false
        ]) = s.lines().next_chunk()
        else {
            panic!("bad input")
        };

        let Some(("Monkey", monkey_id)) = monkey.trim_end_matches(':').split_once(' ')
        else { panic!("bad input") };

        let Some(("Starting items", items)) = items.trim().split_once(": ")
        else { panic!("bad input") };

        let Ok(["Operation:", "new", "=", "old", op, x]) = operation.trim().split(' ').next_chunk()
        else { panic!("bad input") };

        let Ok(["Test:", "divisible", "by", test_divisor]) = test.trim().split(' ').next_chunk()
        else { panic!("bad input") };

        let Ok(["If", "true:", "throw", "to", "monkey", if_true_monkey]) = if_true.trim().split(' ').next_chunk()
        else { panic!("bad input") };
        let Ok(["If", "false:", "throw", "to", "monkey", if_false_monkey]) = if_false.trim().split(' ').next_chunk()
        else { panic!("bad input") };

        (
            MonkeyId(monkey_id.parse().unwrap()),
            Monkey {
                items: items
                    .split(", ")
                    .map(|s| s.parse::<u32>().unwrap())
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

    fn monkey_around(&mut self) -> impl Iterator<Item = (MonkeyId, u32)> + '_ {
        self.items.drain(..).into_iter().map(|item| {
            self.total_inspections += 1;

            let worry_level = self.operation.worry_increase(item).div_floor(3);

            (self.test.throw_to(worry_level), worry_level)
        })
    }
}

enum Operation {
    Add(u32),
    Mul(u32),
    Square,
}

impl Operation {
    fn worry_increase(&self, old: u32) -> u32 {
        match self {
            Operation::Add(x) => x + old,
            Operation::Mul(x) => x * old,
            Operation::Square => old * old,
        }
    }
}

struct Test {
    divisible_by: u32,
    if_true: MonkeyId,
    if_false: MonkeyId,
}

impl Test {
    fn throw_to(&self, x: u32) -> MonkeyId {
        if x % self.divisible_by == 0 {
            self.if_true
        } else {
            self.if_false
        }
    }
}

fn round(monkeys: &mut BTreeMap<MonkeyId, Monkey>) {
    let keys = monkeys.keys().copied().collect::<Vec<_>>();

    for key in keys {
        let mut monkey = monkeys.remove(&key).unwrap();

        for (destination_monkey, item) in monkey.monkey_around() {
            monkeys
                .get_mut(&destination_monkey)
                .unwrap()
                .items
                .push(item);
        }

        monkeys.insert(key, monkey);
    }
}
