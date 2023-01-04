use std::{
    collections::BTreeMap,
    convert::Infallible,
    iter::Peekable,
    str::{Chars, FromStr},
};

use crate::{Day, DaySolution};

impl DaySolution for Day<2022, 5> {
    type Part1Output = String;
    type Part2Output = String;

    fn part_1(input: &str) -> Self::Part1Output {
        parse(input, |mut crates, Action { mov, from, to }| {
            for _ in 0..mov {
                let moved = crates.get_mut(&from).unwrap().pop().unwrap();
                crates.get_mut(&to).unwrap().push(moved);
            }
            crates
        })
    }

    fn part_2(input: &str) -> Self::Part2Output {
        parse(input, |mut crates, Action { mov, from, to }| {
            let from_stack = crates.get_mut(&from).unwrap();

            let moved = from_stack.split_off(from_stack.len() - mov as usize);

            let to_stack = crates.get_mut(&to).unwrap();

            to_stack.extend(moved);

            crates
        })
    }
}

fn parse(input: &str, actions_fn: fn(CrateStacks, Action) -> CrateStacks) -> String {
    let [crates, actions] = input.split("\n\n").next_chunk().unwrap();
    let crates = parse_crate_stack(crates);

    let crates = actions
        .lines()
        .map(|s| str::parse::<Action>(s).unwrap())
        .fold(crates, actions_fn);

    crates.into_values().map(|mut v| v.pop().unwrap()).collect()
}

#[derive(Debug)]
struct Action {
    mov: u8,
    from: u8,
    to: u8,
}

impl FromStr for Action {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ["move", mov, "from", from, "to", to] = s
            .split(' ')
            .next_chunk()
            .unwrap()
        else {
            panic!()
        };

        Ok(Action {
            mov: mov.parse().unwrap(),
            from: from.parse().unwrap(),
            to: to.parse().unwrap(),
        })
    }
}

type CrateStacks = BTreeMap<u8, Vec<char>>;

fn parse_crate_stack(crates: &str) -> CrateStacks {
    let mut output = BTreeMap::new();

    let mut chars = crates.chars().peekable();

    let mut column = 1;

    let next_column = |iter: &mut Peekable<Chars>, column: &mut _| {
        if iter.peek().unwrap() == &'\n' {
            *column = 1;
        } else {
            // eat whitespace between crate stacks
            *column += 1;
        }

        iter.next();
    };

    loop {
        match chars.next_chunk().unwrap() {
            ['[', c, ']'] => {
                output.entry(column).or_insert_with(Vec::new).push(c);
                next_column(&mut chars, &mut column);
            }
            [' ', ' ', ' '] => next_column(&mut chars, &mut column),
            [' ', '1', ' '] => {
                for v in output.values_mut() {
                    v.reverse();
                }
                return output;
            }
            bad => panic!("bad input: {bad:?}"),
        }
    }
}
