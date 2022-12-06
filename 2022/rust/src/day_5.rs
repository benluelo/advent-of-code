use std::{
    collections::{BTreeMap, VecDeque},
    convert::Infallible,
    iter::Peekable,
    str::{Chars, FromStr},
};

pub fn solution(input: String) -> String {
    let [crates, actions] = input.split("\n\n").next_chunk().unwrap();

    let mut crates = parse_crate_stack(crates);

    actions
        .lines()
        .map(|s| str::parse::<Action>(s).unwrap())
        .for_each(|Action { mov, from, to }| {
            for _ in 0..mov {
                let moved = crates.get_mut(&from).unwrap().pop_front().unwrap();
                crates.get_mut(&to).unwrap().push_front(moved);
            }
        });

    crates
        .into_values()
        .map(|mut v| v.pop_front().unwrap())
        .collect()
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
            mov: str::parse(mov).unwrap(),
            from: str::parse(from).unwrap(),
            to: str::parse(to).unwrap(),
        })
    }
}

fn parse_crate_stack(crates: &str) -> BTreeMap<u8, VecDeque<char>> {
    println!("{crates}");

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
                output
                    .entry(column)
                    .or_insert_with(VecDeque::new)
                    .push_back(c);

                next_column(&mut chars, &mut column);
            }
            [' ', ' ', ' '] => next_column(&mut chars, &mut column),
            [' ', '1', ' '] => return output,
            bad => panic!("bad input: {bad:?}"),
        }
    }
}
