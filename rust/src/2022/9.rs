use std::{cell::Cell, collections::BTreeSet, convert::Infallible, fmt::Display, str::FromStr};

use crate::{Day, DaySolution, Input};

impl DaySolution for Day<2022, 9> {
    fn part_1() -> impl Display {
        get_tail_movements::<2>(Self::INPUT)
    }

    fn part_2() -> impl Display {
        get_tail_movements::<10>(Self::INPUT)
    }
}

fn get_tail_movements<const ROPE_LEN: usize>(input: &str) -> usize {
    parse(input)
        .fold(
            (BTreeSet::new(), [(0, 0); ROPE_LEN]),
            |(mut visited_positions, mut rope), (direction, count)| {
                for _ in 0..count {
                    // [a, b, c, d, e]
                    //
                    // [a, b]
                    // move a
                    // b towards a
                    //
                    // [b, c], [c, d], [d, e]
                    //
                    // b towards c
                    // c towards d
                    // d towards e
                    //
                    // [.., e]
                    // add e to visited positions

                    for (idx, [head, next]) in Cell::from_mut(&mut rope)
                        .as_array_of_cells()
                        .windows(2)
                        .map(|slice| match slice {
                            [a, b] => [a, b],
                            _ => unreachable!(),
                        })
                        .enumerate()
                    {
                        if idx == 0 {
                            head.set(direction.move_pos(head.get()));
                        }

                        next.set(move_towards(next.get(), head.get()));
                    }

                    let [.., tail] = rope.as_slice() else {
                        panic!("bad input")
                    };

                    visited_positions.insert(*tail);
                }

                (visited_positions, rope)
            },
        )
        .0
        .len()
}

fn parse(input: &str) -> impl Iterator<Item = (Direction, u32)> + '_ {
    input.trim().lines().map(|line| match line.split_once(' ') {
        Some((action, count)) => (
            action.parse::<Direction>().unwrap(),
            count.parse::<u32>().unwrap(),
        ),
        _ => panic!("bad input"),
    })
}

fn move_towards((x, y): (i32, i32), to: (i32, i32)) -> (i32, i32) {
    let difference = (to.0 - x, to.1 - y);

    match difference {
        (-1..=1, -1..=1) => {
            // tail is within 1 of head, don't move the tail
            (x, y)
        }
        (2, dy) => (x + 1, y + dy.clamp(-1, 1)),
        (-2, dy) => (x - 1, y + dy.clamp(-1, 1)),
        (dx, 2) => (x + dx.clamp(-1, 1), y + 1),
        (dx, -2) => (x + dx.clamp(-1, 1), y - 1),
        _ => panic!("bad input"),
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Right,
    Left,
    Up,
    Down,
}

impl Direction {
    fn move_pos(self, (x, y): (i32, i32)) -> (i32, i32) {
        match self {
            Direction::Right => (x + 1, y),
            Direction::Left => (x - 1, y),
            Direction::Up => (x, y + 1),
            Direction::Down => (x, y - 1),
        }
    }
}

impl FromStr for Direction {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "R" => Direction::Right,
            "L" => Direction::Left,
            "U" => Direction::Up,
            "D" => Direction::Down,
            _ => panic!("bad input"),
        })
    }
}

#[test]
fn test_movement() {
    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . . .
    //  0 . . H . . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    let mut tail = (0, 0);

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . . .
    //  0 . . T H . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (1, 0));
    assert_eq!(tail, (0, 0));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . . .
    //  0 . . # T H .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (2, 0));
    assert_eq!(tail, (1, 0));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . H .
    //  0 . . # T . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (2, 1));
    assert_eq!(tail, (1, 0));

    //  3 . . . . . .
    //  2 . . . . H .
    //  1 . . . . T .
    //  0 . . # # . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (2, 2));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . H .
    //  0 . . # # . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (2, 1));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . T .
    //  0 . . # # H .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (2, 0));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . T .
    //  0 . . # H . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (1, 0));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . # .
    //  0 . . # T . .
    // -1 . . . H . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    tail = move_towards(tail, (1, -1));
    assert_eq!(tail, (1, 0));
}
