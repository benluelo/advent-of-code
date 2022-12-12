use std::{collections::BTreeSet, convert::Infallible, str::FromStr};

pub fn solution(input: &str) -> usize {
    let output = parse(input).fold(
        (BTreeSet::<(i32, i32)>::new(), (0, 0), (0, 0)),
        |(mut visited_positions, mut head_position, mut tail_position), (direction, count)| {
            for _ in 0..count {
                direction.move_pos(&mut head_position);

                move_towards(&mut tail_position, head_position);

                visited_positions.insert(tail_position);
            }

            (visited_positions, head_position, tail_position)
        },
    );

    output.0.len()
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

fn move_towards(tail_position: &mut (i32, i32), head_position: (i32, i32)) {
    let difference = (
        head_position.0 - tail_position.0,
        head_position.1 - tail_position.1,
    );

    dbg!(difference);

    match difference {
        (-1..=1, -1..=1) => {
            // tail is within 1 of head, don't move the tail
        }
        (2, y) => {
            tail_position.0 += 1;
            tail_position.1 += y;
        }
        (-2, y) => {
            tail_position.0 -= 1;
            tail_position.1 += y;
        }
        (x, 2) => {
            tail_position.1 += 1;
            tail_position.0 += x;
        }
        (x, -2) => {
            tail_position.1 -= 1;
            tail_position.0 += x;
        }
        _ => panic!("bad input"),
    }
}

enum Direction {
    Right,
    Left,
    Up,
    Down,
}

impl Direction {
    fn move_pos(&self, pos: &mut (i32, i32)) {
        match self {
            Direction::Right => pos.0 += 1,
            Direction::Left => pos.0 -= 1,
            Direction::Up => pos.1 += 1,
            Direction::Down => pos.1 -= 1,
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
    let steps = parse(
        "R 1
R 1",
    );

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
    move_towards(&mut tail, (1, 0));
    assert_eq!(tail, (0, 0));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . . .
    //  0 . . # T H .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    move_towards(&mut tail, (2, 0));
    assert_eq!(tail, (1, 0));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . H .
    //  0 . . # T . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    move_towards(&mut tail, (2, 1));
    assert_eq!(tail, (1, 0));

    //  3 . . . . . .
    //  2 . . . . H .
    //  1 . . . . T .
    //  0 . . # # . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    move_towards(&mut tail, (2, 2));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . H .
    //  0 . . # # . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    move_towards(&mut tail, (2, 1));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . T .
    //  0 . . # # H .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    move_towards(&mut tail, (2, 0));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . T .
    //  0 . . # H . .
    // -1 . . . . . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    move_towards(&mut tail, (1, 0));
    assert_eq!(tail, (2, 1));

    //  3 . . . . . .
    //  2 . . . . . .
    //  1 . . . . # .
    //  0 . . # T . .
    // -1 . . . H . .
    // -2 . . . . . .
    //   -2-1 0 1 2 3
    move_towards(&mut tail, (1, -1));
    assert_eq!(tail, (1, 0));
}
