use crate::{
    const_helpers::{arr, iter, itoa, opt_unwrap, read_until, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 10> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
    // const PART_1: &'static str = "";
    // const PART_2: &'static str = "";
}

#[allow(long_running_const_eval)]
const SOLUTION_PART_1: u32 = parse(&mut arr!(Day::<2023, 10>::INPUT.as_bytes()));
#[allow(long_running_const_eval)]
const SOLUTION_PART_2: u32 = parse2(&mut arr!(Day::<2023, 10>::INPUT.as_bytes()));

const GROUND: u8 = b'.';

#[test]
fn parse_test() {
    let mut input = *b"\
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
";

    let mut input = *b"\
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
";

    let mut input = *b"\
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
";

    dbg!(parse2(&mut input));

    dbg_input(&input);

    let mut input = Day::<2023, 10>::INPUT.as_bytes().to_vec();

    dbg!(parse2(&mut input));

    dbg_input(&input);
    // dbg!(parse(input2));
    // dbg!(parse2(input));

    for char in [b'F', b'7', b'-', b'|', b'J', b'L', b'.'] {
        println!("{} = {char:08b} ({:08b})", utf8(&[char]), char | PATH_MASK);
        println!("{}", (char & PATH_MASK) == PATH_MASK);
    }
}

const PATH_MASK: u8 = 0b1000_0000;

const fn parse(input: &mut [u8]) -> u32 {
    // + 1 to include newline
    let line_len = read_until(input, 0, b"\n").len() + 1;
    let start_pos = read_until(input, 0, b"S").len();

    let mut next = start(line_len, input, start_pos);

    let init = next;

    let mut count: u32 = 0;

    loop {
        count += 1;

        input[next.1] |= PATH_MASK;

        match next.0 {
            Direction::North => {
                if input[next.1 - line_len] == b'S' {
                    break;
                }
                next = (
                    opt_unwrap!(north(input[next.1 - line_len])),
                    next.1 - line_len,
                );
            }
            Direction::East => {
                if input[next.1 + 1] == b'S' {
                    break;
                }
                next = (opt_unwrap!(east(input[next.1 + 1])), next.1 + 1);
            }
            Direction::South => {
                if input[next.1 + line_len] == b'S' {
                    break;
                }
                next = (
                    opt_unwrap!(south(input[next.1 + line_len])),
                    next.1 + line_len,
                );
            }
            Direction::West => {
                if input[next.1 - 1] == b'S' {
                    break;
                }
                next = (opt_unwrap!(west(input[next.1 - 1])), next.1 - 1);
            }
        }
    }

    input[start_pos] = match (next.0, (init.1 as isize) - (start_pos as isize)) {
        //  i
        // iSi
        //  ^
        //  |
        (Direction::North, 1) => b'F',
        (Direction::North, -1) => b'7',
        (Direction::North, x) if x == line_len as isize => b'|',

        //  i
        // iS<-
        //  i
        (Direction::East, -1) => b'-',
        (Direction::East, x) if x == -(line_len as isize) => b'L',
        (Direction::East, x) if x == line_len as isize => b'F',

        //  |
        //  v
        // iSi
        //  i
        (Direction::South, 1) => b'L',
        (Direction::South, -1) => b'J',
        (Direction::South, x) if x == line_len as isize => b'|',

        //   i
        // ->Si
        //   i
        (Direction::West, 1) => b'-',
        (Direction::West, x) if x == -(line_len as isize) => b'J',
        (Direction::West, x) if x == line_len as isize => b'7',

        _ => panic!(),
    } | PATH_MASK;

    count.div_ceil(2)
}

const fn parse2(input: &mut [u8]) -> u32 {
    let _ = parse(input);

    // + 1 to include newline
    let line_len = read_until(input, 0, b"\n").len() + 1;

    let mut count = 0;

    iter! {
        for line in lines(input) {
            let mut north = false;
            let mut south = false;

            let mut in_shape = false;

            iter! {
                for char in line {
                    if (char & PATH_MASK) == PATH_MASK {
                        match char & !PATH_MASK {
                            b'7' => {
                                // either an F or L has been hit
                                assert!(north || south);

                                in_shape = !(in_shape ^ south);
                                north = false;
                                south = false;
                            }
                            b'J' => {
                                // either an F or L has been hit
                                assert!(north || south);

                                in_shape = !(in_shape ^ north);
                                north = false;
                                south = false;
                            }
                            b'F' => {
                                assert!(!north && !south);
                                south = true;
                            }
                            b'L' => {
                                assert!(!north && !south);
                                north = true;
                            }
                            b'|' => {
                                assert!(!north && !south);
                                in_shape = !in_shape;
                            }
                            _ => {}
                        };
                    } else {
                        count += in_shape as u32;
                    }
                }
            }
        }
    }

    count
}

const fn start(line_len: usize, input: &[u8], pos: usize) -> (Direction, usize) {
    if pos + 1 < input.len()
        && let Some(dir) = east(input[pos + 1])
    {
        return (dir, pos + 1);
    }

    if pos < 0
        && let Some(dir) = west(input[pos - 1])
    {
        return (dir, pos - 1);
    }

    if pos + line_len < input.len()
        && let Some(dir) = south(input[pos + line_len])
    {
        return (dir, pos - 1);
    }

    if pos >= line_len
        && let Some(dir) = north(input[pos - line_len])
    {
        return (dir, pos - line_len);
    }

    panic!();
}

const fn south(b: u8) -> Option<Direction> {
    // |
    // v
    match b {
        b'L' => Some(Direction::East),
        b'|' => Some(Direction::South),
        b'J' => Some(Direction::West),
        _ => None,
    }
}

const fn east(b: u8) -> Option<Direction> {
    // ->
    match b {
        b'7' => Some(Direction::South),
        b'-' => Some(Direction::East),
        b'J' => Some(Direction::North),
        _ => None,
    }
}

const fn north(b: u8) -> Option<Direction> {
    // ^
    // |
    match b {
        b'F' => Some(Direction::East),
        b'|' => Some(Direction::North),
        b'7' => Some(Direction::West),
        _ => None,
    }
}

const fn west(b: u8) -> Option<Direction> {
    // <-
    match b {
        b'L' => Some(Direction::North),
        b'-' => Some(Direction::West),
        b'F' => Some(Direction::South),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[cfg(test)]
fn dbg_input(input: &[u8]) {
    let out = input
        .iter()
        .map(|char| {
            if (char & PATH_MASK) == PATH_MASK {
                match char & !PATH_MASK {
                    b'7' => '┐',
                    b'J' => '┘',
                    b'F' => '┌',
                    b'L' => '└',
                    b'|' => '│',
                    b'-' => '─',
                    c => panic!("{}", c as char),
                }
            } else {
                *char as char
            }
        })
        .collect::<String>();

    println!("{out}");
}
