use core::fmt::{Display, Write};

use crate::{
    const_helpers::{arr, count_segments, iter, itoa, max, read_until, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 16> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
    // const PART_1: &'static str = "";
    // const PART_2: &'static str = "";
}

#[allow(long_running_const_eval)]
const SOLUTION_PART_1: u32 = parse(&mut arr!(Day::<2023, 16>::INPUT.as_bytes()));
#[allow(long_running_const_eval)]
const SOLUTION_PART_2: u32 = parse2(&mut arr!(Day::<2023, 16>::INPUT.as_bytes()));

#[test]
fn parse_test() {
    let mut input = br".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"
    .to_vec();

    let mut input = Day::<2023, 16>::INPUT.as_bytes().to_vec();

    let score = parse2(&mut input);

    println!("{score}");
}

const fn parse(input: &mut [u8]) -> u32 {
    let mut map = Map::new(input);

    // map.dbg_input();

    traverse(&mut map, 0, Direction::East);

    // map.dbg_input();

    map.count_energized()
}

const fn parse2(input: &mut [u8]) -> u32 {
    let mut map = Map::new(input);

    let mut res = 0;

    iter! {
        for i in range(0, map.map.len()) {
            if map.map[i] == b'\n' {
                continue;
            }

            if map.col(i) == 0 {
                traverse(&mut map, i, Direction::East);

                res = max(res, map.count_energized());
                map.reset();
            }

            if map.col(i) == map.cols - 1 {
                traverse(&mut map, i, Direction::West);

                res = max(res, map.count_energized());
                map.reset();
            }

            if map.row(i) == 0 {
                traverse(&mut map, i, Direction::South);

                res = max(res, map.count_energized());
                map.reset();
            }

            if map.row(i) == map.rows - 1 {
                traverse(&mut map, i, Direction::North);

                res = max(res, map.count_energized());
                map.reset();
            }
        }
    }

    res
}

struct Map<'a> {
    map: &'a mut [u8],
    rows: usize,
    cols: usize,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[rustfmt::skip]
enum Direction {
    North = 0b0000_0100,
    East  = 0b0000_1000,
    South = 0b0100_0000,
    West  = 0b1000_0000,
}

impl<'a> Map<'a> {
    const fn new(map: &'a mut [u8]) -> Self {
        iter! {
            for i in range(0, map.len()) {
                if map[i] != b'\n' {
                    map[i] &= MASK;
                }
            }
        }

        let rows = count_segments::<b'\n', true>(map);
        let cols = (read_until(map, 0, b"\n")).len();

        Self { map, rows, cols }
    }

    const fn reset(&mut self) {
        iter! {
            for i in range(0, self.map.len()) {
                if self.map[i] != b'\n' {
                    self.map[i] &= MASK;
                }
            }
        }
    }

    const fn read(&self, cursor: usize) -> Option<Tile> {
        if cursor >= 0 && cursor < self.map.len() {
            let b = self.map[cursor];
            Tile::from_byte(b)
        } else {
            None
        }
    }

    const fn energize(&mut self, cursor: usize, direction: Direction) {
        self.map[cursor] |= direction as u8
    }

    const fn is_energized(&self, cursor: usize, direction: Direction) -> bool {
        self.map[cursor] & direction as u8 == direction as u8
    }

    const fn row(&self, cursor: usize) -> usize {
        cursor.div_floor(self.cols + 1)
    }

    const fn col(&self, cursor: usize) -> usize {
        cursor % (self.cols + 1)
    }

    #[cfg(test)]
    fn dbg_input(&self) {
        let row_empty_color = "\u{001b}[46;1m";
        let col_empty_color = "\u{001b}[30;47m";
        let both_empty_color = "\u{001b}[42;1m";
        let reset = "\u{001b}[0m";

        let formatted = &self
            .map
            .chunks(self.cols + 1)
            .map(|chars| {
                chars
                    .iter()
                    .take(self.cols)
                    .map(|char| {
                        let c = Tile::from_byte(*char).unwrap();
                        (char & !MASK > 0)
                            .then_some(format!("{col_empty_color}{c}{reset}"))
                            .unwrap_or(c.to_string())
                    })
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n");

        println!("{formatted}");
    }

    const fn cursor_delta(&self, direction: Direction) -> isize {
        match direction {
            Direction::North => -((self.cols + 1) as isize),
            Direction::East => 1,
            Direction::South => (self.cols + 1) as isize,
            Direction::West => -1,
        }
    }

    const fn count_energized(&self) -> u32 {
        let mut res = 0;

        let map = &self.map;

        iter! {
            for char in map {
                res += (Tile::from_byte(char).is_some() && char & !MASK > 0) as u32;
            }
        }

        res
    }
}

const fn traverse(map: &mut Map, mut cursor: usize, mut direction: Direction) {
    use Direction::*;
    use Tile::*;

    loop {
        if cursor >= map.map.len() {
            return;
        }

        if map.is_energized(cursor, direction) {
            return;
        }

        map.energize(cursor, direction);

        enum NextDirection {
            Next(Direction),
            Split(Direction, Direction),
        }

        if let Some(tile) = map.read(cursor) {
            let next_directions = match (direction, tile) {
                (prev @ (North | South), EMPTY | VSPLIT) => NextDirection::Next(prev),
                (prev @ (East | West), EMPTY | HSPLIT) => NextDirection::Next(prev),

                (North | South, HSPLIT) => NextDirection::Split(East, West),
                (East | West, VSPLIT) => NextDirection::Split(North, South),

                (North, FMIRROR) => NextDirection::Next(East),
                (North, BMIRROR) => NextDirection::Next(West),
                (East, FMIRROR) => NextDirection::Next(North),
                (East, BMIRROR) => NextDirection::Next(South),
                (South, FMIRROR) => NextDirection::Next(West),
                (South, BMIRROR) => NextDirection::Next(East),
                (West, FMIRROR) => NextDirection::Next(South),
                (West, BMIRROR) => NextDirection::Next(North),
            };

            match next_directions {
                NextDirection::Next(next) => direction = next,
                NextDirection::Split(a, b) => {
                    direction = a;
                    traverse(map, cursor, b);
                }
            }
        } else {
            return;
        };

        cursor = match cursor.checked_add_signed(map.cursor_delta(direction)) {
            Some(c) => c,
            None => return,
        };
    }
}

const MASK: u8 = 0b0011_0011;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
#[rustfmt::skip]
pub enum Tile {
    EMPTY   = b'.' & MASK,
    VSPLIT  = b'|' & MASK,
    HSPLIT  = b'-' & MASK,
    FMIRROR = b'/' & MASK,
    BMIRROR = b'\\' & MASK,
}

impl Display for Tile {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_char(match self {
            Tile::EMPTY => '.',
            Tile::VSPLIT => '|',
            Tile::HSPLIT => '-',
            Tile::FMIRROR => '/',
            Tile::BMIRROR => '\\',
        })
    }
}

impl Tile {
    const fn from_byte(b: u8) -> Option<Self> {
        const EMPTY: u8 = b'.' & MASK;
        const VSPLIT: u8 = b'|' & MASK;
        const HSPLIT: u8 = b'-' & MASK;
        const FMIRROR: u8 = b'/' & MASK;
        const BMIRROR: u8 = b'\\' & MASK;

        match b & MASK {
            EMPTY => Some(Self::EMPTY),
            VSPLIT => Some(Self::VSPLIT),
            HSPLIT => Some(Self::HSPLIT),
            FMIRROR => Some(Self::FMIRROR),
            BMIRROR => Some(Self::BMIRROR),
            _ => None,
        }
    }
}
