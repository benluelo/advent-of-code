use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{
        array::ArrayVec,
        grid::{GridMut, Position},
        iter, option_try,
    },
};

#[apply(day)]
impl Day<2024, 8> {
    pub const fn parse(input: &mut [u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &mut [u8]) -> u32 {
    let (mut grid, antennas) = setup(input);

    let mut total = 0;

    #[apply(iter)]
    for antenna_type in iter(&antennas.locations) {
        let len = antenna_type.len();

        #[apply(iter)]
        for i in range(0, len) {
            #[apply(iter)]
            for j in range(i + 1, len) {
                let i = antenna_type.get(i);
                let j = antenna_type.get(j);

                let slope = Slope::from_positions(*i, *j);

                if let Some(pos) = slope.apply_to_position(*i)
                    && let Some(c) = grid.get_mut(pos)
                {
                    apply_mask(c, &mut total);
                }

                if let Some(pos) = slope.neg().apply_to_position(*j)
                    && let Some(c) = grid.get_mut(pos)
                {
                    apply_mask(c, &mut total);
                }
            }
        }
    }

    total
}

const fn parse2(input: &mut [u8]) -> u32 {
    let (mut grid, antennas) = setup(input);

    let mut total = 0;

    #[apply(iter)]
    for antenna_type in iter(&antennas.locations) {
        let len = antenna_type.len();

        #[apply(iter)]
        for i in range(0, len) {
            #[apply(iter)]
            for j in range(i + 1, len) {
                let mut i = *antenna_type.get(i);
                let mut j = *antenna_type.get(j);

                let slope = Slope::from_positions(i, j);

                {
                    let c = grid.get_mut(i).unwrap();

                    apply_mask(c, &mut total);
                }
                {
                    let c = grid.get_mut(j).unwrap();

                    apply_mask(c, &mut total);
                }

                while let Some(pos) = slope.apply_to_position(i)
                    && let Some(c) = grid.get_mut(pos)
                {
                    i = pos;
                    apply_mask(c, &mut total);
                }

                while let Some(pos) = slope.neg().apply_to_position(j)
                    && let Some(c) = grid.get_mut(pos)
                {
                    j = pos;

                    apply_mask(c, &mut total);
                }
            }
        }
    }

    total
}

const fn setup(input: &mut [u8]) -> (GridMut<'_>, AntennaPositions) {
    let grid = GridMut::new(input);

    let mut antennas = AntennaPositions::new();

    #[apply(iter)]
    for row in range(0, grid.rows()) {
        #[apply(iter)]
        for col in range(0, grid.cols()) {
            let pos = Position::new(row, col);
            let letter = *grid.get(pos).unwrap();
            if letter != b'.' {
                antennas.push_antenna(letter, pos);
            }
        }
    }

    (grid, antennas)
}

pub const LOCATIONS_COUNT: usize = 26 + 26 + 10;
pub const ANTINODE_MASK: u8 = 0b1000_0000;

const fn apply_mask(c: &mut u8, total: &mut u32) {
    assert!(*c != b'\n');

    if *c < ANTINODE_MASK {
        *total += 1;
        *c |= ANTINODE_MASK;
    }
}

#[derive(Debug)]
struct AntennaPositions {
    pub locations: [ArrayVec<Position, 4>; LOCATIONS_COUNT],
}

impl AntennaPositions {
    const fn new() -> Self {
        Self {
            locations: [const { ArrayVec::new() }; LOCATIONS_COUNT],
        }
    }

    const fn push_antenna(&mut self, letter: u8, position: Position) {
        let idx = match letter {
            b'0'..=b'9' => letter - 48,
            b'A'..=b'Z' => letter - 55,
            b'a'..=b'z' => letter - 61,
            _ => panic!(),
        } as usize;

        self.locations[idx].push(position);
    }
}

#[derive(Clone, Copy)]
struct Slope {
    row: isize,
    col: isize,
}

impl Slope {
    #[allow(clippy::cast_possible_wrap)]
    const fn from_positions(i: Position, j: Position) -> Slope {
        Slope {
            row: i.row() as isize - j.row() as isize,
            col: i.col() as isize - j.col() as isize,
        }
    }

    const fn apply_to_position(&self, pos: Position) -> Option<Position> {
        let row = option_try!(pos.row().checked_add_signed(self.row));
        let col = option_try!(pos.col().checked_add_signed(self.col));

        Some(Position::new(row, col))
    }

    const fn neg(self) -> Self {
        Self {
            row: -self.row,
            col: -self.col,
        }
    }
}

impl core::fmt::Debug for Slope {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "({},{})", self.row, self.col)
    }
}

#[allow(unused)]
fn debug_grid(c: u8) -> String {
    if c & ANTINODE_MASK == ANTINODE_MASK {
        format!("\x1B[7m{}\x1B[0m", (c & !ANTINODE_MASK) as char)
    } else {
        (c as char).to_string()
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
";

    dbg!(parse(&mut input.as_bytes().to_owned()));
    dbg!(parse(&mut Today::INPUT.as_bytes().to_owned()));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
