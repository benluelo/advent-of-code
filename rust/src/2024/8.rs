use cfg_proc::apply;

use crate::{
    day,
    utils::{array::ArrayVec, count_segments, iter, option_try, read_until, trim_ascii_mut},
    Day,
};

#[apply(day)]
impl Day<2024, 8> {
    pub const fn parse(input: &[u8]) -> u32 {
        todo!()
        // parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        todo!()
        // parse2(input)
    }
}

fn parse(input: &mut [u8]) -> u32 {
    let input = trim_ascii_mut(input);

    let rows = count_segments::<b'\n', false>(input);
    let cols = read_until(input, 0, b"\n").len();

    let mut antennas = AntennaPositions::new();

    dbg!(rows, cols);

    #[apply(iter)]
    for row in range(0, rows) {
        #[apply(iter)]
        for col in range(0, cols) {
            let letter = input[Position { row, col }.mk_idx(cols)];
            if letter != b'.' {
                antennas.push_antenna(letter, Position { row, col });
            }
        }
    }

    dbg!(&antennas);

    let mut total = 0;

    for antenna_type in &antennas.locations {
        let len = antenna_type.len();

        // debug_input(input);

        #[apply(iter)]
        for i in range(0, len) {
            #[apply(iter)]
            for j in range(i + 1, len) {
                let i = antenna_type.get(i);
                let j = antenna_type.get(j);
                println!("{:?}", (i, j));

                let slope = i.slope(*j);
                // dbg!(slope);

                if let Some(antinode_1) = i.add_slope(slope)
                    && antinode_1.col < cols
                    && antinode_1.row < rows
                {
                    let c = &mut input[antinode_1.mk_idx(cols)];

                    assert!(*c != b'\n');

                    if *c < ANTINODE_MASK {
                        total += 1;
                        *c |= ANTINODE_MASK;
                    }
                }

                if let Some(antinode_2) = j.add_slope(slope.neg())
                    && antinode_2.col < cols
                    && antinode_2.row < rows
                {
                    let c = &mut input[antinode_2.mk_idx(cols)];

                    assert!(*c != b'\n');

                    if *c < ANTINODE_MASK {
                        total += 1;
                        *c |= ANTINODE_MASK;
                    }
                }
            }
        }
    }

    let total2 = input.iter().filter(|b| **b > ANTINODE_MASK).count();

    dbg!(total2);

    debug_input(input);

    total
}

fn parse2(input: &mut [u8]) -> u32 {
    let input = trim_ascii_mut(input);

    let rows = count_segments::<b'\n', false>(input);
    let cols = read_until(input, 0, b"\n").len();

    let mut antennas = AntennaPositions::new();

    dbg!(rows, cols);

    #[apply(iter)]
    for row in range(0, rows) {
        #[apply(iter)]
        for col in range(0, cols) {
            let letter = input[Position { row, col }.mk_idx(cols)];
            if letter != b'.' {
                antennas.push_antenna(letter, Position { row, col });
            }
        }
    }

    dbg!(&antennas);

    let mut total = 0;

    for antenna_type in &antennas.locations {
        let len = antenna_type.len();

        // debug_input(input);
        #[apply(iter)]
        for i in range(0, len) {
            #[apply(iter)]
            for j in range(i + 1, len) {
                let mut i = *antenna_type.get(i);
                let mut j = *antenna_type.get(j);
                println!("{:?}", (i, j));

                let slope = i.slope(j);
                // dbg!(slope);

                {
                    let c = &mut input[i.mk_idx(cols)];

                    if *c < ANTINODE_MASK {
                        total += 1;
                        *c |= ANTINODE_MASK;
                    }
                }
                {
                    let c = &mut input[j.mk_idx(cols)];

                    if *c < ANTINODE_MASK {
                        total += 1;
                        *c |= ANTINODE_MASK;
                    }
                }

                while let Some(antinode_1) = i.add_slope(slope)
                    && antinode_1.col < cols
                    && antinode_1.row < rows
                {
                    i = antinode_1;

                    let c = &mut input[antinode_1.mk_idx(cols)];

                    assert!(*c != b'\n');

                    if *c < ANTINODE_MASK {
                        total += 1;
                        *c |= ANTINODE_MASK;
                    }
                }

                while let Some(antinode_2) = j.add_slope(slope.neg())
                    && antinode_2.col < cols
                    && antinode_2.row < rows
                {
                    j = antinode_2;

                    let c = &mut input[antinode_2.mk_idx(cols)];

                    assert!(*c != b'\n');

                    if *c < ANTINODE_MASK {
                        total += 1;
                        *c |= ANTINODE_MASK;
                    }
                }
            }
        }
    }

    let total2 = input.iter().filter(|b| **b > ANTINODE_MASK).count();

    dbg!(total2);

    debug_input(input);

    total
}

pub const LOCATIONS_COUNT: usize = 26 + 26 + 10;
pub const ANTINODE_MASK: u8 = 0b1000_0000;

#[derive(Debug)]
struct AntennaPositions {
    pub locations: [ArrayVec<Position, 4>; LOCATIONS_COUNT],
}

impl AntennaPositions {
    fn new() -> Self {
        Self {
            locations: [const { ArrayVec::new() }; LOCATIONS_COUNT],
        }
    }

    fn push_antenna(&mut self, letter: u8, position: Position) {
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
struct Position {
    row: usize,
    col: usize,
}

impl Position {
    const fn slope(&self, other: Position) -> Slope {
        Slope {
            row: self.row as isize - other.row as isize,
            col: self.col as isize - other.col as isize,
        }
    }

    const fn add_slope(&self, slope: Slope) -> Option<Position> {
        let row = option_try!(self.row.checked_add_signed(slope.row));
        let col = option_try!(self.col.checked_add_signed(slope.col));

        Some(Position { row, col })
    }

    const fn mk_idx(&self, cols: usize) -> usize {
        (self.row * (cols + 1)) + self.col
    }
}

impl core::fmt::Debug for Position {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "({},{})", self.row, self.col)
    }
}

#[derive(Clone, Copy)]
struct Slope {
    row: isize,
    col: isize,
}

impl Slope {
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

fn debug_input(input: &[u8]) {
    let s = input
        .iter()
        .map(|c| {
            if *c & ANTINODE_MASK == ANTINODE_MASK {
                format!("\x1B[7m{}\x1B[0m", (c & !ANTINODE_MASK) as char)
            } else {
                (*c as char).to_string()
            }
        })
        .collect::<String>();

    println!("{s}\n");
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

    //     let input = "
    // abyz........
    // ABYZ....0...
    // .....012789.
    // .......0....
    // ....0.......
    // ......A.....
    // ............
    // ............
    // ........A...
    // .........A..
    // ............
    // ............
    // ";

    // dbg!(parse2(&mut input.as_bytes().to_owned()));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
