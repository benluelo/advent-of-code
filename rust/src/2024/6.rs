//! This problem is quite simple, however due to the constraints of const it is
//! also quite a pain. All state must be kept in the input itself (hence `&mut
//! [u8]`), which is accomplished through some fancy bit twiddling:
//!
//! - all non-newline bytes are masked with [`MASK`]. we assume that the input
//!   is valid, and thus only contains `.` or `#` (or `^`, but this is
//!   overwritten as a [`TILE`] that has been visited going north during map
//!   construction)
//! - as the tiles are traversed, they are marked with the direction they were
//!   visited in. The top 4 bits of each tile are reserved for direction
//!   marking, as all 4 directions need to be potentially marked.
//! - for part 1, all visited tiles are counted for the solution. for part 2,
//!   the solution to part 1 is run, and then all visited tiles are marked as a
//!   [`POSSIBLE_OBSTACLE_LOCATION`]. this reduces the amount of tiles that need
//!   to be checked by about 3.3x (there are 16040 empty tiles in my input, and
//!   4778 are visited during the part 1 solution)
//!
//! A faster solution would be to solve part 1, and then backtrace through the
//! solution, placing an obstacle at the end of the path at each step backwards,
//! and then continuing stepping forwards with the obstacle there. Upon either
//! exiting the map (no loop found) or encountering a loop, step backwards to
//! the obstacle, then step back one more on the original path, and repeat until
//! having stepped back through the entire traversed path.

use cfg_proc::apply;

use crate::{
    day,
    utils::{count_segments, iter, read_until, trim_ascii_mut},
    Day,
};

#[apply(day)]
impl Day<2024, 6> {
    pub const fn parse(input: &mut [u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &mut [u8]) -> u32 {
    let input = trim_ascii_mut(input);

    let mut total = 0;

    let mut map = Map::new(input);

    while !matches!(map.step(), StepResult::Exit) {}

    #[apply(iter)]
    for pos in input {
        // if this tile was visited in any direction, consider it visited
        if pos & VISITED_MASK > 0 {
            total += 1;
        }
    }

    total
}

const fn parse2(input: &mut [u8]) -> u32 {
    let input = trim_ascii_mut(input);

    // first, solve the normal case to see which tiles the guard will traverse. only
    // obstacles placed on these tiles will have an effect on the guard's path, so
    // there's no point in checking *every* tile - only check these ones.
    let mut map = Map::new(input);
    let start_pos = map.guard_pos;

    while !matches!(map.step(), StepResult::Exit) {}

    #[apply(iter)]
    for i in range(0, map.map.len()) {
        // if this tile was visited in any direction, consider it visited and mark it as
        // a possible obstacle location
        if map.map[i] & VISITED_MASK > 0 {
            map.map[i] = TILE | POSSIBLE_OBSTACLE_LOCATION;
        }
    }

    map.reset(start_pos);

    let mut total = 0;

    #[apply(iter)]
    for i in range(0, map.map.len()) {
        if map.map[i] == POSSIBLE_OBSTACLE_LOCATION {
            map.map[i] = WALL;

            loop {
                match map.step() {
                    StepResult::Exit => break,
                    StepResult::NewlyVisited => {}
                    StepResult::Loop => {
                        total += 1;
                        break;
                    }
                }
            }

            // no need to set it as a possible obstacle location as it's already been
            // visited
            map.map[i] = TILE;
            map.reset(start_pos);
        }
    }

    total
}

const VISITED_MASK: u8 = 0b1111_0000;
const MASK: u8 = 0b0000_0001;
// this is explicitly chosen so as to not have any overlap with the bit pattern
// of b'\n'
const POSSIBLE_OBSTACLE_LOCATION: u8 = 0b0000_0100;
const WALL: u8 = b'#' & MASK;
const TILE: u8 = b'.' & MASK;

#[derive(Debug)]
struct Map<'a> {
    map: &'a mut [u8],
    rows: usize,
    cols: usize,
    guard_pos: (usize, usize),
    direction: Direction,
}

impl<'a> Map<'a> {
    const fn new(map: &'a mut [u8]) -> Self {
        let rows = count_segments::<b'\n', true>(map);
        let cols = read_until(map, 0, b"\n").len();

        let guard_pos = read_until(map, 0, b"^").len();

        // apply the mask to all non-newline positions
        #[apply(iter)]
        for i in range(0, map.len()) {
            if map[i] != b'\n' {
                map[i] &= MASK;
            }
        }

        let mut this = Self {
            map,
            rows,
            cols,
            guard_pos: ((guard_pos / rows) - 1, guard_pos % (cols + 1)),
            direction: Direction::North,
        };

        *this.get(this.guard_pos.0, this.guard_pos.1).unwrap() = TILE | this.direction as u8;

        this
    }

    const fn step(&mut self) -> StepResult {
        let delta = match self.direction {
            Direction::North => (-1, 0),
            Direction::East => (0, 1),
            Direction::South => (1, 0),
            Direction::West => (0, -1),
        };

        let mut new_pos = {
            match (
                self.guard_pos.0.checked_add_signed(delta.0),
                self.guard_pos.1.checked_add_signed(delta.1),
            ) {
                (Some(row), Some(col)) => (row, col),
                _ => return StepResult::Exit,
            }
        };

        // partial borrows would be nice

        let dir_mask = self.direction as u8;

        let res = match self.get(new_pos.0, new_pos.1) {
            Some(&mut WALL) => {
                self.direction = self.direction.turn_right();
                new_pos = self.guard_pos;
                StepResult::NewlyVisited
            }
            Some(pos) => {
                if dir_mask & *pos == dir_mask {
                    StepResult::Loop
                } else {
                    *pos |= dir_mask;
                    StepResult::NewlyVisited
                }
            }
            None => StepResult::Exit,
        };

        if !matches!(res, StepResult::Exit) {
            self.guard_pos = new_pos;
        }

        res
    }

    #[track_caller]
    const fn get(&mut self, row: usize, col: usize) -> Option<&mut u8> {
        if row <= self.rows && col <= self.cols {
            Some(&mut self.map[(row * (self.cols + 1)) + col])
        } else {
            None
        }
    }

    const fn reset(&mut self, guard_pos: (usize, usize)) {
        // un-visit all positions by unsetting the top 4 bits of every byte (where the
        // direction information is stored)
        #[apply(iter)]
        for i in range(0, self.map.len()) {
            // newline is 0b00001010, so no need to check here as none of it's bits will get
            // overwritten by the mask; it's more efficient to just unconditinally apply the
            // mask (almost 10% faster than branching)
            self.map[i] &= (!VISITED_MASK) | (self.map[i] & POSSIBLE_OBSTACLE_LOCATION);
        }

        self.guard_pos = guard_pos;
        self.direction = Direction::North;
        *self.get(guard_pos.0, guard_pos.1).unwrap() = TILE | Direction::North as u8;
    }

    #[allow(unused, reason = "only used in debugging")]
    fn debug(&self) {
        let s = self
            .map
            .iter()
            .map(|b| {
                if *b == b'\n' {
                    *b as char
                } else if *b == POSSIBLE_OBSTACLE_LOCATION {
                    'O'
                } else if b & MASK == TILE {
                    if b & VISITED_MASK > 0 {
                        'X'
                    } else {
                        '.'
                    }
                } else {
                    '#'
                }
            })
            .collect::<String>();

        println!("{s}\n");
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[rustfmt::skip]
enum Direction {
    North = 0b1000_0000,
    East  = 0b0100_0000,
    South = 0b0010_0000,
    West  = 0b0001_0000,
}

impl Direction {
    const fn turn_right(self) -> Direction {
        match self {
            Self::North => Self::East,
            Self::East => Self::South,
            Self::South => Self::West,
            Self::West => Self::North,
        }
    }
}

enum StepResult {
    /// Exited the map.
    Exit,
    /// Visited a tile from a new direction.
    NewlyVisited,
    /// Visited a tile from a direction that it has already been visited from
    /// before, thus a loop has been encountered in the map.
    Loop,
}

#[cfg(test)]
#[test]
fn test() {
    let input = b"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...";

    //     let input = b"123456789
    // abcdefghi
    // ABCDEFGHI";

    // dbg!(input[83] as char);

    // dbg!(parse2(&mut input.to_owned()));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
