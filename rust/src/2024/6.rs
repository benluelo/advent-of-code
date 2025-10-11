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
    Day, day,
    utils::{
        grid::{GridMut, Position},
        iter, read_until,
    },
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
    let mut total = 0;

    let mut map = Map::new(input);

    while !matches!(map.step(), StepResult::Exit) {}

    #[apply(iter)]
    for pos in iter(input) {
        // if this tile was visited in any direction, consider it visited
        if *pos & VISITED_MASK > 0 {
            total += 1;
        }
    }

    total
}

const fn parse2(input: &mut [u8]) -> u32 {
    // first, solve the normal case to see which tiles the guard will traverse. only
    // obstacles placed on these tiles will have an effect on the guard's path, so
    // there's no point in checking *every* tile - only check these ones.
    let mut map = Map::new(input);
    let start_pos = map.guard_pos;

    while !matches!(map.step(), StepResult::Exit) {}

    #[apply(iter)]
    for c in iter_mut(map.grid.raw_mut()) {
        // if this tile was visited in any direction, consider it visited and mark it as
        // a possible obstacle location
        if *c & VISITED_MASK > 0 {
            *c = TILE | POSSIBLE_OBSTACLE_LOCATION;
        }
    }

    map.reset(start_pos);

    let mut total = 0;

    #[apply(iter)]
    for i in range(0, map.grid.raw().len()) {
        if map.grid.raw()[i] == POSSIBLE_OBSTACLE_LOCATION {
            map.grid.raw_mut()[i] = WALL;

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
            map.grid.raw_mut()[i] = TILE;
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
    grid: GridMut<'a>,
    guard_pos: Position,
    direction: Direction,
}

impl<'a> Map<'a> {
    const fn new(map: &'a mut [u8]) -> Self {
        let mut grid = GridMut::new(map);

        let guard_pos_raw = read_until(grid.raw(), 0, b"^").len();

        // apply the mask to all non-newline positions
        #[apply(iter)]
        for c in iter_mut(grid.raw_mut()) {
            if *c != b'\n' {
                *c &= MASK;
            }
        }

        let mut this = Self {
            guard_pos: Position::new(
                guard_pos_raw / grid.rows() - 1,
                guard_pos_raw % (grid.cols() + 1),
            ),
            grid,
            direction: Direction::North,
        };

        *this.grid.get_mut(this.guard_pos).unwrap() = TILE | this.direction as u8;

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
                self.guard_pos.row().checked_add_signed(delta.0),
                self.guard_pos.col().checked_add_signed(delta.1),
            ) {
                (Some(row), Some(col)) => Position::new(row, col),
                _ => return StepResult::Exit,
            }
        };

        // partial borrows would be nice

        let dir_mask = self.direction as u8;

        let res = match self.grid.get_mut(new_pos) {
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

    const fn reset(&mut self, guard_pos: Position) {
        // un-visit all positions by unsetting the top 4 bits of every byte (where the
        // direction information is stored)
        #[apply(iter)]
        for c in iter_mut(self.grid.raw_mut()) {
            // newline is 0b00001010, so no need to check here as none of it's bits will get
            // overwritten by the mask; it's more efficient to just unconditinally apply the
            // mask (almost 10% faster than branching)
            *c &= (!VISITED_MASK) | (*c & POSSIBLE_OBSTACLE_LOCATION);
        }

        self.guard_pos = guard_pos;
        self.direction = Direction::North;
        *self.grid.get_mut(guard_pos).unwrap() = TILE | Direction::North as u8;
    }

    #[allow(unused, reason = "only used in debugging")]
    fn debug(&self) {
        self.grid.debug(|b| {
            if b == b'\n' {
                b as char
            } else if b == POSSIBLE_OBSTACLE_LOCATION {
                'O'
            } else if b & MASK == TILE {
                if b & VISITED_MASK > 0 { 'X' } else { '.' }
            } else {
                '#'
            }
        });
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

    dbg!(parse2(&mut input.to_owned()));
    dbg!(parse(&mut Today::INPUT.as_bytes().to_owned()));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
