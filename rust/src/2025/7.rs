//! A classic advent of code O^n problem - brute forcing part 2 simply isn't
//! possible, so I had to resort to using a cache. That being said, both parts
//! compile extremely quickly (~1-2s).

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{
        grid::{Direction, Grid, GridMut, Position},
        iter, read_until,
    },
};

#[apply(day)]
impl Day<2025, 7> {
    pub const fn parse(input: &mut [u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

const fn parse(input: &mut [u8]) -> u32 {
    const fn step(grid: &mut GridMut<'_>, from: Position) {
        let pos = from.direction(Direction::South).unwrap();
        match grid.get(pos) {
            Some(b'.') => {
                *grid.get_mut(pos).unwrap() = b'|';
                step(grid, pos);
            }
            Some(b'^') => {
                *grid.get_mut(pos).unwrap() = b'H';
                *grid
                    .get_mut(pos.direction(Direction::East).unwrap())
                    .unwrap() = b'|';
                *grid
                    .get_mut(pos.direction(Direction::West).unwrap())
                    .unwrap() = b'|';
                step(grid, pos.direction(Direction::East).unwrap());
                step(grid, pos.direction(Direction::West).unwrap());
            }
            Some(b'|' | b'H') | None => {}
            _ => panic!(),
        }
    }

    let start = Position::new(0, read_until(input, 0, b"S").len());

    let mut grid = GridMut::new(input);

    step(&mut grid, start);

    let mut total = 0;

    #[apply(iter)]
    for (_, tile) in grid(grid) {
        if tile == b'H' {
            total += 1
        }
    }

    total
}

const CACHE_SIZE: usize = 50000;

const fn parse2(input: &[u8]) -> u64 {
    const fn step(grid: &Grid<'_>, from: Position, cache: &mut [Option<u64>; CACHE_SIZE]) -> u64 {
        let pos = from.direction(Direction::South).unwrap();

        match grid.get(pos) {
            Some(b'.') => step(grid, pos, cache),
            Some(b'^') => {
                if let Some(res) = cache[pos.pair()] {
                    return res;
                }

                let res = step(grid, pos.direction(Direction::East).unwrap(), cache)
                    + step(grid, pos.direction(Direction::West).unwrap(), cache);

                cache[pos.pair()] = Some(res);

                res
            }
            None => 1,
            _ => panic!(),
        }
    }

    let start = Position::new(0, read_until(input, 0, b"S").len());

    let grid = Grid::new(input);

    #[allow(clippy::large_stack_arrays)]
    let mut cache = [None; CACHE_SIZE];

    step(&grid, start, &mut cache)
}

#[cfg(test)]
#[test]
fn test() {
    let input = b".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
";

    assert_eq!(parse(&mut input.to_vec()), 21);
    assert_eq!(parse(&mut Today::INPUT.as_bytes().to_vec()), 1622);

    assert_eq!(parse2(input), 40);
    assert_eq!(parse2(Today::INPUT.as_bytes()), 10_357_305_916_520);
}
