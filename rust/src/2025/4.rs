//! Quite a simple solution, compiles nearly instantly as well (it's hard to
//! tell how much time is actually spent in the const evaluator when the
//! solutions are this fast).

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{
        grid::{Direction, GridMut},
        iter,
    },
};

#[apply(day)]
impl Day<2025, 4> {
    pub const fn parse(input: &mut [u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &mut [u8]) -> u32 {
    let mut grid = GridMut::new(input);

    remove_accessible(&mut grid)
}

const fn parse2(input: &mut [u8]) -> u32 {
    let mut grid = GridMut::new(input);

    let mut total = 0;

    loop {
        let removed = remove_accessible(&mut grid);

        if removed == 0 {
            break;
        }

        total += removed;
    }

    total
}

const fn remove_accessible(grid: &mut GridMut<'_>) -> u32 {
    let mut total = 0;

    #[apply(iter)]
    'outer: for (pos, tile) in grid(grid) {
        if tile == b'@' {
            let mut adjacent_rolls = 0;

            #[apply(iter)]
            for direction in iter([
                Direction::North,
                Direction::East,
                Direction::South,
                Direction::West,
            ]) {
                if let Some(pos) = pos.direction(*direction)
                    && let Some(tile) = grid.get(pos)
                {
                    if *tile == b'@' || *tile == b'x' {
                        adjacent_rolls += 1;
                        if adjacent_rolls >= 4 {
                            continue 'outer;
                        }
                    }

                    if let Some(pos) = pos.direction(direction.clockwise())
                        && let Some(b'@' | b'x') = grid.get(pos)
                    {
                        adjacent_rolls += 1;
                        if adjacent_rolls >= 4 {
                            continue 'outer;
                        }
                    }
                }
            }

            total += 1;

            *grid.get_mut(pos).unwrap() = b'x';
        }
    }

    #[apply(iter)]
    for (_, tile) in grid_mut(grid) {
        if *tile == b'x' {
            *tile = b'.';
        }
    }

    total
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
";

    assert_eq!(parse(&mut input.as_bytes().to_vec()), 13);
    assert_eq!(parse2(&mut input.as_bytes().to_vec()), 43);

    assert_eq!(parse(&mut Today::INPUT.as_bytes().to_vec()), 1424);
    assert_eq!(parse2(&mut Today::INPUT.as_bytes().to_vec()), 8727);

    // dbg!(parse(&mut input.as_bytes().to_vec()));
    // dbg!(parse(&mut Today::INPUT.as_bytes().to_vec()));
}
