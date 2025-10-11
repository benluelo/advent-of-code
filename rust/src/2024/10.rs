use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{
        grid::{GridMut, Position},
        iter,
    },
};

#[apply(day)]
impl Day<2024, 10> {
    pub const fn parse(input: &mut [u8]) -> usize {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> usize {
        parse2(input)
    }
}

const fn parse(input: &mut [u8]) -> usize {
    let mut grid = GridMut::new(input);

    let mut total = 0;

    #[apply(iter)]
    for row in range(0, grid.rows()) {
        #[apply(iter)]
        for col in range(0, grid.cols()) {
            let pos = Position::new(row, col);

            if *grid.get(pos).unwrap() == b'0' {
                total += traverse(&mut grid, pos, TraverseMode::Mark, 0);
                traverse(&mut grid, pos, TraverseMode::Unmark, 0);
            }
        }
    }

    total
}

// technically this doesn't need &mut, but i want to resuse the traverse fn from
// part 1
const fn parse2(input: &mut [u8]) -> usize {
    let mut grid = GridMut::new(input);

    let mut total = 0;

    #[apply(iter)]
    for row in range(0, grid.rows()) {
        #[apply(iter)]
        for col in range(0, grid.cols()) {
            let pos = Position::new(row, col);

            if *grid.get(pos).unwrap() == b'0' {
                let peaks_found = traverse(&mut grid, pos, TraverseMode::Full, 0);

                total += peaks_found;
            }
        }
    }

    total
}

const VISITED_MASK: u8 = 0b0100_0000;

#[derive(Debug, Clone, Copy)]
enum TraverseMode {
    Mark,
    Unmark,
    Full,
}

const fn traverse(grid: &mut GridMut, pos: Position, mode: TraverseMode, depth: usize) -> usize {
    let curr = *grid.get(pos).unwrap() - 48;

    let target = curr + 1;

    let mut total = 0;

    if let Some(north) = pos.north()
        && let Some(pos) = grid.get_mut(north)
    {
        if depth == 8 && check_if_peak(pos, mode) {
            total += 1;
        } else if *pos - 48 == target {
            total += traverse(grid, north, mode, depth + 1);
        }
    }

    if let Some(south) = pos.south()
        && let Some(pos) = grid.get_mut(south)
    {
        if depth == 8 && check_if_peak(pos, mode) {
            total += 1;
        } else if *pos - 48 == target {
            total += traverse(grid, south, mode, depth + 1);
        }
    }

    if let Some(east) = pos.east()
        && let Some(pos) = grid.get_mut(east)
    {
        if depth == 8 && check_if_peak(pos, mode) {
            total += 1;
        } else if *pos - 48 == target {
            total += traverse(grid, east, mode, depth + 1);
        }
    }

    if let Some(west) = pos.west()
        && let Some(pos) = grid.get_mut(west)
    {
        if depth == 8 && check_if_peak(pos, mode) {
            total += 1;
        } else if *pos - 48 == target {
            total += traverse(grid, west, mode, depth + 1);
        }
    }

    total
}

const fn check_if_peak(curr: &mut u8, mode: TraverseMode) -> bool {
    if matches!(mode, TraverseMode::Unmark) && *curr == (b'9' | VISITED_MASK) {
        *curr &= !VISITED_MASK;
        true
    } else if matches!(mode, TraverseMode::Mark) && *curr == b'9' {
        *curr |= VISITED_MASK;
        true
    } else {
        matches!(mode, TraverseMode::Full) && *curr == b'9'
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
";

    dbg!(parse2(&mut input.as_bytes().to_owned()));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
