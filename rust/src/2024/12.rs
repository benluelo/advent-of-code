//! Pretty straightforward geometric problem, finding the edges, perimeter, and
//! area of a polygon. The trick to solving part 2 is that the amount of edges
//! in a polygon is always equal to the amount of corners, and in the context of
//! this problem, counting corners is much easier than counting edges.

use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{
        grid::{Direction, GridMut, Position},
        iter,
    },
};

#[apply(day)]
impl Day<2024, 12> {
    pub const fn parse(input: &mut [u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> u32 {
        parse2(input)
    }
}

const VISITED_MASK: u8 = 0b1000_0000;

const fn parse(input: &mut [u8]) -> u32 {
    let mut farm = GridMut::new(input);

    let mut total = 0;

    #[apply(iter)]
    for row in range(0, farm.rows()) {
        #[apply(iter)]
        for col in range(0, farm.cols()) {
            let pos = Position::new(row, col);
            let plot = farm.get(pos).unwrap();

            if *plot & !VISITED_MASK == *plot {
                let plot = *plot;
                total += explore_region(&mut farm, pos, plot).cost();
            }
        }
    }

    total
}

const fn parse2(input: &mut [u8]) -> u32 {
    let mut farm = GridMut::new(input);

    let mut total = 0;

    #[apply(iter)]
    for row in range(0, farm.rows()) {
        #[apply(iter)]
        for col in range(0, farm.cols()) {
            let pos = Position::new(row, col);
            let plot = farm.get(pos).unwrap();

            if *plot & !VISITED_MASK == *plot {
                let plot = *plot;
                total += explore_region(&mut farm, pos, plot).bulk_cost();
            }
        }
    }

    total
}

const fn explore_region(grid: &mut GridMut, from: Position, plot: u8) -> PlotInfo {
    *grid.get_mut(from).unwrap() |= VISITED_MASK;

    PlotInfo {
        perimiter: 0,
        corners: 0,
        area: 1,
    }
    .add(&explore_plot(from, Direction::North, grid, plot))
    .add(&explore_plot(from, Direction::East, grid, plot))
    .add(&explore_plot(from, Direction::South, grid, plot))
    .add(&explore_plot(from, Direction::West, grid, plot))
}

const fn explore_plot(
    from: Position,
    direction: Direction,
    grid: &mut GridMut<'_>,
    plot: u8,
) -> PlotInfo {
    if let Some(pos) = from.direction(direction)
        && let Some(p) = grid.get_mut(pos)
    {
        if *p == plot {
            explore_region(grid, pos, plot)
        } else if *p == plot | VISITED_MASK {
            // already visited this plot in this region
            PlotInfo {
                perimiter: 0,
                corners: 0,
                area: 0,
            }
        } else {
            PlotInfo {
                perimiter: 1,
                corners: check_for_corner(from, direction, grid, plot) as u32,
                area: 0,
            }
        }
    } else {
        // the edge of the grid is considered part of the perimeter
        PlotInfo {
            perimiter: 1,
            corners: check_for_corner(from, direction, grid, plot) as u32,
            area: 0,
        }
    }
}

#[derive(Debug)]
pub struct PlotInfo {
    perimiter: u32,
    corners: u32,
    area: u32,
}

impl PlotInfo {
    const fn add(self, other: &Self) -> Self {
        Self {
            perimiter: self.perimiter + other.perimiter,
            corners: self.corners + other.corners,
            area: self.area + other.area,
        }
    }

    const fn cost(self) -> u32 {
        self.perimiter * self.area
    }

    const fn bulk_cost(self) -> u32 {
        self.corners * self.area
    }
}

const fn check_for_corner(
    from: Position,
    direction: Direction,
    grid: &GridMut<'_>,
    plot: u8,
) -> bool {
    // acute corner
    //
    // +-+
    //  ^|
    if let Some(pos) = from.direction(direction.clockwise())
        && let Some(p) = grid.get(pos)
    {
        // if there's a plot in this space and it's *not* the same as the plot we're
        // currently in, this is an acute corner
        if *p & !VISITED_MASK == plot {
            // if the plot is the same as the plot we're starting from, then this is either
            // a straight wall or an obtuse corner
            //
            //   |
            // +-+
            //  ^
            if let Some(pos) = pos.direction(direction)
                && let Some(p) = grid.get(pos)
                && *p & !VISITED_MASK == plot
            {
                // if there's a plot in this space and it *is* the same as the plot we're
                // currently in, this is an obtuse corner
                true
            } else {
                // otherwise, this is a straight line
                false
            }
        } else {
            true
        }
    } else {
        true
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
";

    dbg!(parse2(&mut input.as_bytes().to_owned()));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
