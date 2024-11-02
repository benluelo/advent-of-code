use core::{
    fmt::{Display, Write},
    mem,
};

use cfg_proc::apply;

use crate::{
    const_helpers::{count_segments, iter, max, min, option_try, read_until},
    day, Day,
};

#[apply(day)]
impl Day<2023, 17> {
    pub const fn parse(input: &mut [u8]) -> u32 {
        // parse(input)
        todo!()
    }
    pub const fn parse2(input: &mut [u8]) -> u32 {
        parse2(input)
    }
}

#[test]
fn parse_test() {
    let mut input = b"\
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
"
    .to_vec();

    //     let mut input = b"\
    // 24134324
    // 32154532
    // 32552432
    // 24134324
    // 32154532
    // 32552432
    // "
    //     .to_vec();

    let mut input = Day::<2023, 17>::INPUT.as_bytes().to_vec();

    let score = parse(&mut input);

    println!("{score}");
}

fn parse(input: &mut [u8]) -> u32 {
    let mut map = Map::new(input);

    let mut state = State {
        map: &mut map,
        lowest_heat_loss_so_far: Some(3433),
        current_heat_loss: 0,
        past_directions: PastDirections::None,
    };

    dbg!(&state);

    dfs(&mut state, Position { x: 0, y: 0 }, 0);

    dbg!(&state);

    state.lowest_heat_loss_so_far.unwrap()
}

const fn parse2(input: &mut [u8]) -> u32 {
    // let mut map = Map::new(input);

    let mut res = 0;

    // #[apply(iter)]
    // for i in range(0, map.map.len()) {
    //     if map.map[i] == b'\n' {
    //         continue;
    //     }

    //     if map.col(i) == 0 {
    //         traverse(&mut map, i, Direction::East);

    //         res = max(res, map.count_energized());
    //         map.reset();
    //     }

    //     if map.col(i) == map.cols - 1 {
    //         traverse(&mut map, i, Direction::West);

    //         res = max(res, map.count_energized());
    //         map.reset();
    //     }

    //     if map.row(i) == 0 {
    //         traverse(&mut map, i, Direction::South);

    //         res = max(res, map.count_energized());
    //         map.reset();
    //     }

    //     if map.row(i) == map.rows - 1 {
    //         traverse(&mut map, i, Direction::North);

    //         res = max(res, map.count_energized());
    //         map.reset();
    //     }
    // }

    res
}

const VISITED_MASK: u8 = 0b1000_0000;

#[derive(Debug)]
struct Neighbours {
    north: Option<Position>,
    east: Option<Position>,
    south: Option<Position>,
    west: Option<Position>,
}

struct Map<'a> {
    map: &'a mut [u8],
    rows: usize,
    cols: usize,
}

impl<'a> core::fmt::Debug for Map<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // f.write_str(
        //     &self
        //         .map
        //         .iter()
        //         .map(|c| (c & !VISITED_MASK) as char)
        //         .collect::<String>(),
        // )

        f.debug_struct("Map")
            .field("cols", &self.cols)
            .field("rows", &self.rows)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
struct State<'a> {
    map: &'a mut Map<'a>,
    lowest_heat_loss_so_far: Option<u32>,
    current_heat_loss: u32,
    past_directions: PastDirections,
}

#[derive(Debug, Clone, PartialEq)]
enum PastDirections {
    None,
    One(Direction),
    Two(Direction, Direction),
    Three(Direction, Direction, Direction),
}

impl PastDirections {
    const fn push(&self, new: Direction) -> Self {
        match self {
            Self::None => Self::One(new),
            Self::One(a) => Self::Two(*a, new),
            Self::Two(a, b) => Self::Three(*a, *b, new),
            Self::Three(a, b, c) => Self::Three(*b, *c, new),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Position {
    x: usize,
    y: usize,
}

impl Position {
    const fn north(self) -> Option<Self> {
        Some(Self {
            x: self.x,
            y: option_try!(self.y.checked_sub(1)),
        })
    }

    const fn south(self) -> Option<Self> {
        Some(Self {
            x: self.x,
            y: option_try!(self.y.checked_add(1)),
        })
    }

    const fn west(self) -> Option<Self> {
        Some(Self {
            x: option_try!(self.x.checked_sub(1)),
            y: self.y,
        })
    }

    const fn east(self) -> Option<Self> {
        Some(Self {
            x: option_try!(self.x.checked_add(1)),
            y: self.y,
        })
    }
}

impl<'a> Map<'a> {
    fn new(map: &'a mut [u8]) -> Self {
        let rows = count_segments::<b'\n', true>(map);
        let cols = (read_until(map, 0, b"\n")).len();

        // dbg!(map[(10 + 1) * 5] as char);

        Self { map, rows, cols }
    }

    fn read(&self, position: Position) -> u32 {
        assert!(self.is_valid(position));
        ((self.map[position.x + (position.y * (self.cols + 1))] & (!VISITED_MASK)) - 48) as u32
    }

    const fn is_valid(&self, position: Position) -> bool {
        position.x < self.cols && position.y < self.rows
    }

    const fn visit(&mut self, position: Position) {
        assert!(self.is_valid(position));
        self.map[position.x + (position.y * (self.cols + 1))] |= VISITED_MASK;
    }

    const fn unvisit(&mut self, position: Position) {
        assert!(self.is_valid(position));
        self.map[position.x + (position.y * (self.cols + 1))] &= !VISITED_MASK;
    }

    const fn is_visited(&self, position: Position) -> bool {
        assert!(self.is_valid(position));
        self.map[position.x + (position.y * (self.cols + 1))] & VISITED_MASK == VISITED_MASK
    }

    const fn neighbours(&self, position: Position) -> Neighbours {
        Neighbours {
            north: if let Some(north) = position.north()
                && self.is_valid(north)
                && !self.is_visited(north)
            {
                Some(north)
            } else {
                None
            },
            east: if let Some(east) = position.east()
                && self.is_valid(east)
                && !self.is_visited(east)
            {
                Some(east)
            } else {
                None
            },
            south: if let Some(south) = position.south()
                && self.is_valid(south)
                && !self.is_visited(south)
            {
                Some(south)
            } else {
                None
            },
            west: if let Some(west) = position.west()
                && self.is_valid(west)
                && !self.is_visited(west)
            {
                Some(west)
            } else {
                None
            },
        }
    }

    #[cfg(test)]
    fn dbg_input(&self) {
        // let col_empty_color = "\u{001b}[30;47m";
        // let reset = "\u{001b}[0m";

        // let formatted = &self
        //     .map
        //     .chunks(self.cols + 1)
        //     .map(|chars| {
        //         chars
        //             .iter()
        //             .take(self.cols)
        //             .map(|char| {
        //                 let c = Tile::from_byte(*char).unwrap();
        //                 (char & !MASK > 0)
        //
        // .then_some(format!("{col_empty_color}{c}{reset}"))
        //                     .unwrap_or(c.to_string())
        //             })
        //             .collect::<String>()
        //     })
        //     .collect::<Vec<_>>()
        //     .join("\n");

        // println!("{formatted}");
    }
}

// get neighbours
// for each neighbour
// mark the node (neighbour) as visited
// then, traverse with state:
// - map
// - lowest heat loss to end so far (as an Option)
// - current heat loss + current neighbour's heat loss
// - past 3 movement directions
fn dfs(state: &mut State, pos: Position, depth: u32) {
    // dbg!((&state, &pos));
    if let Some(l) = state.lowest_heat_loss_so_far {
        if l < state.current_heat_loss {
            return;
        }
    }

    // if depth > 35 {
    //     return;
    // }

    if pos
        == (Position {
            x: state.map.cols - 1,
            y: state.map.rows - 1,
        })
    {
        println!(
            "found end at depth {depth} with heat loss {:?}",
            state.current_heat_loss
        );
        state.lowest_heat_loss_so_far = if let Some(lowest) = state.lowest_heat_loss_so_far {
            Some(min!(lowest, state.current_heat_loss))
        } else {
            Some(state.current_heat_loss)
        };

        return;
    }

    let neighbours = state.map.neighbours(pos);

    // dbg!(&neighbours);

    let arr = [
        (neighbours.north, Direction::North),
        (neighbours.south, Direction::South),
        (neighbours.east, Direction::East),
        (neighbours.west, Direction::West),
    ];

    for (neighbour, dir) in arr {
        if let Some(n) = neighbour
            && state.past_directions != PastDirections::Three(dir, dir, dir)
        {
            // dbg!((n, dir));

            let new_past_directions = state.past_directions.push(dir);
            state.map.visit(n);
            state.current_heat_loss += state.map.read(n);
            let old_past_directions = mem::replace(&mut state.past_directions, new_past_directions);
            dfs(state, n, depth + 1);

            state.map.unvisit(n);
            state.current_heat_loss -= state.map.read(n);
            state.past_directions = old_past_directions;
        }
    }
}
