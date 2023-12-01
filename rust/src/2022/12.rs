use std::{
    borrow::Borrow,
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap},
    fmt::Display,
};

use crate::{Day, DaySolution, Input};

impl DaySolution for Day<2022, 12> {
    fn part_1() -> impl Display {
        let ParsedMap {
            start, end, edges, ..
        } = parse(Self::INPUT);

        dijkstra(start, |pos| pos == end, |pos| edges[&pos].iter()).unwrap()
    }

    fn part_2() -> impl Display {
        let ParsedMap {
            grid, end, edges, ..
        } = parse(Self::INPUT);

        dijkstra(
            end,
            |pos| Height::from(grid_at_position(&grid, pos).unwrap()) == Height::ZERO,
            |pos| {
                // this is terribly ineficient
                // but it works!
                edges.iter().filter_map(move |(position, edges)| {
                    edges
                        .iter()
                        .any(|edge| edge.to == pos)
                        .then_some(Edge { to: *position })
                })
            },
        )
        .unwrap()
    }
}

struct ParsedMap {
    grid: Vec<Vec<char>>,
    start: Position,
    end: Position,
    edges: BTreeMap<Position, Vec<Edge>>,
}

fn parse(input: &str) -> ParsedMap {
    let grid = input
        .trim()
        .lines()
        .map(|line| line.trim().chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let width = grid[0].len();
    let height = grid.len();

    let start = find_char(&grid, 'S');
    let end = find_char(&grid, 'E');

    let edges: BTreeMap<Position, Vec<Edge>> = (0..width)
        .flat_map(|x| {
            (0..height).map(move |y| Position { x, y }).map(|pos| {
                (
                    pos,
                    [
                        pos.up()
                            .and_then(|new_pos| get_edge_if_traversable(pos, new_pos, &grid)),
                        pos.down()
                            .and_then(|new_pos| get_edge_if_traversable(pos, new_pos, &grid)),
                        pos.left()
                            .and_then(|new_pos| get_edge_if_traversable(pos, new_pos, &grid)),
                        pos.right()
                            .and_then(|new_pos| get_edge_if_traversable(pos, new_pos, &grid)),
                    ]
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>(),
                )
            })
        })
        .collect();

    ParsedMap {
        grid,
        start,
        end,
        edges,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Position {
    x: usize,
    y: usize,
}

impl Position {
    fn up(self) -> Option<Self> {
        Some(Self {
            x: self.x,
            y: self.y.checked_sub(1)?,
        })
    }

    fn down(self) -> Option<Self> {
        Some(Self {
            x: self.x,
            y: self.y.checked_add(1)?,
        })
    }

    fn left(self) -> Option<Self> {
        Some(Self {
            x: self.x.checked_sub(1)?,
            y: self.y,
        })
    }

    fn right(self) -> Option<Self> {
        Some(Self {
            x: self.x.checked_add(1)?,
            y: self.y,
        })
    }
}

fn find_char(grid: &[Vec<char>], char: char) -> Position {
    grid.iter()
        .enumerate()
        .find_map(|(col_idx, row)| {
            row.iter()
                .enumerate()
                .find(|(_, &c)| c == char)
                .map(|opt| (col_idx, opt))
        })
        .map(|(row, (col, _))| Position { x: col, y: row })
        .unwrap()
}

fn dijkstra<Edges>(
    start: Position,
    end_fn: impl Fn(Position) -> bool,
    get_edges: impl Fn(Position) -> Edges,
) -> Option<u32>
where
    Edges: Iterator,
    <Edges as Iterator>::Item: Borrow<Edge>,
{
    #[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
    struct State {
        cost: u32,
        position: Position,
    }

    let mut dist = BTreeMap::new();
    let mut heap = BinaryHeap::new();

    dist.insert(start, 0);

    heap.push(Reverse(State {
        cost: 0,
        position: start,
    }));

    while let Some(Reverse(State { cost, position })) = heap.pop() {
        if end_fn(position) {
            return Some(cost);
        }

        if dist.get(&position).is_some_and(|c| cost > *c) {
            continue;
        }

        for edge in get_edges(position) {
            let edge = edge.borrow();

            let next = State {
                cost: cost + 1,
                position: edge.to,
            };

            let use_next_node = match dist.get(&edge.to) {
                Some(current_next) => next.cost < *current_next,
                None => true,
            };
            if use_next_node {
                heap.push(Reverse(next.clone()));
                dist.insert(next.position, next.cost);
            }
        }
    }

    None
}

fn get_edge_if_traversable(from: Position, to: Position, grid: &[Vec<char>]) -> Option<Edge> {
    Height::from(grid_at_position(grid, from).unwrap())
        .can_access(&grid_at_position(grid, to)?.into())
        .then_some(Edge { to })
}

#[derive(Debug)]
struct Edge {
    to: Position,
}

fn grid_at_position(grid: &[Vec<char>], pos: Position) -> Option<char> {
    grid.get(pos.y)?.get(pos.x).copied()
}

// would be intersting to benchmark with these enabled:
// #[rustc_layout_scalar_valid_range_start(0)]
// #[rustc_layout_scalar_valid_range_end(25)]
#[derive(PartialEq, PartialOrd)]
struct Height(u8);

impl Height {
    const ZERO: Self = Self(0);

    // destination can be one larger than the current position or any value less
    // 5 -> 6 - ok
    // 5 -> 5 - ok
    // 5 -> 2 - ok
    // 5 -> 7 - not ok, destination is 2 greater than the current position
    fn can_access(self, other: &Self) -> bool {
        self.0 + 1 >= other.0
    }
}

impl From<char> for Height {
    fn from(c: char) -> Self {
        match c {
            'a'..='z' => Self(c as u8 - 97),
            'S' => Self::from('a'),
            'E' => Self::from('z'),
            _ => panic!("bad input: {c}"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_basic_map() {
        let map = "SbcdefghijklmnopqrstuvwxyE";

        let ParsedMap {
            start, end, edges, ..
        } = parse(map);

        dbg!(&edges);

        dbg!(dijkstra(start, |pos| pos == end, |pos| edges[&pos].iter()));
    }
}
