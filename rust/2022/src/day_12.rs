use std::{
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap},
};

pub fn solution(input: &str) -> u32 {
    let ParsedMap { start, end, edges } = parse(input);

    dijkstra(start, end, &edges).unwrap()
}

struct ParsedMap {
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

    ParsedMap { start, end, edges }
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

fn dijkstra(
    start: Position,
    end: Position,
    adj_list: &BTreeMap<Position, Vec<Edge>>,
) -> Option<u32> {
    #[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
    struct State {
        cost: u32,
        position: Position,
    }

    // dist[node] = current shortest distance from `start` to `node`
    // let mut dist: Vec<_> = grid
    //     .iter()
    //     .map(|v| v.iter().map(|_| None).collect::<Vec<_>>())
    //     .collect::<Vec<_>>();

    let mut dist = BTreeMap::new();

    let mut heap = BinaryHeap::new();

    // We're at `start`, with a zero cost
    dist.insert(start, 0);

    heap.push(Reverse(State {
        cost: 0,
        position: start,
    }));

    // Examine the frontier with lower cost nodes first (min-heap)
    while let Some(Reverse(State { cost, position })) = heap.pop() {
        // dbg!(&dist);
        // dbg!(&heap);

        // Alternatively we could have continued to find all shortest paths
        if position == end {
            return Some(cost);
        }

        // Important as we may have already found a better way
        if dist.get(&position).is_some_and(|c| cost > *c) {
            continue;
        }

        // For each node we can reach, see if we can find a way with
        // a lower cost going through this node
        // dbg!(adj_list);
        for edge in &adj_list[&position] {
            let next = State {
                cost: cost + 1,
                position: edge.to,
            };

            // If so, add it to the frontier and continue
            let use_next_node = match dist.get(&edge.to) {
                Some(current_next) => next.cost < *current_next,
                None => true,
            };
            if use_next_node {
                heap.push(Reverse(next.clone()));
                // Relaxation, we have now found a better way
                dist.insert(next.position, next.cost);
            }
        }
    }

    // Goal not reachable
    None
}

fn get_edge_if_traversable(from: Position, to: Position, grid: &[Vec<char>]) -> Option<Edge> {
    // destination can be one larger than the current position or any value less
    // 5 -> 6 - ok
    // 5 -> 5 - ok
    // 5 -> 2 - ok
    // 5 -> 7 - not ok, destination is 2 greater than the current position
    (char_to_height(grid[from.y][from.x]) + 1 >= char_to_height(*grid.get(to.y)?.get(to.x)?))
        .then_some(Edge {
            to: Position { x: to.x, y: to.y },
        })
}

#[derive(Debug)]
struct Edge {
    to: Position,
}

pub fn solution_part_2(_input: &str) -> u32 {
    todo!();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_dijkstra() {
        // let map = r#"Sab
        //              abb
        //              ccE"#;

        let map = "SbcdefghijklmnopqrstuvwxyE";

        let ParsedMap { start, end, edges } = parse(map);

        dbg!(&edges);

        dbg!(dijkstra(start, end, &edges));

        // dbg!(char_to_height('a'));
    }

    // #[test]
    // fn test_is_valid_neighbour() {
    //     let map = r#"Sab
    //                  abb
    //                  ccE"#;

    //     let ParsedMap {
    //         grid,
    //         start,
    //         end,
    //         edges,
    //     } = parse(map);

    //     assert!(get_edge_if_traversable().is_some())
    // }
}

fn char_to_height(c: char) -> u8 {
    match c {
        'a'..='z' => c as u8 - 97,
        'S' => char_to_height('a'),
        'E' => char_to_height('z'),
        _ => panic!("bad input: {c}"),
    }
}
