use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{
        array::{self, ArrayVec},
        grid::{Direction, GridMut, Position},
        iter,
    },
};

#[apply(day)]
impl Day<2024, 16> {
    pub const fn parse(input: &mut [u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &mut [u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &mut [u8]) -> u32 {
    let mut grid = GridMut::new(input);

    let mut start_pos = Position::new(0, 0);

    #[apply(iter)]
    for (pos, val) in grid(grid) {
        if val == b'S' {
            start_pos = pos;
            break;
        }
    }

    find_shortest_path(&mut grid, start_pos)
}

const fn parse2(input: &mut [u8]) -> u32 {
    let mut grid = GridMut::new(input);

    let mut start_pos = Position::new(0, 0);

    #[apply(iter)]
    for (pos, val) in grid(grid) {
        if val == b'S' {
            start_pos = pos;
        }
    }

    find_all_shortest_paths::<39340>(&mut grid, start_pos)
}

const fn find_shortest_path(grid: &mut GridMut, start: Position) -> u32 {
    const fn check_direction_and_visit(
        grid: &mut GridMut<'_>,
        position: Position,
        direction: Direction,
    ) -> bool {
        let next = position.direction(direction).unwrap();
        let next_tile = grid.get_mut(next).unwrap();

        if *next_tile == b'E' {
            true
        } else if is_empty(*next_tile) && !is_visited(*next_tile) {
            visit(next_tile);
            true
        } else {
            false
        }
    }

    let mut queue = BinaryHeap::<512, Entry>::new();

    let mut path = ArrayVec::<_, 25000>::new();

    queue.insert(Entry {
        score: 0,
        position: MiniPosition::from_position(start),
        direction: Direction::East,
    });

    while let Some(Entry {
        score,
        position,
        direction,
    }) = queue.pop()
    {
        path.push(Entry {
            score,
            position,
            direction,
        });

        let position = position.into_position();

        if *grid.get(position).unwrap() == b'E' {
            return score;
        }

        if check_direction_and_visit(grid, position, direction) {
            queue.insert(Entry {
                score: score + 1,
                position: MiniPosition::from_position(position.direction(direction).unwrap()),
                direction,
            });
        }

        if check_direction_and_visit(grid, position, direction.clockwise()) {
            queue.insert(Entry {
                score: score + 1001,
                position: MiniPosition::from_position(
                    position.direction(direction.clockwise()).unwrap(),
                ),
                direction: direction.clockwise(),
            });
        }

        if check_direction_and_visit(grid, position, direction.counterclockwise()) {
            queue.insert(Entry {
                score: score + 1001,
                position: MiniPosition::from_position(
                    position.direction(direction.counterclockwise()).unwrap(),
                ),
                direction: direction.counterclockwise(),
            });
        }
    }

    panic!("no route found");
}

/// `N` is the max [`Position::pair`] value for `grid`
const fn find_all_shortest_paths<const N: usize>(grid: &mut GridMut, start: Position) -> u32 {
    const fn check_direction_and_visit(
        grid: &mut GridMut<'_>,
        position: Position,
        direction: Direction,
        visited: &mut [[u32; 4]],
        score: u32,
    ) -> bool {
        let next_tile = grid.get_mut(position).unwrap();

        if *next_tile == b'E' {
            true
        } else if is_empty(*next_tile) {
            let prev_score = &mut visited[position.pair()][direction as usize];
            if *prev_score > 0 && *prev_score < score {
                return false;
            }

            *prev_score = score;

            true
        } else {
            false
        }
    }

    let min_score = find_shortest_path(grid, start);

    #[apply(iter)]
    for (_, tile) in grid_mut(grid) {
        un_visit(tile);
    }

    let mut queue = BinaryHeap::<30000, EntryWithPath<512>>::new();

    queue.insert(EntryWithPath {
        score: 0,
        position: MiniPosition::from_position(start),
        direction: Direction::East,
        path: ArrayVec::new(),
    });

    let mut min_paths = [false; N];

    let mut visited = [[0_u32; 4]; N];

    while let Some(EntryWithPath {
        score,
        position,
        direction,
        mut path,
    }) = queue.pop()
    {
        if min_score < score {
            break;
        }

        path.push(position);

        let position = position.into_position();

        if *grid.get(position).unwrap() == b'E' {
            assert!(score == min_score);

            #[apply(iter)]
            for position in iter(path.as_slice()) {
                min_paths[position.into_position().pair()] = true;
            }

            continue;
        }

        {
            let new_score = score + 1;
            let new_direction = direction;
            let new_position = position.direction(direction).unwrap();

            if check_direction_and_visit(grid, new_position, new_direction, &mut visited, new_score)
                && new_score <= min_score
            {
                queue.insert(EntryWithPath {
                    score: new_score,
                    position: MiniPosition::from_position(new_position),
                    direction: new_direction,
                    path: array::clone!(path; |x| *x),
                });
            }
        }

        {
            let new_score = score + 1001;
            let new_direction = direction.clockwise();
            let new_position = position.direction(new_direction).unwrap();

            if check_direction_and_visit(grid, new_position, new_direction, &mut visited, new_score)
                && new_score <= min_score
            {
                queue.insert(EntryWithPath {
                    score: new_score,
                    position: MiniPosition::from_position(new_position),
                    direction: new_direction,
                    path: array::clone!(path; |x| *x),
                });
            }
        }
        {
            let new_score = score + 1001;
            let new_direction = direction.counterclockwise();
            let new_position = position.direction(new_direction).unwrap();

            if check_direction_and_visit(grid, new_position, new_direction, &mut visited, new_score)
                && new_score <= min_score
            {
                queue.insert(EntryWithPath {
                    score: new_score,
                    position: MiniPosition::from_position(new_position),
                    direction: new_direction,
                    path,
                });
            }
        }
    }

    let mut total = 0;

    #[apply(iter)]
    for &is_min_path_tile in iter(min_paths) {
        if is_min_path_tile {
            total += 1;
        }
    }

    total
}

const VISITED_MASK: u8 = 0b1000_0000;

const fn visit(tile: &mut u8) {
    *tile |= VISITED_MASK;
}

const fn un_visit(tile: &mut u8) {
    *tile &= !VISITED_MASK;
}

const fn is_visited(tile: u8) -> bool {
    tile & VISITED_MASK == VISITED_MASK
}

const fn is_empty(tile: u8) -> bool {
    tile & !VISITED_MASK == b'.'
}

#[derive(Debug)]
struct BinaryHeap<const N: usize, T> {
    arr: ArrayVec<T, N>,
}

impl<const N: usize, T> BinaryHeap<N, T> {
    const fn new() -> Self {
        Self {
            arr: ArrayVec::new(),
        }
    }
}

macro_rules! monomorphize {
    ([$($gen:tt)*] $T:ty; |$x:ident| $e:expr) => {
        impl<const N: usize, $($gen)*> BinaryHeap<N, $T> {
            const fn insert(&mut self, item: $T) {
                #[track_caller]
                const fn parent(i: usize) -> usize {
                    (i - 1) / 2
                }

                self.arr.append(item);

                let mut i = self.arr.len() - 1;

                let heap = self.arr.as_slice_mut();

                while i != 0 && {
                    let $x = &heap[parent(i)];
                    $e
                } > {
                    let $x = &heap[i];
                    $e
                } {
                    heap.swap(i, parent(i));
                    i = parent(i);
                }
            }

            const fn pop(&mut self) -> Option<$T> {
                match self.arr.len() {
                    0 => None,
                    1 => Some(self.arr.pop()),
                    _ => {
                        let len = self.arr.len();
                        self.arr.as_slice_mut().swap(0, len - 1);
                        let min = self.arr.pop();
                        self.min_heapify(0);
                        Some(min)
                    }
                }
            }

            const fn min_heapify(&mut self, i: usize) {
                let heap = self.arr.as_slice_mut();
                let n = heap.len();

                if n == 1 {
                    return;
                }

                let l = (2 * i) + 1;
                let r = (2 * i) + 2;
                let mut smallest = i;

                if l < n && {
                    let $x = &heap[l];
                    $e
                } < {
                    let $x = &heap[i];
                    $e
                } {
                    smallest = l
                }

                if r < n && {
                    let $x = &heap[r];
                    $e
                } < {
                    let $x = &heap[smallest];
                    $e
                } {
                    smallest = r
                }

                if smallest != i {
                    heap.swap(i, smallest);
                    self.min_heapify(smallest);
                }
            }
        }
    };
}

monomorphize!(              [] Entry;            |x| x.score);
monomorphize!([const M: usize] EntryWithPath<M>; |x| x.score);

#[derive(Clone, Copy, PartialEq)]
struct Entry {
    score: u32,
    position: MiniPosition,
    direction: Direction,
}

struct EntryWithPath<const N: usize> {
    score: u32,
    position: MiniPosition,
    direction: Direction,
    path: ArrayVec<MiniPosition, N>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct MiniPosition {
    pub row: u8,
    pub col: u8,
}

impl MiniPosition {
    pub const fn from_position(position: Position) -> Self {
        assert!(position.row() <= u8::MAX as usize);
        assert!(position.col() <= u8::MAX as usize);

        #[expect(clippy::cast_possible_truncation, reason = "checked above")]
        Self {
            row: position.row() as u8,
            col: position.col() as u8,
        }
    }

    pub const fn into_position(self) -> Position {
        Position::new(self.row as usize, self.col as usize)
    }
}

#[cfg(test)]
#[test]
fn test() {
    #[allow(unused)]
    let mut input = b"
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"
    .to_vec();

    #[allow(unused)]
    let mut input2 = b"
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
"
    .to_vec();

    // dbg!(parse(&mut input));
    // dbg!(parse2(&mut input));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
