use cfg_proc::apply;

use crate::{
    day,
    utils::{
        grid::{Direction, GridMut, Position},
        iter, split_once_mut,
    },
    Day,
};

#[apply(day)]
impl Day<2024, 15> {
    pub const fn parse(input: &mut [u8]) -> usize {
        parse(input)
        // 0
    }

    pub const fn parse2(input: &mut [u8]) -> usize {
        parse2(input)
        // 0
    }
}

const fn parse(input: &mut [u8]) -> usize {
    let (grid, movements) = split_once_mut(input, b"\n\n").unwrap();

    let mut grid = GridMut::new(grid);

    let mut robot_pos;
    'set: {
        #[apply(iter)]
        for (pos, val) in grid(grid) {
            if val == b'@' {
                robot_pos = pos;
                break 'set;
            }
        }

        panic!("no robot present in map");
    }

    #[apply(iter)]
    for movement in iter(movements) {
        let Some(direction) = direction_from_movement_byte(*movement) else {
            continue;
        };

        move_robot(&mut grid, &mut robot_pos, direction);
    }

    let mut total = 0;

    #[apply(iter)]
    for (pos, val) in grid(grid) {
        if val == b'O' {
            total += (pos.row() * 100) + pos.col();
        }
    }

    total
}

const fn direction_from_movement_byte(movement: u8) -> Option<Direction> {
    match movement {
        b'^' => Some(Direction::North),
        b'v' => Some(Direction::South),
        b'>' => Some(Direction::East),
        b'<' => Some(Direction::West),
        _ => None,
    }
}

const fn move_robot(grid: &mut GridMut, robot_pos: &mut Position, direction: Direction) {
    let original_pos = *robot_pos;
    let mut curr_pos = robot_pos.direction(direction).unwrap();
    loop {
        let new_val = grid.get_mut(curr_pos).unwrap();
        if *new_val == b'.' {
            // hit an empty space, shift between original position and current position
            while !curr_pos.eq(original_pos) {
                let prev_pos = curr_pos.direction(direction.opposite()).unwrap();
                grid.swap(curr_pos, prev_pos);
                curr_pos = prev_pos;
            }

            *robot_pos = robot_pos.direction(direction).unwrap();

            return;
        } else if *new_val == b'#' {
            // hit a wall, can't move
            return;
        } else if *new_val == b'O' {
            // hit a box, attempt to move it
            curr_pos = curr_pos.direction(direction).unwrap();
        } else {
            panic!()
        }
    }
}

#[derive(Debug, PartialEq)]
#[repr(u8)]
enum TileNibble {
    Empty = 0b0000,
    BoxLeft = 0b0001,
    BoxRight = 0b0010,
    Wall = 0b0100,
    Robot = 0b1000,
}

impl TileNibble {
    /// Split the given byte into it's component tile nibbles. This assumes that
    /// both nibbles are valid tiles, and will panic otherwise.
    const fn from_byte(byte: u8) -> (Self, Self) {
        const NIBBLE_MASK: u8 = 0b1111;

        (
            Self::from_nibble((byte & (NIBBLE_MASK << 4)) >> 4),
            Self::from_nibble(byte & NIBBLE_MASK),
        )
    }

    /// Reads the *bottom* nibble of the provided byte. Assumes that the top
    /// nibble has been zeroed and the bottom nibble is a valid tile, and
    /// will panic otherwise.
    const fn from_nibble(byte: u8) -> Self {
        match byte {
            _ if byte == Self::Empty as u8 => Self::Empty,
            _ if byte == Self::BoxLeft as u8 => Self::BoxLeft,
            _ if byte == Self::BoxRight as u8 => Self::BoxRight,
            _ if byte == Self::Wall as u8 => Self::Wall,
            _ if byte == Self::Robot as u8 => Self::Robot,
            _ => panic!("unknown nibble"),
        }
    }
}

const fn full_tile(top: TileNibble, bottom: TileNibble) -> u8 {
    ((top as u8) << 4) | bottom as u8
}

struct WideWarehouse<'a> {
    grid: GridMut<'a>,
    robot_pos: Position,
}

impl<'a> WideWarehouse<'a> {
    const fn new(input: &'a mut [u8]) -> Self {
        use TileNibble::{BoxLeft, BoxRight, Empty, Robot, Wall};

        let mut grid = GridMut::new(input);

        let mut robot_pos = None;

        #[apply(iter)]
        for (pos, tile) in grid_mut(grid) {
            match *tile {
                b'.' => *tile = full_tile(Empty, Empty),
                b'#' => *tile = full_tile(Wall, Wall),
                b'O' => *tile = full_tile(BoxLeft, BoxRight),
                b'@' => {
                    robot_pos = Some(Position::new(pos.row(), pos.col() * 2));
                    *tile = full_tile(Robot, Empty);
                }
                _ => panic!("unknown tile"),
            }
        }

        Self {
            grid,
            robot_pos: robot_pos.unwrap(),
        }
    }

    const fn get(&self, position: Position) -> Option<TileNibble> {
        let pos = Position::new(position.row(), position.col() / 2);
        match self.grid.get(pos) {
            Some(tile) => {
                let (top, bottom) = TileNibble::from_byte(*tile);
                Some(if position.col() % 2 == 0 { top } else { bottom })
            }
            None => None,
        }
    }

    const fn set(&mut self, position: Position, tile: TileNibble) {
        let pos = Position::new(position.row(), position.col() / 2);
        let val = self.grid.get_mut(pos).unwrap();

        let (top, bottom) = TileNibble::from_byte(*val);

        *val = if position.col() % 2 == 0 {
            full_tile(tile, bottom)
        } else {
            full_tile(top, tile)
        };
    }

    const fn swap(&mut self, a: Position, b: Position) {
        let a_tile = self.get(a).unwrap();
        let b_tile = self.get(b).unwrap();

        assert!(
            !(matches!(a_tile, TileNibble::Wall) || matches!(b_tile, TileNibble::Wall)),
            "can't move a wall"
        );

        self.set(a, b_tile);
        self.set(b, a_tile);
    }

    const fn step(&mut self, direction: Direction) {
        // if the direction is north/south, check if a box is being pushed. if
        // there is, then check if that box is pushing any other boxes. this
        // must be done carefully, since a box can be both split between two
        // tiles and be pushing up to two boxes (which can themselves be pushing
        // up to two boxes, and so on). if any of the boxes in this tree of
        // boxes cannot be moved (due to having hit a wall), then none of the
        // boxes will be moved and the robot will stay in the same position.

        match direction {
            direction @ (Direction::North | Direction::South) => {
                let new_robot_pos = self.robot_pos.direction(direction).unwrap();

                // check that the tile being moved by the robot can be moved
                if self.check_move_tile(new_robot_pos, direction) {
                    self.move_tile(new_robot_pos, direction);
                    self.swap(self.robot_pos, new_robot_pos);
                    self.robot_pos = new_robot_pos;
                }
            }
            // this is mostly a copy/paste from part 1
            direction @ (Direction::West | Direction::East) => {
                let original_pos = self.robot_pos;
                let mut curr_pos = self.robot_pos.direction(direction).unwrap();
                loop {
                    let new_val = self.get(curr_pos).unwrap();
                    if matches!(new_val, TileNibble::Empty) {
                        // hit an empty space, shift between original position and current position
                        while !curr_pos.eq(original_pos) {
                            let prev_pos = curr_pos.direction(direction.opposite()).unwrap();
                            self.swap(curr_pos, prev_pos);
                            curr_pos = prev_pos;
                        }

                        self.robot_pos = self.robot_pos.direction(direction).unwrap();

                        return;
                    } else if matches!(new_val, TileNibble::Wall) {
                        // hit a wall, can't move
                        return;
                    } else if matches!(new_val, TileNibble::BoxLeft | TileNibble::BoxRight) {
                        // hit a box, attempt to move it
                        curr_pos = curr_pos.direction(direction).unwrap();
                    } else {
                        panic!()
                    }
                }
            }
        }
    }

    const fn check_move_tile(&self, position: Position, direction: Direction) -> bool {
        let Some(new_pos) = position.direction(direction) else {
            return false;
        };
        match self.get(position).unwrap() {
            TileNibble::Empty => true,
            TileNibble::BoxLeft => {
                self.check_move_tile(new_pos, direction)
                    && self.check_move_tile(new_pos.direction(Direction::East).unwrap(), direction)
            }
            TileNibble::BoxRight => {
                self.check_move_tile(new_pos, direction)
                    && self.check_move_tile(new_pos.direction(Direction::West).unwrap(), direction)
            }
            TileNibble::Wall => false,
            TileNibble::Robot => panic!("how did this happen"),
        }
    }

    const fn move_tile(&mut self, position: Position, direction: Direction) {
        let new_pos = position.direction(direction).unwrap();
        match self.get(position).unwrap() {
            TileNibble::Empty => {
                // don't move empty tiles, if this space is being moved *into*
                // then
            }
            TileNibble::BoxLeft => {
                let box_new_right_pos = new_pos.direction(Direction::East).unwrap();

                // println!("[move_tile] box left");

                self.move_tile(new_pos, direction);
                self.move_tile(box_new_right_pos, direction);

                self.swap(new_pos, position);
                self.swap(
                    box_new_right_pos,
                    position.direction(Direction::East).unwrap(),
                );
            }
            TileNibble::BoxRight => {
                let box_new_left_pos = new_pos.direction(Direction::West).unwrap();

                // println!("[move_tile] box right");

                self.move_tile(new_pos, direction);
                self.move_tile(box_new_left_pos, direction);

                self.swap(new_pos, position);
                self.swap(
                    box_new_left_pos,
                    position.direction(Direction::West).unwrap(),
                );
            }
            TileNibble::Wall => panic!("can't move a wall"),
            TileNibble::Robot => panic!("how did this happen"),
        }
    }

    #[allow(dead_code)]
    fn debug(&self) {
        // assert_eq!(self.get(self.robot_pos).unwrap(), TileNibble::Robot);

        self.grid.debug(|tile| {
            if tile == b'\n' {
                "\n".to_owned()
            } else {
                let (top, bottom) = TileNibble::from_byte(tile);
                let mk_char = |tn| match tn {
                    TileNibble::Empty => '.',
                    TileNibble::BoxLeft => '[',
                    TileNibble::BoxRight => ']',
                    TileNibble::Wall => '#',
                    TileNibble::Robot => '@',
                };

                String::from_iter([mk_char(top), mk_char(bottom)])
            }
        });
    }
}

const fn parse2(input: &mut [u8]) -> usize {
    let (grid, movements) = split_once_mut(input, b"\n\n").unwrap();

    let mut wh = WideWarehouse::new(grid);

    #[apply(iter)]
    for movement in iter(movements) {
        let Some(direction) = direction_from_movement_byte(*movement) else {
            continue;
        };

        wh.step(direction);
    }

    let mut total = 0;

    #[apply(iter)]
    for (pos, val) in grid(wh.grid) {
        let (l, r) = TileNibble::from_byte(val);
        if matches!(l, TileNibble::BoxLeft) {
            total += (pos.row() * 100) + (pos.col() * 2);
        } else if matches!(r, TileNibble::BoxLeft) {
            total += ((pos.row()) * 100) + (pos.col() * 2) + 1;
        }
    }

    total
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
";

    //     let input = "
    // #####
    // #.@.#
    // #.O.#
    // #...#
    // #####

    // v
    // ";

    dbg!(parse2(&mut input.as_bytes().to_owned()));
    dbg!(parse2(&mut Today::INPUT.as_bytes().to_owned()));
}
