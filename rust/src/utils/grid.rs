use crate::utils::{count_segments, read_until, trim_ascii_mut};

#[derive(Debug)]
pub struct GridMut<'a> {
    grid: &'a mut [u8],
    rows: usize,
    cols: usize,
}

impl<'a> GridMut<'a> {
    pub const fn new(grid: &'a mut [u8]) -> Self {
        let grid = trim_ascii_mut(grid);

        let rows = count_segments::<b'\n', false>(grid);
        let cols = read_until(grid, 0, b"\n").len();

        Self { grid, rows, cols }
    }

    #[must_use]
    pub const fn rows(&self) -> usize {
        self.rows
    }

    #[must_use]
    pub const fn cols(&self) -> usize {
        self.cols
    }

    #[track_caller]
    pub const fn get_mut(&mut self, pos: Position) -> Option<&mut u8> {
        if pos.row < self.rows && pos.col < self.cols {
            // +1 for newline
            Some(&mut self.grid[(pos.row * (self.cols + 1)) + pos.col])
        } else {
            None
        }
    }

    #[track_caller]
    #[must_use]
    pub const fn get(&self, pos: Position) -> Option<&u8> {
        if pos.row < self.rows && pos.col < self.cols {
            // +1 for newline
            Some(&self.grid[(pos.row * (self.cols + 1)) + pos.col])
        } else {
            None
        }
    }

    #[must_use]
    pub const fn raw(&self) -> &[u8] {
        self.grid
    }

    #[must_use]
    pub const fn raw_mut(&mut self) -> &mut [u8] {
        self.grid
    }

    #[allow(unused, reason = "only used in debugging")]
    pub fn debug<C>(&self, f: impl Fn(u8) -> C)
    where
        String: FromIterator<C>,
    {
        let s = self.grid.iter().map(|c| f(*c)).collect::<String>();

        println!("{s}\n");
    }
}

#[derive(Clone, Copy)]
pub struct Position {
    row: usize,
    col: usize,
}

impl Position {
    #[must_use]
    pub const fn new(row: usize, col: usize) -> Self {
        Self { row, col }
    }

    #[must_use]
    pub const fn row(&self) -> usize {
        self.row
    }

    #[must_use]
    pub const fn col(&self) -> usize {
        self.col
    }
}

impl core::fmt::Debug for Position {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "({},{})", self.row, self.col)
    }
}

#[test]
fn grid_get() {
    let mut input = b"
123
456
789
"
    .to_owned();

    let grid = GridMut::new(&mut input);

    dbg!(&grid);

    assert_eq!(grid.get(Position::new(0, 0)), Some(&b'1'));
    assert_eq!(grid.get(Position::new(1, 2)), Some(&b'6'));
    assert_eq!(grid.get(Position::new(2, 2)), Some(&b'9'));
    assert_eq!(grid.get(Position::new(2, 3)), None);
}
