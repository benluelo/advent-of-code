use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{count_segments, iter, read_until},
};

#[apply(day)]
impl Day<2024, 4> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u32 {
    let mut total = 0;

    let ws = WordSearch::new(input);

    #[apply(iter)]
    for x in range(0, ws.rows) {
        #[apply(iter)]
        for y in range(0, ws.cols) {
            total += ws.find_xmas(x, y);
        }
    }

    total
}

const fn parse2(input: &[u8]) -> u32 {
    let mut total = 0;

    let ws = WordSearch::new(input);

    // the first and last rows/columns can be skipped, as an A at the edges can
    // never be in the center of an X-MAS
    #[apply(iter)]
    for x in range(1, ws.rows - 1) {
        #[apply(iter)]
        for y in range(1, ws.cols - 1) {
            total += ws.find_x_mas(x, y) as u32;
        }
    }

    total
}

macro_rules! do_ {
    ($($pat:pat = $expr:expr;)* { else $else:expr }) => {
        $(
            #[allow(unused_parens)] // see https://stackoverflow.com/questions/70929552/why-are-top-level-or-patterns-not-allowed-in-let-bindings
            let ($pat) = $expr else {
                $else
            };
        )*
    };
}

struct WordSearch<'a> {
    grid: &'a [u8],
    rows: usize,
    cols: usize,
}

impl<'a> WordSearch<'a> {
    const fn new(grid: &'a [u8]) -> Self {
        let grid = grid.trim_ascii();

        let rows = count_segments::<b'\n', false>(grid);
        let cols = read_until(grid, 0, b"\n").len();

        Self { grid, rows, cols }
    }

    const fn get(&self, row: usize, col: usize) -> Option<char> {
        if row < self.rows && col < self.cols {
            Some(self.grid[(row * (self.cols + 1)) + col] as char)
        } else {
            None
        }
    }

    const fn find_xmas(&self, row: usize, col: usize) -> u32 {
        let mut total = 0;

        let Some('X') = self.get(row, col) else {
            return 0;
        };

        // check all 8 directions around this X for an M

        let directions: [(isize, isize); 8] = [
            (-1, -1), // NW
            (-1, 0),  // N
            (-1, 1),  // NE
            (0, 1),   // E
            (1, 1),   // SE
            (1, 0),   // S
            (1, -1),  // SW
            (0, -1),  // W
        ];

        #[apply(iter)]
        for (dx, dy) in iter(directions) {
            do_! {
                Some(row) = row.checked_add_signed(*dx);
                Some(col) = col.checked_add_signed(*dy);
                Some('M') = self.get(row, col);

                Some(row) = row.checked_add_signed(*dx);
                Some(col) = col.checked_add_signed(*dy);
                Some('A') = self.get(row, col);

                Some(row) = row.checked_add_signed(*dx);
                Some(col) = col.checked_add_signed(*dy);
                Some('S') = self.get(row, col);

                { else continue }
            };

            total += 1;
        }

        total
    }

    const fn find_x_mas(&self, row: usize, col: usize) -> bool {
        let nw = (-1, -1); // NW
        let ne = (-1, 1); // NE
        let se = (1, 1); // SE
        let sw = (1, -1); // SW

        do_! {
            Some('A') = self.get(row, col);

            Some(nw_row) = row.checked_add_signed(nw.0);
            Some(nw_col) = col.checked_add_signed(nw.1);

            Some(se_row) = row.checked_add_signed(se.0);
            Some(se_col) = col.checked_add_signed(se.1);

            (Some('M'), Some('S')) | (Some('S'), Some('M')) = (
                self.get(nw_row, nw_col),
                self.get(se_row, se_col)
            );

            Some(ne_row) = row.checked_add_signed(ne.0);
            Some(ne_col) = col.checked_add_signed(ne.1);

            Some(sw_row) = row.checked_add_signed(sw.0);
            Some(sw_col) = col.checked_add_signed(sw.1);

            (Some('M'), Some('S')) | (Some('S'), Some('M')) = (
                self.get(ne_row, ne_col),
                self.get(sw_row, sw_col)
            );

            { else return false }
        };

        true
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
";

    dbg!(parse2(input.as_bytes()));
}
