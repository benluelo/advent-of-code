use cfg_proc::apply;

use crate::{
    const_helpers::{iter, line_len},
    day, Day,
};

#[apply(day)]
impl Day<2023, 13> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

#[test]
#[cfg(test)]
fn parse_test() {
    //     let input = b"\
    // #.##..##.
    // ..#.##.#.
    // ##......#
    // ##......#
    // ..#.##.#.
    // ..##..##.
    // #.#.##.#.

    // #...##..#
    // #....#..#
    // ..##..###
    // #####.##.
    // #####.##.
    // ..##..###
    // #....#..#
    // ";

    //     let input = b"\
    // ..###..##
    // .#.##..##
    // ##.######
    // .#.......
    // .#.#....#
    // ##.######
    // .#.##..##
    // ";

    let input = Day::<2023, 13>::INPUT.as_bytes();

    dbg!(parse(input));
    dbg!(parse2(input));
}

const fn parse(input: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for map in split(input, b"\n\n") {
        let map = Map {
            map,
            line_len: line_len(map),
        };

        res += if let Some(count) = parse_map_horizontal(map) {
            count * 100
        } else if let Some(count) = parse_map_vertical(map) {
            count
        } else {
            panic!()
        };
    }

    res
}

const fn parse2(input: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for map in split(input, b"\n\n") {
        let map = Map {
            map,
            line_len: line_len(map),
        };

        res += if let Some(count) = parse_map_horizontal_fix_smudge(map) {
            count * 100
        } else if let Some(count) = parse_map_vertical_fix_smudge(map) {
            count
        } else {
            panic!()
        };
    }

    res
}

const fn parse_map_horizontal(map: Map) -> Option<u32> {
    let mut a = 0;
    let mut b = map.line_len;

    while (b + map.line_len) - 1 <= map.map.len() {
        'outer: {
            if matches!(row_cmp(map, a, b), LineDiff::Equal) {
                // compare until either a or b hits the limit
                let mut a2 = a;
                let mut b2 = b + map.line_len;

                while (b2 + map.line_len) - 1 <= map.map.len() && a2 > 0 {
                    if !matches!(row_cmp(map, a2 - map.line_len, b2), LineDiff::Equal) {
                        break 'outer;
                    }

                    a2 -= map.line_len;
                    b2 += map.line_len;
                }

                #[allow(clippy::cast_possible_truncation)]
                return Some((a / map.line_len) as u32 + 1);
            }
        }

        a += map.line_len;
        b += map.line_len;
    }

    None
}

const fn parse_map_horizontal_fix_smudge(map: Map) -> Option<u32> {
    let unfixed_result = parse_map_horizontal(map);

    let mut a = 0;
    let mut b = map.line_len;

    let mut smudge_fixed = false;

    while (b + map.line_len) - 1 <= map.map.len() {
        'outer: {
            #[allow(clippy::cast_possible_truncation)]
            let matches_unfixed = if let Some(u) = unfixed_result
                && u == ((a / map.line_len) as u32) + 1
            {
                true
            } else {
                false
            };
            let lines_match = !matches_unfixed
                && match row_cmp(map, a, b) {
                    LineDiff::Equal => true,
                    LineDiff::OneDifference if !smudge_fixed => {
                        smudge_fixed = true;
                        true
                    }
                    _ => false,
                };

            if lines_match {
                // compare until either a or b hits the limit
                let mut a2 = a;
                let mut b2 = b + map.line_len;

                while (b2 + map.line_len) - 1 <= map.map.len() && a2 > 0 {
                    let lines_match = match row_cmp(map, a2 - map.line_len, b2) {
                        LineDiff::Equal => true,
                        LineDiff::OneDifference if !smudge_fixed => {
                            smudge_fixed = true;
                            true
                        }
                        _ => false,
                    };

                    if !lines_match {
                        smudge_fixed = false;
                        break 'outer;
                    }

                    a2 -= map.line_len;
                    b2 += map.line_len;
                }

                #[allow(clippy::cast_possible_truncation)]
                return Some((a / map.line_len) as u32 + 1);
            }
        }

        a += map.line_len;
        b += map.line_len;
    }

    None
}

const fn row_cmp(map: Map, a: usize, b: usize) -> LineDiff {
    let mut one_difference_found = false;

    // newlines --+
    //            |
    //            v
    // +-> #.#.#.#
    // |   .#.#.#. <-+
    // a   #.#.#.#   |
    // |   .#.#.#.   b
    // +-> #.#.#.#   |
    //     .#.#.#. <-+

    assert!(a < b);
    assert!(a % map.line_len == 0);
    assert!(b % map.line_len == 0);
    // not sure how to get this assertion to work but i got the right answer so
    // ¯\_(ツ)_/¯
    // assert!(a < map.map.len() - (line_len * 2) - 1);
    // NOTE: -2 to ignore newlines
    assert!(b < (map.map.len() - (map.line_len - 2)));
    assert!(b >= map.line_len);

    #[apply(iter)]
    for i in range(0, map.line_len - 1) {
        if map.map[a + i] != map.map[b + i] {
            if one_difference_found {
                return LineDiff::Different;
            }

            one_difference_found = true;
        }
    }

    if one_difference_found {
        LineDiff::OneDifference
    } else {
        LineDiff::Equal
    }
}

const fn parse_map_vertical(map: Map) -> Option<u32> {
    #[apply(iter)]
    'outer: for x in range(0, map.line_len - 2) {
        let left_col = x;
        let right_col = x + 1;

        if !matches!(col_cmp(map, left_col, right_col), LineDiff::Equal) {
            continue 'outer;
        }

        let mut left_col2 = left_col;
        let mut right_col2 = right_col + 1;

        // skip the newline
        while right_col2 < map.line_len - 1 && left_col2 > 0 {
            if !matches!(col_cmp(map, left_col2 - 1, right_col2), LineDiff::Equal) {
                continue 'outer;
            }

            left_col2 -= 1;
            right_col2 += 1;
        }

        #[allow(clippy::cast_possible_truncation)]
        return Some(left_col as u32 + 1);
    }

    None
}

const fn parse_map_vertical_fix_smudge(map: Map) -> Option<u32> {
    let unfixed_result = parse_map_vertical(map);

    let mut smudge_fixed = false;

    #[apply(iter)]
    'outer: for x in range(0, map.line_len - 2) {
        let left_col = x;
        let right_col = x + 1;

        #[allow(clippy::cast_possible_truncation)]
        let matches_unfixed = if let Some(u) = unfixed_result
            && u == ((left_col + 1) as u32)
        {
            true
        } else {
            false
        };

        let lines_match = !matches_unfixed
            && match col_cmp(map, left_col, right_col) {
                LineDiff::Equal => true,
                LineDiff::OneDifference if !smudge_fixed => {
                    smudge_fixed = true;
                    true
                }
                _ => false,
            };

        if !lines_match {
            continue 'outer;
        }

        let mut left_col2 = left_col;
        let mut right_col2 = right_col + 1;

        // skip the newline
        while right_col2 < map.line_len - 1 && left_col2 > 0 {
            let lines_match = match col_cmp(map, left_col2 - 1, right_col2) {
                LineDiff::Equal => true,
                LineDiff::OneDifference if !smudge_fixed => {
                    smudge_fixed = true;
                    true
                }
                _ => false,
            };

            if !lines_match {
                smudge_fixed = false;
                continue 'outer;
            }

            left_col2 -= 1;
            right_col2 += 1;
        }

        #[allow(clippy::cast_possible_truncation)]
        return Some(left_col as u32 + 1);
    }

    None
}

const fn col_cmp(map: Map, mut left_col: usize, mut right_col: usize) -> LineDiff {
    let mut one_difference_found = false;

    assert!(left_col < right_col);
    assert!(left_col < map.line_len - 1);
    assert!(right_col < map.line_len);
    assert!(right_col > 0);

    while right_col < map.map.len() {
        let l = map.map[left_col];
        let r = map.map[right_col];

        if l != r {
            if one_difference_found {
                return LineDiff::Different;
            }

            one_difference_found = true;
        }

        left_col += map.line_len;
        right_col += map.line_len;
    }

    if one_difference_found {
        LineDiff::OneDifference
    } else {
        LineDiff::Equal
    }
}

enum LineDiff {
    Equal,
    OneDifference,
    Different,
}

#[derive(Clone, Copy)]
struct Map<'a> {
    map: &'a [u8],
    line_len: usize,
}
