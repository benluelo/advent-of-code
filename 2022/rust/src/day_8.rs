use core::slice;
use std::{
    borrow::Borrow,
    fmt::Debug,
    iter::{self, Enumerate, Map, RepeatN, Rev, Zip},
    marker::PhantomData,
};

pub fn solution(input: &str) -> usize {
    let forest = Forest::parse(input);

    let mut visible_trees = forest
        .trees
        .iter()
        .map(|row| row.iter().map(|_| false).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    print_bool_matrix(&visible_trees);

    trace(forest.iter::<Column>(), &mut visible_trees);
    print_bool_matrix(&visible_trees);

    trace(forest.iter::<(Column, Backwards)>(), &mut visible_trees);
    print_bool_matrix(&visible_trees);

    trace(forest.iter::<Row>(), &mut visible_trees);
    print_bool_matrix(&visible_trees);

    trace(forest.iter::<(Row, Backwards)>(), &mut visible_trees);
    print_bool_matrix(&visible_trees);

    dbg!(visible_trees.iter().flatten().count());
    dbg!(forest.height, forest.width);

    visible_trees.iter().flatten().filter(|b| **b).count()
}

fn trace<I, II, U8, T: Copy, U: Copy>(iter: I, visible_trees: &mut [Vec<bool>])
where
    I: Iterator<Item = (T, II)>,
    II: IntoIterator<Item = (U, U8)> + Clone,
    U8: Borrow<u8> + Debug,
    (T, U): RowCol,
{
    let mut is_first_tree_line = true;
    let mut tree_lines = iter.peekable();

    while let Some((line_idx, tree_line)) = tree_lines.next() {
        // dbg!(tree_line.clone().into_iter().collect::<Vec<_>>());
        // panic!();

        let mut tree_line = tree_line.into_iter().peekable();

        // CASE: first tree line - all visible
        if is_first_tree_line {
            // the whole line is visible
            for (tree_idx, _tree_height) in tree_line {
                // it's on the edge, therefore visible
                *(line_idx, tree_idx).index_in_to(visible_trees) = true;
                continue;
            }

            is_first_tree_line = false;
        }
        // CASE: not the last line
        // NOTE: the actual values in the next (i.e. peeked) line aren't important, since parallel
        // lines don't affect each other when "looking down" them; all that matters is that there
        // *is* another line.
        else if tree_lines.peek().is_some() {
            // state for the following loop:
            let mut is_first_tree = true;
            let mut highest_tree_so_far: u8 = 0;

            while let Some((tree_idx, tree_height)) = tree_line.next() {
                if is_first_tree {
                    is_first_tree = false;
                    // it's on the edge, therefore visible
                    *(line_idx, tree_idx).index_in_to(visible_trees) = true;
                }

                highest_tree_so_far = std::cmp::max(*tree_height.borrow(), highest_tree_so_far);

                if let Some((next_idx, next)) = tree_line.peek() {
                    let next_tree_visibility = (line_idx, *next_idx).index_in_to(visible_trees);
                    if !*next_tree_visibility {
                        *next_tree_visibility = highest_tree_so_far < *next.borrow();
                    }
                } else {
                    // it's on the edge, therefore visible
                    *(line_idx, tree_idx).index_in_to(visible_trees) = true;
                }
            }
        }
        // CASE: last line, since there is no following line as per check above
        else {
            for (tree_idx, _tree_height) in tree_line {
                // it's on the edge, therefore visible
                *(line_idx, tree_idx).index_in_to(visible_trees) = true;
            }
        }
    }
}

fn print_bool_matrix(matrix: &[Vec<bool>]) {
    let output = matrix
        .iter()
        .map(|i| {
            i.iter()
                .map(|b| if *b { "▓▓" } else { "░░" })
                .collect::<String>()
        })
        .intersperse("\n".to_string())
        .collect::<String>();

    println!("{output}\n");
}

/// Represents a forest in this form:
///
/// ```
/// 123456
/// 789012
/// 345678
/// 901234
/// ```
struct Forest {
    trees: Vec<Vec<u8>>,
    // visible_trees: Vec<Vec<bool>>,
    height: usize,
    width: usize,
}

impl Forest {
    // assumes that the input is rectangular (i.e. all the lines are the same
    // width).
    fn parse(input: &str) -> Self {
        let trees = input
            .trim()
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| c.to_digit(10).unwrap().try_into().unwrap())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        Self {
            height: trees.len(),
            width: trees[0].len(),
            trees,
        }
    }

    fn iter<How: ToTypeAndDirection>(&self) -> ForestIter<'_, How> {
        ForestIter {
            forest: self,
            current: 0,
            _type: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RowIdx(usize);
impl From<usize> for RowIdx {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ColIdx(usize);
impl From<usize> for ColIdx {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

// >:)
trait RowCol {
    fn row(&self) -> RowIdx;
    fn col(&self) -> ColIdx;

    fn index_in_to<'a, 'b: 'a, T>(&'a self, matrix: &'b mut [Vec<T>]) -> &'b mut T {
        &mut matrix[self.row().0][self.col().0]
    }
}

impl RowCol for (RowIdx, ColIdx) {
    fn row(&self) -> RowIdx {
        self.0
    }

    fn col(&self) -> ColIdx {
        self.1
    }
}

impl RowCol for (ColIdx, RowIdx) {
    fn row(&self) -> RowIdx {
        self.1
    }

    fn col(&self) -> ColIdx {
        self.0
    }
}

trait ToTypeAndDirection {
    type Type: ForestIterType;
    type Direction: TreeLineIterationDirection;
}

impl<T: ForestIterType> ToTypeAndDirection for T {
    type Type = T;
    type Direction = Forwards;
}

impl<T: ForestIterType, D: TreeLineIterationDirection> ToTypeAndDirection for (T, D) {
    type Type = T;
    type Direction = D;
}

enum Forwards {}
enum Backwards {}

trait TreeLineIterationDirection {
    type MaybeReversed<TreeLine>;

    fn maybe_reverse<TreeLine: DoubleEndedIterator>(tl: TreeLine) -> Self::MaybeReversed<TreeLine>;
}

impl TreeLineIterationDirection for Forwards {
    type MaybeReversed<TreeLine> = TreeLine;

    fn maybe_reverse<TreeLine: DoubleEndedIterator>(tl: TreeLine) -> Self::MaybeReversed<TreeLine> {
        tl
    }
}

impl TreeLineIterationDirection for Backwards {
    type MaybeReversed<TreeLine> = Rev<TreeLine>;

    fn maybe_reverse<TreeLine: DoubleEndedIterator>(tl: TreeLine) -> Self::MaybeReversed<TreeLine> {
        tl.rev()
    }
}

struct ForestIter<'a, Type> {
    forest: &'a Forest,
    current: usize,
    _type: PhantomData<Type>,
}

trait ForestIterType: Sized {
    type TreeLine<'a>: DoubleEndedIterator;

    type OtherIdx: From<usize>;

    fn bound<T>(fi: &ForestIter<'_, T>) -> usize
    where
        T: ToTypeAndDirection<Type = Self>;

    fn current_line<'a, T>(fi: &ForestIter<'a, T>) -> Self::TreeLine<'a>
    where
        T: ToTypeAndDirection<Type = Self>;
}

struct Column;

type ColumnMapFn = for<'f> fn((usize, (&'f Vec<u8>, usize))) -> (RowIdx, u8);

impl ForestIterType for Column {
    type TreeLine<'a> = Map<Enumerate<Zip<slice::Iter<'a, Vec<u8>>, RepeatN<usize>>>, ColumnMapFn>;

    type OtherIdx = ColIdx;

    fn bound<T>(fi: &ForestIter<'_, T>) -> usize
    where
        T: ToTypeAndDirection<Type = Self>,
    {
        fi.forest.width
    }

    fn current_line<'a, T>(fi: &ForestIter<'a, T>) -> Self::TreeLine<'a>
    where
        T: ToTypeAndDirection<Type = Self>,
    {
        fi.forest
            .trees
            .iter()
            .zip(iter::repeat_n(fi.current, fi.forest.height))
            .enumerate()
            .map(|(row, (vec, i))| (RowIdx(row), vec[i]))
    }
}

struct Row;

impl ForestIterType for Row {
    type TreeLine<'a> =
        Map<Enumerate<slice::Iter<'a, u8>>, for<'f> fn((usize, &'f u8)) -> (ColIdx, &'f u8)>;

    type OtherIdx = RowIdx;

    fn bound<T>(fi: &ForestIter<'_, T>) -> usize
    where
        T: ToTypeAndDirection<Type = Self>,
    {
        fi.forest.height
    }

    fn current_line<'a, T>(fi: &ForestIter<'a, T>) -> Self::TreeLine<'a>
    where
        T: ToTypeAndDirection<Type = Self>,
    {
        fi.forest
            .trees
            .get(fi.current)
            .unwrap()
            .iter()
            .enumerate()
            .map(|(col, vec)| (ColIdx(col), vec))
    }
}

trait TreeLineIter: Sized {
    type TreeLine<'a>;

    type OtherIdx;

    fn next_fn<'iter, 'forest: 'iter>(
        forest_iter: &'iter mut ForestIter<'forest, Self>,
    ) -> Option<(Self::OtherIdx, Self::TreeLine<'forest>)>;
}

impl<How: ToTypeAndDirection> TreeLineIter for How {
    type TreeLine<'a> = <How::Direction as TreeLineIterationDirection>::MaybeReversed<
        <How::Type as ForestIterType>::TreeLine<'a>,
    >;

    type OtherIdx = <How::Type as ForestIterType>::OtherIdx;

    fn next_fn<'iter, 'forest: 'iter>(
        forest_iter: &'iter mut ForestIter<'forest, Self>,
    ) -> Option<(Self::OtherIdx, Self::TreeLine<'forest>)> {
        forest_iter
            .current
            .lt(&(How::Type::bound(forest_iter)))
            .then(|| {
                let current = Self::OtherIdx::from(forest_iter.current);
                let tree_line = How::Type::current_line(forest_iter);
                let tl = <How::Direction as TreeLineIterationDirection>::maybe_reverse(tree_line);

                forest_iter.current += 1;

                (current, tl)
            })
    }
}

impl<'a, How> Iterator for ForestIter<'a, How>
where
    How: ToTypeAndDirection + 'static,
{
    type Item = (
        <How as TreeLineIter>::OtherIdx,
        <How as TreeLineIter>::TreeLine<'a>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        How::next_fn(self)
    }
}

#[cfg(test)]
mod test {
    use crate::day_8::{print_bool_matrix, trace, Backwards, Column, Row};

    use super::{ColIdx, Forest, RowIdx};

    const INPUT: &str = "
123
789
345
912";

    fn collect_column<I>((i, iter): (ColIdx, I)) -> (ColIdx, Vec<(RowIdx, u8)>)
    where
        I: Iterator<Item = (RowIdx, u8)>,
    {
        (i, iter.collect::<Vec<_>>())
    }

    fn collect_row<'a, I>((i, iter): (RowIdx, I)) -> (RowIdx, Vec<(ColIdx, u8)>)
    where
        I: Iterator<Item = (ColIdx, &'a u8)>,
    {
        (i, iter.map(|(a, &b)| (a, b)).collect::<Vec<_>>())
    }

    #[test]
    pub(crate) fn column_iter() {
        let forest = Forest::parse(INPUT);
        let column_iter = forest
            .iter::<Column>()
            .map(collect_column)
            .collect::<Vec<_>>();

        // assert_eq!(
        //     column_iter,
        //     vec![
        //         // row 0, column 0 = 1
        //         // row 0, column 1 = 7
        //         (0, vec![(0, 1), (1, 7), (2, 3), (3, 9)]),
        //         (1, vec![(0, 2), (1, 8), (2, 4), (3, 1)]),
        //         (2, vec![(0, 3), (1, 9), (2, 5), (3, 2)]),
        //     ]
        // );

        for (i, tl) in column_iter {
            for (j, height) in tl {
                assert_eq!(forest.trees[j.0][i.0], height);
            }
        }
    }

    #[test]
    pub(crate) fn column_iter_iteration() {
        let column_iter = Forest::parse(INPUT)
            .iter::<(Column, Backwards)>()
            .map(collect_column)
            .collect::<Vec<_>>();

        assert_eq!(column_iter, vec![]);
    }

    #[test]
    pub(crate) fn column_trace() {
        let forest = Forest::parse(INPUT);

        let mut visible_trees = forest
            .trees
            .iter()
            .map(|row| row.iter().map(|_| true).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        trace(forest.iter::<Column>(), &mut visible_trees);

        let mut visible_trees_with_reversed = forest
            .trees
            .iter()
            .map(|row| row.iter().map(|_| true).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        trace(
            forest.iter::<(Column, Backwards)>(),
            &mut visible_trees_with_reversed,
        );

        assert_ne!(visible_trees, visible_trees_with_reversed);

        print_bool_matrix(&visible_trees);
        println!();
        print_bool_matrix(&visible_trees_with_reversed);
    }

    #[test]
    pub(crate) fn row_iter() {
        let forest = Forest::parse(INPUT);
        let row_iter = forest.iter::<Row>().map(collect_row).collect::<Vec<_>>();

        // assert_eq!(row_iter, vec![]);

        for (row_idx, tree_line) in row_iter {
            for (col_idx, height) in tree_line {
                assert_eq!(forest.trees[row_idx.0][col_idx.0], height);
            }
        }
    }

    #[test]
    pub(crate) fn row_iter_iteration() {
        let row_iter = Forest::parse(INPUT)
            .iter::<(Row, Backwards)>()
            .map(collect_row)
            .collect::<Vec<_>>();

        assert_eq!(row_iter, vec![]);
    }

    #[test]
    pub(crate) fn row_trace() {
        let forest = Forest::parse(INPUT);

        let mut visible_trees = forest
            .trees
            .iter()
            .map(|row| row.iter().map(|_| true).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        trace(forest.iter::<Row>(), &mut visible_trees);

        let mut visible_trees_with_reversed = forest
            .trees
            .iter()
            .map(|row| row.iter().map(|_| true).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        trace(
            forest.iter::<(Row, Backwards)>(),
            &mut visible_trees_with_reversed,
        );

        assert_ne!(visible_trees, visible_trees_with_reversed);

        print_bool_matrix(&visible_trees);
        println!();
        print_bool_matrix(&visible_trees_with_reversed);
    }
}
