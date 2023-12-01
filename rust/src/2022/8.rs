use core::slice;
use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    iter::{self, Enumerate, Map, Repeat, RepeatN, Rev, Zip},
    marker::PhantomData,
};

use crate::{Day, DaySolution, Input};

impl DaySolution for Day<2022, 8> {
    fn part_1() -> impl Display {
        let forest = Forest::parse(Self::INPUT);

        let mut visible_trees = forest
            .trees
            .iter()
            .map(|row| row.iter().map(|_| false).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        // uncomment the following calls to `print_bool_matrix` for some funky terminal
        // output showing the traces

        trace_for_visible_trees(forest.iter::<Column>(), &mut visible_trees);
        // print_bool_matrix(&visible_trees);

        trace_for_visible_trees(forest.iter::<(Column, Backwards)>(), &mut visible_trees);
        // print_bool_matrix(&visible_trees);

        trace_for_visible_trees(forest.iter::<Row>(), &mut visible_trees);
        // print_bool_matrix(&visible_trees);

        trace_for_visible_trees(forest.iter::<(Row, Backwards)>(), &mut visible_trees);
        // print_bool_matrix(&visible_trees);

        visible_trees.iter().flatten().filter(|b| **b).count()
    }

    fn part_2() -> impl Display {
        let forest = Forest::parse(Self::INPUT);

        let column_scores = forest
            .iter::<Column>()
            .map(calculate_scores_in_lines::<Column>);

        let row_scores = forest.iter::<Row>().map(calculate_scores_in_lines::<Row>);

        let mut tree_visibility_scores = forest
            .trees
            .iter()
            .map(|row| row.iter().map(|_| TreeScore::default()).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        for (outer_idx, iter) in column_scores {
            for (inner_idx, score) in iter {
                (outer_idx, inner_idx)
                    .index_in_to(&mut tree_visibility_scores)
                    .column = score;
            }
        }

        for (outer_idx, iter) in row_scores {
            for (inner_idx, score) in iter {
                (outer_idx, inner_idx)
                    .index_in_to(&mut tree_visibility_scores)
                    .row = score;
            }
        }

        tree_visibility_scores
            .into_iter()
            .flatten()
            .map(TreeScore::total)
            .max()
            .unwrap()
    }
}

#[allow(clippy::type_complexity)]
fn calculate_scores_in_lines<Type: ForestIterType>(
    (outer_idx, tree_line): (OuterIdxOf<Type>, TreeLineOf<'_, Type>),
) -> (
    OuterIdxOf<Type>,
    Map<Zip<TreeLineOf<'_, Type>, Repeat<TreeLineOf<'_, Type>>>, CalculateLineScoresFn<Type>>,
) {
    let line_scores = tree_line
        .clone()
        .zip(iter::repeat(tree_line.clone()))
        .map(calculate_line_scores::<Type> as _);

    (outer_idx, line_scores)
}

type CalculateLineScoresFn<Type> = fn(
    (
        (InnerIdxOf<Type>, TreeHeightOf<'_, Type>),
        TreeLineOf<'_, Type>,
    ),
) -> (InnerIdxOf<Type>, TreeScoreInLine<Type>);

fn calculate_line_scores<Type: ForestIterType>(
    ((inner_idx, tree_height), mut tree_line_clone): (
        (InnerIdxOf<Type>, TreeHeightOf<'_, Type>),
        TreeLineOf<'_, Type>,
    ),
) -> (<Type as ForestIterType>::InnerIdx, TreeScoreInLine<Type>) {
    use std::ops::ControlFlow::{Break, Continue};

    let calculate_score_on_side_of_tree =
        |acc, (curr_inner_idx, curr_height): (_, TreeHeightOf<'_, Type>)| {
            if inner_idx == curr_inner_idx {
                Break(acc)
            } else {
                Continue(if curr_height.borrow() >= tree_height.borrow() {
                    1
                } else {
                    acc + 1
                })
            }
        };

    let (Continue(in_front) | Break(in_front)) = tree_line_clone
        .by_ref()
        .try_rfold(0_u32, calculate_score_on_side_of_tree);
    let (Continue(behind) | Break(behind)) =
        tree_line_clone.try_fold(0_u32, calculate_score_on_side_of_tree);

    (
        inner_idx,
        TreeScoreInLine {
            in_front,
            behind,
            _type: PhantomData,
        },
    )
}

#[derive(Default)]
struct TreeScore {
    column: TreeScoreInLine<Column>,
    row: TreeScoreInLine<Row>,
}

impl TreeScore {
    fn total(self) -> u32 {
        self.column.in_front * self.column.behind * self.row.in_front * self.row.behind
    }
}

struct TreeScoreInLine<T: ForestIterType> {
    in_front: u32,
    behind: u32,
    _type: PhantomData<T>,
}

impl<T: ForestIterType> Debug for TreeScoreInLine<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TreeScoreInLine")
            .field("in_front", &self.in_front)
            .field("behind", &self.behind)
            .finish()
    }
}

impl<T: ForestIterType> Default for TreeScoreInLine<T> {
    fn default() -> Self {
        Self {
            in_front: u32::default(),
            behind: u32::default(),
            _type: PhantomData,
        }
    }
}

fn trace_for_visible_trees<I, II, U8, T: Copy, U: Copy>(iter: I, visible_trees: &mut [Vec<bool>])
where
    I: Iterator<Item = (T, II)>,
    II: IntoIterator<Item = (U, U8)> + Clone,
    U8: Borrow<u8> + Debug,
    (T, U): RowCol,
{
    let mut is_first_tree_line = true;
    let mut tree_lines = iter.peekable();

    while let Some((line_idx, tree_line)) = tree_lines.next() {
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
        // lines don't affect each other when "looking down"/ traversing them; all that matters is
        // that there *is* another line.
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

#[allow(dead_code)]
fn print_bool_matrix(matrix: &[Vec<bool>]) {
    let output = matrix
        .iter()
        .map(|i| {
            i.iter()
                .map(|b| if *b { "▓▓" } else { "░░" })
                .collect::<String>()
        })
        .intersperse("\n".to_owned())
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

    fn iter<TypeAndDirection: ToTypeAndDirection>(&self) -> ForestIter<'_, TypeAndDirection> {
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

impl<Type: ForestIterType, Direction: TreeLineIterationDirection> ToTypeAndDirection
    for (Type, Direction)
{
    type Type = Type;
    type Direction = Direction;
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
    type TreeLine<'a>: DoubleEndedIterator<Item = (Self::InnerIdx, Self::TreeHeight<'a>)> + Clone;

    type TreeHeight<'a>: Borrow<u8> + PartialEq + Debug;

    /// The "inner" index type
    type InnerIdx: From<usize> + PartialEq + Debug + Copy;

    /// The "outer" index type
    type OuterIdx: From<usize> + PartialEq + Debug + Copy;

    fn bound<T>(fi: &ForestIter<'_, T>) -> usize
    where
        T: ToTypeAndDirection<Type = Self>;

    fn current_line<'a, T>(fi: &ForestIter<'a, T>) -> Self::TreeLine<'a>
    where
        T: ToTypeAndDirection<Type = Self>;
}

type InnerIdxOf<T> = <T as ForestIterType>::InnerIdx;
type OuterIdxOf<T> = <T as ForestIterType>::OuterIdx;
type TreeHeightOf<'a, T> = <T as ForestIterType>::TreeHeight<'a>;
type TreeLineOf<'a, T> = <T as ForestIterType>::TreeLine<'a>;

struct Column;

impl ForestIterType for Column {
    type TreeLine<'a> = Map<
        Enumerate<Zip<slice::Iter<'a, Vec<u8>>, RepeatN<usize>>>,
        for<'f> fn((usize, (&'f Vec<u8>, usize))) -> (RowIdx, u8),
    >;

    type TreeHeight<'a> = u8;

    type InnerIdx = RowIdx;

    type OuterIdx = ColIdx;

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
        Map<Enumerate<slice::Iter<'a, u8>>, fn((usize, &'a u8)) -> (ColIdx, &'a u8)>;

    type TreeHeight<'a> = &'a u8;

    type InnerIdx = ColIdx;

    type OuterIdx = RowIdx;

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
        fi.forest.trees[fi.current]
            .iter()
            .enumerate()
            .map(|(col, height)| (ColIdx(col), height))
    }
}

trait TreeLineIter: Sized {
    type TreeLine<'a>;

    type ForestIterTypeInnerIdx;

    type ForestIterTypeOuterIdx;

    fn next_fn<'iter, 'forest: 'iter>(
        forest_iter: &'iter mut ForestIter<'forest, Self>,
    ) -> Option<(Self::ForestIterTypeOuterIdx, Self::TreeLine<'forest>)>;
}

impl<TypeAndDirection: ToTypeAndDirection> TreeLineIter for TypeAndDirection {
    type TreeLine<'a> = <TypeAndDirection::Direction as TreeLineIterationDirection>::MaybeReversed<
        <TypeAndDirection::Type as ForestIterType>::TreeLine<'a>,
    >;

    type ForestIterTypeInnerIdx = InnerIdxOf<TypeAndDirection::Type>;

    type ForestIterTypeOuterIdx = OuterIdxOf<TypeAndDirection::Type>;

    fn next_fn<'iter, 'forest: 'iter>(
        forest_iter: &'iter mut ForestIter<'forest, Self>,
    ) -> Option<(Self::ForestIterTypeOuterIdx, Self::TreeLine<'forest>)> {
        forest_iter
            .current
            .lt(&(TypeAndDirection::Type::bound(forest_iter)))
            .then(|| {
                let current = Self::ForestIterTypeOuterIdx::from(forest_iter.current);
                let tree_line = TypeAndDirection::Type::current_line(forest_iter);
                let tl = <TypeAndDirection::Direction as TreeLineIterationDirection>::maybe_reverse(
                    tree_line,
                );

                forest_iter.current += 1;

                (current, tl)
            })
    }
}

impl<'a, TypeAndDirection> Iterator for ForestIter<'a, TypeAndDirection>
where
    TypeAndDirection: ToTypeAndDirection + 'static,
{
    type Item = (
        <TypeAndDirection as TreeLineIter>::ForestIterTypeOuterIdx,
        <TypeAndDirection as TreeLineIter>::TreeLine<'a>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        TypeAndDirection::next_fn(self)
    }
}
