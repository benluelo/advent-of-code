use core::slice;
use std::{
    borrow::Borrow,
    convert,
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

    // trace(forest.iter::<Column>(), &mut visible_trees);
    // print_bool_matrix(&visible_trees);

    // trace(forest.iter::<Reversed<Column>>(), &mut visible_trees);
    // print_bool_matrix(&visible_trees);

    trace(forest.iter::<Row>(), &mut visible_trees);
    print_bool_matrix(&visible_trees);

    trace(forest.iter::<Reversed<Row>>(), &mut visible_trees);
    print_bool_matrix(&visible_trees);

    dbg!(visible_trees.iter().flatten().count());

    visible_trees.iter().flatten().filter(|b| **b).count()
}

fn trace<I, II, U8>(iter: I, visible_trees: &mut [Vec<bool>])
where
    I: Iterator<Item = (usize, II)>,
    II: IntoIterator<Item = (usize, U8)> + Clone,
    U8: Borrow<u8> + Debug,
{
    for (line_idx, tree_line) in iter {
        dbg!(tree_line.clone().into_iter().collect::<Vec<_>>());
        let mut is_first = true;
        let mut peekable = tree_line.into_iter().peekable();
        let mut highest_tree_so_far: u8 = 0;
        while let Some((tree_idx, tree)) = peekable.next() {
            if is_first {
                is_first = false;
                // it's on the edge, therefore visible
                visible_trees[tree_idx][line_idx] = true;
            }

            highest_tree_so_far = std::cmp::max(*tree.borrow(), highest_tree_so_far);

            if let Some((next_idx, next)) = peekable.peek() {
                dbg!((next_idx, line_idx));
                if !visible_trees[*next_idx][line_idx] {
                    visible_trees[*next_idx][line_idx] = highest_tree_so_far < *next.borrow();
                }
            } else {
                // it's on the edge, therefore visible
                visible_trees[tree_idx][line_idx] = true;
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
    // assumes that the input is rectangular (i.e. all the lines are the same width).
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

    #[allow(clippy::iter_not_returning_iterator)] // yes it does
    fn iter<Type: ForestIterType>(&self) -> ForestIter<'_, Type> {
        ForestIter {
            forest: self,
            current: 0,
            _type: PhantomData,
        }
    }
}

struct ForestIter<'a, Type> {
    forest: &'a Forest,
    // ensure to start at 0 and increment *before* indexing
    current: usize,
    _type: PhantomData<Type>,
}

trait ForestIterType {}

struct Column;
impl ForestIterType for Column {}

struct Row;
impl ForestIterType for Row {}

struct Reversed<Type>(PhantomData<Type>);
impl<Type: ForestIterType> ForestIterType for Reversed<Type> {}

trait Is<T> {}
impl<Type> Is<Type> for Reversed<Type> {}
impl<Type> Is<Type> for Type {}

type ColumnMapFn = for<'f> fn((usize, (&'f Vec<u8>, usize))) -> (usize, u8);

impl<'a> Iterator for ForestIter<'a, Column> {
    type Item = (
        usize,
        Map<Enumerate<Zip<slice::Iter<'a, Vec<u8>>, RepeatN<usize>>>, ColumnMapFn>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.forest.width - 2 {
            self.current += 1;
            let map = self.current_column();

            Some((self.current, map))
        } else {
            None
        }
    }
}

impl<'a> Iterator for ForestIter<'a, Reversed<Column>> {
    type Item = (
        usize,
        Rev<Map<Enumerate<Zip<slice::Iter<'a, Vec<u8>>, RepeatN<usize>>>, ColumnMapFn>>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.forest.width - 2 {
            self.current += 1;
            let idx = self.current;
            let map = self.current_column().rev();

            Some((idx, map))
        } else {
            None
        }
    }
}

#[allow(clippy::type_complexity)]
impl<'a, Type: Is<Column>> ForestIter<'a, Type> {
    fn current_column(
        &self,
    ) -> Map<Enumerate<Zip<slice::Iter<'a, Vec<u8>>, RepeatN<usize>>>, ColumnMapFn> {
        self.forest
            .trees
            .iter()
            .zip(iter::repeat_n(self.current, self.forest.height))
            .enumerate()
            .map((|(row, (vec, i))| (row, vec[i])) as for<'f> fn((_, (&'f Vec<_>, _))) -> _)
    }
}

impl<'a> Iterator for ForestIter<'a, Reversed<Row>> {
    type Item = (usize, Rev<Enumerate<slice::Iter<'a, u8>>>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.forest.height - 2 {
            self.current += 1;
            let idx = self.current;
            let map = self.current_row().rev();

            Some((idx, map))
        } else {
            None
        }
    }
}

impl<'a> Iterator for ForestIter<'a, Row> {
    type Item = (usize, Enumerate<slice::Iter<'a, u8>>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.forest.height - 2 {
            self.current += 1;
            Some((self.current, self.current_row()))
        } else {
            None
        }
    }
}

impl<'a, Type: Is<Row>> ForestIter<'a, Type> {
    fn current_row(&mut self) -> Enumerate<slice::Iter<'a, u8>> {
        self.forest
            .trees
            .get(self.current)
            .unwrap()
            .iter()
            .enumerate()
    }
}

#[cfg(test)]
mod test {
    use crate::day_8::{print_bool_matrix, trace, Column, Reversed, Row};

    use super::Forest;

    const INPUT: &str = "
123456789
234567891
345678912
456789123
567891234
678912345";

    fn collect_column<I>((i, iter): (usize, I)) -> (usize, Vec<(usize, u8)>)
    where
        I: Iterator<Item = (usize, u8)>,
    {
        (i, iter.collect::<Vec<_>>())
    }

    fn collect_row<'a, I>((i, iter): (usize, I)) -> (usize, Vec<(usize, u8)>)
    where
        I: Iterator<Item = (usize, &'a u8)>,
    {
        (i, iter.map(|(a, &b)| (a, b)).collect::<Vec<_>>())
    }

    #[test]
    pub(crate) fn column_iter() {
        let column_iter = Forest::parse(INPUT)
            .iter::<Column>()
            .map(collect_column)
            .collect::<Vec<_>>();

        assert_eq!(
            column_iter,
            vec![
                (1, vec![(0, 2), (1, 3), (2, 4), (3, 5), (4, 6), (5, 7)]),
                (2, vec![(0, 3), (1, 4), (2, 5), (3, 6), (4, 7), (5, 8)]),
                (3, vec![(0, 4), (1, 5), (2, 6), (3, 7), (4, 8), (5, 9)]),
                (4, vec![(0, 5), (1, 6), (2, 7), (3, 8), (4, 9), (5, 1)]),
                (5, vec![(0, 6), (1, 7), (2, 8), (3, 9), (4, 1), (5, 2)]),
                (6, vec![(0, 7), (1, 8), (2, 9), (3, 1), (4, 2), (5, 3)]),
                (7, vec![(0, 8), (1, 9), (2, 1), (3, 2), (4, 3), (5, 4)]),
            ]
        );
    }

    #[test]
    pub(crate) fn column_iter_iteration() {
        let column_iter = Forest::parse(INPUT)
            .iter::<Reversed<Column>>()
            .map(collect_column)
            .collect::<Vec<_>>();

        #[rustfmt::skip]
        assert_eq!(
            column_iter,
            vec![
                (1, [(0, 2), (1, 3), (2, 4), (3, 5), (4, 6), (5, 7)].into_iter().rev().collect()),
                (2, [(0, 3), (1, 4), (2, 5), (3, 6), (4, 7), (5, 8)].into_iter().rev().collect()),
                (3, [(0, 4), (1, 5), (2, 6), (3, 7), (4, 8), (5, 9)].into_iter().rev().collect()),
                (4, [(0, 5), (1, 6), (2, 7), (3, 8), (4, 9), (5, 1)].into_iter().rev().collect()),
                (5, [(0, 6), (1, 7), (2, 8), (3, 9), (4, 1), (5, 2)].into_iter().rev().collect()),
                (6, [(0, 7), (1, 8), (2, 9), (3, 1), (4, 2), (5, 3)].into_iter().rev().collect()),
                (7, [(0, 8), (1, 9), (2, 1), (3, 2), (4, 3), (5, 4)].into_iter().rev().collect()),
            ]
        );
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
            forest.iter::<Reversed<Column>>(),
            &mut visible_trees_with_reversed,
        );

        assert_ne!(visible_trees, visible_trees_with_reversed);

        print_bool_matrix(&visible_trees);
        println!();
        print_bool_matrix(&visible_trees_with_reversed);
    }

    const _: &str = "
123456789
234567891
345678912
456789123
567891234
678912345";

    #[test]
    pub(crate) fn row_iter() {
        let row_iter = Forest::parse(INPUT)
            .iter::<Row>()
            .map(collect_row)
            .collect::<Vec<_>>();

        #[rustfmt::skip]
        assert_eq!(
            row_iter,
            vec![
                (1, [2, 3, 4, 5, 6, 7, 8, 9, 1].into_iter().enumerate().collect()),
                (2, [3, 4, 5, 6, 7, 8, 9, 1, 2].into_iter().enumerate().collect()),
                (3, [4, 5, 6, 7, 8, 9, 1, 2, 3].into_iter().enumerate().collect()),
                (4, [5, 6, 7, 8, 9, 1, 2, 3, 4].into_iter().enumerate().collect()),
            ]
        );
    }

    #[test]
    pub(crate) fn row_iter_iteration() {
        let row_iter = Forest::parse(INPUT)
            .iter::<Reversed<Row>>()
            .map(collect_row)
            .collect::<Vec<_>>();

        #[rustfmt::skip]
        assert_eq!(
            row_iter,
            vec![
                (1, [2, 3, 4, 5, 6, 7, 8, 9, 1].into_iter().enumerate().rev().collect()),
                (2, [3, 4, 5, 6, 7, 8, 9, 1, 2].into_iter().enumerate().rev().collect()),
                (3, [4, 5, 6, 7, 8, 9, 1, 2, 3].into_iter().enumerate().rev().collect()),
                (4, [5, 6, 7, 8, 9, 1, 2, 3, 4].into_iter().enumerate().rev().collect()),
            ]
        );
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
            forest.iter::<Reversed<Row>>(),
            &mut visible_trees_with_reversed,
        );

        assert_ne!(visible_trees, visible_trees_with_reversed);

        print_bool_matrix(&visible_trees);
        println!();
        print_bool_matrix(&visible_trees_with_reversed);
    }
}
