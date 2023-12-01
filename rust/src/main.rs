#![feature(
    iter_array_chunks,
    iter_next_chunk,
    array_windows,
    iter_intersperse,
    iter_repeat_n,
    as_array_of_cells,
    int_roundings,
    control_flow_enum,
    lint_reasons,
    trace_macros
)]
#![warn(clippy::pedantic)]
#![allow(dead_code, unused_macros)]

#[path = "2022/mod.rs"]
mod year_2022;

use std::fmt::Display;

fn main() {
    for_each_day! {
        run_solution
    };
}

#[macro_export]
macro_rules! run_solution {
    ($YEAR:literal, $DAY:literal) => {
        solve::<$YEAR, $DAY>();
    };
}

struct Day<const YEAR: u16, const DAY: u8>;

/// An abstraction over [`Day`] allowing it to be used in generic contexts.
trait DaySolution {
    type Part1Output: Display;
    type Part2Output: Display;

    fn part_1(input: &str) -> Self::Part1Output;
    fn part_2(input: &str) -> Self::Part2Output;
}

#[cfg(not(feature = "static-inputs"))]
fn input<const YEAR: u16, const DAY: u8>() -> String {
    std::fs::read_to_string(format!("../inputs/{YEAR}/{DAY}.txt")).unwrap()
}

#[cfg(feature = "static-inputs")]
const fn input<const YEAR: u16, const DAY: u8>() -> &'static str {
    macro_rules! static_input {
        ($YEAR:literal, $DAY:literal) => {
            if YEAR == $YEAR && DAY == $DAY {
                return include_str!(concat!(
                    "../../inputs/",
                    stringify!($YEAR),
                    "/",
                    stringify!($DAY),
                    ".txt"
                ));
            }
        };
    }

    for_each_day! { static_input }

    unreachable!();
}

/// Calls `$f` for every (year, day) pair that's feature is enabled.
#[macro_export]
macro_rules! for_each_day {
    (
        $f:ident
    ) => {
        for_each_day! {
            $f
            @YEARS (2022)
        }
    };

    (
        $f:ident
        @YEARS ($($YEAR:literal),+)
    ) => {
        $(
            for_each_day! {
                $f
                @DAYS
                $YEAR,
                (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
            }
        )+
    };

    (
        $f:ident
        @DAYS $YEAR:literal,
        ($($DAY:literal),+)
    ) => {
        $(
            // block so that the generated macro doesn't conflict with itself
            {
                #[cfg_proc::concat_cfg(feature = $YEAR + "-" + $DAY)]
                macro_rules! generated {
                    () => {
                        $f! { $YEAR, $DAY }
                    };
                }

                #[cfg_proc::concat_cfg(not(feature = $YEAR + "-" + $DAY))]
                macro_rules! generated {
                    () => {};
                }

                generated! {};
            }
        )+
    };
}

fn solve<const YEAR: u16, const DAY: u8>()
where
    Day<YEAR, DAY>: DaySolution,
{
    let input = input::<YEAR, DAY>();

    #[cfg_attr(feature = "static-inputs", expect(clippy::needless_borrow))]
    {
        println!("{YEAR}/{DAY}-1: {}", Day::<YEAR, DAY>::part_1(&input));
        println!("{YEAR}/{DAY}-2: {}", Day::<YEAR, DAY>::part_2(&input));
    }
}
