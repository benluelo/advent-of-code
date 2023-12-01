// #![cfg_attr(not(feature = "std"), no_std)]
#![no_main]
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

#[path = "2022/mod.rs"]
mod year_2022;
#[path = "2023/mod.rs"]
mod year_2023;

use core::fmt::Display;
use std::{fs::File, io::Write, os::unix::io::FromRawFd};

#[no_mangle]
pub extern "Rust" fn main(_argc: i32, _argv: *const *const u8) {
    let mut stdout = unsafe { File::from_raw_fd(1) };

    macro_rules! run_solution {
        ($YEAR:literal, $DAY:literal) => {
            solve::<$YEAR, $DAY>(&mut stdout);
        };
    }

    for_each_day! {
        run_solution
    };
}

#[inline]
fn solve<const YEAR: u16, const DAY: u8>(stdout: &mut File)
where
    Day<YEAR, DAY>: DaySolution,
{
    writeln!(stdout, "{}/{}-1: {}", YEAR, DAY, Day::<YEAR, DAY>::part_1()).unwrap();
    writeln!(stdout, "{}/{}-2: {}", YEAR, DAY, Day::<YEAR, DAY>::part_2()).unwrap();
}

struct Day<const YEAR: u16, const DAY: u8>;

/// An abstraction over [`Day`] allowing it to be used in generic contexts.
trait DaySolution: Input {
    fn part_1() -> impl Display;
    fn part_2() -> impl Display;
}

trait Input {
    const INPUT: &'static str;
}

macro_rules! static_input {
    ($YEAR:literal, $DAY:literal) => {
        impl Input for Day<$YEAR, $DAY> {
            const INPUT: &'static str = include_str!(concat!(
                "../../inputs/",
                stringify!($YEAR),
                "/",
                stringify!($DAY),
                ".txt"
            ));
        }
    };
}

const _: () = {
    for_each_day! { static_input }
};

/// Calls `$f` for every (year, day) pair that's feature is enabled.
#[macro_export]
macro_rules! for_each_day {
    (
        $f:ident
    ) => {
        for_each_day! {
            $f
            @YEARS (2022,2023)
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
