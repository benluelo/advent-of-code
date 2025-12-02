#![feature(
    // iterator hackery
    iter_array_chunks,
    iter_next_chunk,
    iter_intersperse,

    int_from_ascii,

    // arrays
    array_windows,
    maybe_uninit_slice,

    // slices
    slice_split_once,

    // numbers
    int_roundings,

    // cool and handy
    trace_macros,

    stmt_expr_attributes,
    proc_macro_hygiene,
)]
#![recursion_limit = "512"]
#![allow(long_running_const_eval, clippy::too_many_lines)]

#[cfg(windows)]
compile_error!("windows is not supported");

#[path = "2021/mod.rs"]
mod year_2021;
#[path = "2022/mod.rs"]
mod year_2022;
#[path = "2023/mod.rs"]
mod year_2023;
#[path = "2024/mod.rs"]
mod year_2024;
#[path = "2025/mod.rs"]
mod year_2025;

pub mod displayable;
pub mod utils;

extern crate alloc;
extern crate core;

use crate::{
    displayable::Displayable,
    utils::{arr, concat_array_const, itoa, utf8},
};

struct Day<const YEAR: u16, const DAY: u8>;

#[cfg(not(feature = "const"))]
fn main() {
    let mut args = std::env::args();

    let 3 = args.len() else {
        print!(
            "usage: {} <day> <path-to-input>\navailable days:\n",
            args.next().unwrap()
        );

        macro_rules! solution_name {
            ($YEAR:literal, $DAY:literal) => {{
                const PREFIX: &'static str = {
                    concat_array_const! {
                        const PREFIX: [u8; _] = arr!(itoa!($YEAR as u32).as_str().as_bytes()), *b"/", arr!(itoa!($DAY as u32).as_str().as_bytes());
                    };

                    utf8(&PREFIX)
                };

                println!("{PREFIX}");
            }};
        }

        for_each_day! { solution_name };

        return;
    };

    args.next();

    let day = args.next().unwrap();
    let path = args.next().unwrap();

    #[cfg(not(feature = "const"))]
    macro_rules! run_solution {
        ($YEAR:literal, $DAY:literal) => {{
            const ID: &'static str = {
                concat_array_const! {
                    const ID: [u8; _] = arr!(itoa!($YEAR as u32).as_str().as_bytes()), *b"/", arr!(itoa!($DAY as u32).as_str().as_bytes());
                }

                utf8(&ID)
            };

            if &day == ID {
                let input = std::fs::read_to_string(path).unwrap();

                let part_1 = Day::<$YEAR, $DAY>::parse(&mut input.clone().into_bytes());

                println!("{ID}-1: {}", Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part1>::new(part_1).as_str());

                let part_2 = Day::<$YEAR, $DAY>::parse2(&mut input.into_bytes());

                println!("{ID}-1: {}", Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part2>::new(part_2).as_str());

                return;
            }
        }};
    }

    for_each_day! {
        run_solution
    };

    #[cfg(not(feature = "const"))]
    {
        println!("day `{day}` not found\n");
    }
}

#[cfg(feature = "const")]
fn main() {
    macro_rules! run_solution {
        ($YEAR:literal, $DAY:literal) => {{
            const SOLUTION_PART_1: &str = const {
                Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part1>::new(
                    Day::<$YEAR, $DAY>::parse(&mut arr!(Day::<$YEAR, $DAY>::INPUT.as_bytes())),
                )
            }
            .as_str();
            const SOLUTION_PART_2: &str = const {
                const {
                    Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part2>::new(
                        Day::<$YEAR, $DAY>::parse2(&mut arr!(Day::<$YEAR, $DAY>::INPUT.as_bytes())),
                    )
                }
                .as_str()
            };

            concat_array_const! {
                const PREFIX: [u8; _] = arr!(itoa!($YEAR as u32).as_str().as_bytes()), *b"/", arr!(itoa!($DAY as u32).as_str().as_bytes()), *b"-";
                const PART_1: [u8; _] = *b"1: ", arr!(SOLUTION_PART_1.as_bytes()), *b"\n";
                const PART_2: [u8; _] = *b"2: ", arr!(SOLUTION_PART_2.as_bytes()), *b"\n";
                const OUTPUT: [u8; _] = PREFIX, PART_1, PREFIX, PART_2;
            }

            print!("{}", utf8(&OUTPUT));
        }};
    }

    for_each_day! {
        run_solution
    };
}

#[cfg(any(feature = "const", test))]
const _: () = {
    macro_rules! static_input {
        ($YEAR:literal, $DAY:literal) => {
            impl Day<$YEAR, $DAY> {
                #[allow(unused)]
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

    for_each_day! { static_input }
};

#[cfg(not(any(feature = "const", test)))]
const _: () = {
    macro_rules! static_input {
        ($YEAR:literal, $DAY:literal) => {
            impl Day<$YEAR, $DAY> {
                #[allow(unused)]
                const INPUT: &'static str = "";
            }
        };
    }

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
            @YEARS (2021,2022,2023,2024)
            (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
        }

        for_each_day! {
            $f
            @YEARS (2025)
            (1,2,3,4,5,6,7,8,9,10,11,12)
        }
    };

    (
        $f:ident
        @YEARS ($($YEAR:literal),+)
        $DAYS:tt
    ) => {
        $(
            for_each_day! {
                $f
                @DAYS
                $YEAR,
                $DAYS
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

pub trait SolutionTypes {
    type Part1;
    type Part2;
}

#[macro_export]
macro_rules! day {
    // const impls
    (
        $(#[cfg($meta:meta)])*
        impl $Ty:ty {
            pub const fn parse($($p1_args:tt)*) -> $P1Ret:ty $p1_body:block
            pub const fn parse2($($p2_args:tt)*) -> $P2Ret:ty $p2_body:block
        }
    ) => {
        $(#[cfg($meta)])*
        impl $Ty {
            pub const fn parse($($p1_args)*) -> $P1Ret $p1_body
            pub const fn parse2($($p2_args)*) -> $P2Ret $p2_body
        }

        $(#[cfg($meta)])*
        impl $crate::SolutionTypes for $Ty {
            type Part1 = $P1Ret;
            type Part2 = $P2Ret;
        }

        #[allow(unused)]
        type Today = $Ty;
    };

    // non-const impls
    (
        $(#[cfg($meta:meta)])*
        impl Day<$DAY:literal, $YEAR:literal> {
            pub fn parse($($p1_args:tt)*) -> $P1Ret:ty $p1_body:block
            pub fn parse2($($p2_args:tt)*) -> $P2Ret:ty $p2_body:block
        }
    ) => {
        #[cfg(not(feature = "const"))]
        $(#[cfg($meta)])*
        impl Day<$DAY, $YEAR> {
            pub fn parse($($p1_args)*) -> $P1Ret $p1_body
            pub fn parse2($($p2_args)*) -> $P2Ret $p2_body
        }

        #[cfg(not(feature = "const"))]
        $(#[cfg($meta)])*
        impl $crate::SolutionTypes for Day<$DAY, $YEAR> {
            type Part1 = $P1Ret;
            type Part2 = $P2Ret;
        }

        #[cfg(feature = "const")]
        $(#[cfg($meta)])*
        impl Day<$DAY, $YEAR> {
            #[allow(unused_variables)]
            pub const fn parse($($p1_args)*) -> &'static str {
                let _ = || $p1_body;

                "no const solution"
            }

            #[allow(unused_variables)]
            pub const fn parse2($($p2_args)*) -> &'static str {
                let _ = || $p2_body;

                "no const solution"
            }
        }

        #[cfg(feature = "const")]
        $(#[cfg($meta)])*
        impl $crate::SolutionTypes for Day<$DAY, $YEAR> {
            type Part1 =  &'static str;
            type Part2 =  &'static str;
        }

        #[allow(unused)]
        type Today = Day<$DAY, $YEAR>;
    };
}
