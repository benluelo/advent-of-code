// #![cfg_attr(not(any(test, target_arch = "aarch64")), no_main)]
#![cfg_attr(not(any(test)), no_main)]
// #![cfg_attr(target_arch = "aarch64", feature(start))]
#![cfg_attr(not(any(test, debug_assertions)), no_std)]
#![feature(
    // iterator hackery
    iter_array_chunks,
    iter_next_chunk,
    iter_intersperse,
    iter_repeat_n,

    // arrays
    array_windows,
    as_array_of_cells,

    // numbers
    int_roundings,
    isqrt,

    // misc
    control_flow_enum,
    byte_slice_trim_ascii,

    // cool and handy
    lint_reasons,
    trace_macros,

    // const stuff
    const_mut_refs,
    const_slice_split_at_mut
)]

#[cfg(windows)]
compile_error!("windows is not supported");

#[path = "2022/mod.rs"]
mod year_2022;
#[path = "2023/mod.rs"]
mod year_2023;

pub(crate) mod const_helpers;

#[cfg(feature = "alloc")]
extern crate alloc;

use core::{self, fmt::Display};

#[cfg(not(any(test, debug_assertions)))]
#[panic_handler]
fn panic_handler<'a, 'b>(_: &'a core::panic::PanicInfo<'b>) -> ! {
    loop {}
}

#[cfg(feature = "alloc")]
mod allocator;

#[cfg(feature = "libc")]
mod libc_write;

// #[no_mangle]
// // #[cfg(all(not(test), feature = "alloc"))]
// // #[start]
// // #[cfg(target_arch = "aarch64")]
// pub extern "C" fn _start(_argc: i32, _argv: *const *const u8) {
//     use core::fmt::Write;

//     use crate::libc_write::Stdout;

//     #[inline]
//     fn solve<const YEAR: u16, const DAY: u8>()
//     where
//         Day<YEAR, DAY>: DaySolution,
//     {
//         writeln!(
//             &mut Stdout,
//             "{}/{}-1: {}",
//             YEAR,
//             DAY,
//             Day::<YEAR, DAY>::part_1()
//         )
//         .unwrap();
//         writeln!(
//             &mut Stdout,
//             "{}/{}-2: {}",
//             YEAR,
//             DAY,
//             Day::<YEAR, DAY>::part_2()
//         )
//         .unwrap();
//     }

//     macro_rules! run_solution {
//         ($YEAR:literal, $DAY:literal) => {
//             solve::<$YEAR, $DAY>();
//         };
//     }

//     for_each_day! {
//         run_solution
//     };
// }

#[no_mangle]
// #[cfg(all(
//     target_os = "linux",
//     target_arch = "x86_64",
//     target_env = "gnu",
//     not(test),
//     not(feature = "alloc"),
//     not(feature = "libc"),
//     feature = "const"
// ))]
pub extern "C" fn _start(_argc: i32, _argv: *const *const u8) -> ! {
    use crate::const_helpers::{arr, concat_array_const, itoa};

    macro_rules! run_solution {
        ($YEAR:literal, $DAY:literal) => {{
            concat_array_const! {
                const PREFIX: [u8; _] = itoa!($YEAR as u32), *b"/", itoa!($DAY as u32), *b"-";
                const PART_1: [u8; _] = *b"1: ", arr!(Day::<$YEAR, $DAY>::PART_1.as_bytes()), *b"\n";
                const PART_2: [u8; _] = *b"2: ", arr!(Day::<$YEAR, $DAY>::PART_2.as_bytes()), *b"\n";
                const OUTPUT: [u8; _] = PREFIX, PART_1, PREFIX, PART_2;
            }

            // sys::write(PREFIX.as_slice());
            // sys::write(PART_1.as_slice());

            // sys::write(PREFIX.as_slice());
            // sys::write(PART_2.as_slice());
            sys::write(OUTPUT.as_slice());
        }};
    }

    for_each_day! {
        run_solution
    };

    sys::exit();
}

struct Day<const YEAR: u16, const DAY: u8>;

/// An abstraction over [`Day`] allowing it to be used in generic contexts.
trait DaySolution: Input {
    fn part_1() -> impl Display;
    fn part_2() -> impl Display;
}

trait ConstDaySolution: Input {
    // could be neat!
    // const P: fn(&[u8]) -> u32 = parse;
    const PART_1: &'static str;
    const PART_2: &'static str;
}

impl<T: ConstDaySolution> DaySolution for T {
    fn part_1() -> impl Display {
        Self::PART_1
    }

    fn part_2() -> impl Display {
        Self::PART_2
    }
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

pub(crate) mod sys {
    use core::{arch::asm, hint::unreachable_unchecked};

    #[cfg(target_os = "linux")]
    const WRITE: u64 = 1;
    #[cfg(target_os = "linux")]
    const EXIT: u64 = 60;

    #[cfg(target_os = "macos")]
    const WRITE: u64 = 4;
    #[cfg(target_os = "macos")]
    const EXIT: u64 = 1;

    /// Wrapper around a Linux syscall with three arguments. It returns
    /// the syscall result (or error code) that gets stored in rax.
    unsafe fn syscall_3(num: u64, arg1: u64, arg2: u64, arg3: u64) -> i64 {
        let res;
        #[cfg(target_os = "linux")]
        asm!(
            // there is no need to write "mov"-instructions, see below
            "syscall",
            // from 'in("rax")' the compiler will
            // generate corresponding 'mov'-instructions
            in("rax") num,
            in("rdi") arg1,
            in("rsi") arg2,
            in("rdx") arg3,
            lateout("rax") res,
        );
        #[cfg(target_os = "macos")]
        asm!(
            "svc 0",
            in("x16") num,
            inout("x0") arg1 => res,
            in("x1") arg2,
            in("x2") arg3,
            options(nostack),
        );
        res
    }

    /// Wrapper around a Linux syscall with three arguments. It returns
    /// the syscall result (or error code) that gets stored in rax.
    unsafe fn syscall_1(num: u64, arg1: u64) -> i64 {
        let res;
        #[cfg(target_os = "linux")]
        asm!(
            // there is no need to write "mov"-instructions, see below
            "syscall",
            // from 'in("rax")' the compiler will
            // generate corresponding 'mov'-instructions
            in("rax") num,
            in("rdi") arg1,
            lateout("rax") res,
        );
        #[cfg(target_os = "macos")]
        asm!(
            "svc 0",
            in("x16") num,
            inout("x0") arg1 => res,
            options(nostack),
        );
        res
    }

    #[inline(always)]
    pub(crate) fn write(data: &[u8]) {
        let written = unsafe { syscall_3(WRITE, 1, data.as_ptr() as u64, data.len() as u64) };
    }

    pub(crate) fn exit() -> ! {
        unsafe {
            syscall_1(EXIT, 0);
            unreachable_unchecked();
        };
    }
}
