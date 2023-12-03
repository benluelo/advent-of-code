#![cfg_attr(not(test), no_main)]
#![cfg_attr(not(any(test, debug_assertions)), no_std)]
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
    trace_macros,
    byte_slice_trim_ascii
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
mod libc_write {
    use core::fmt::Write;

    pub struct Stdout;

    impl Write for Stdout {
        #[inline]
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            let msg = s.as_bytes();

            let mut written = 0;
            while written < msg.len() {
                let bytes: &[u8] = &msg[written..];

                let res = usize::try_from(unsafe {
                    libc::write(1, bytes.as_ptr().cast::<core::ffi::c_void>(), bytes.len())
                });

                match res {
                    Ok(res) => written += res,
                    // Ignore errors
                    Err(_) => break,
                }
            }

            Ok(())
        }
    }
}

#[no_mangle]
#[cfg(all(not(test), feature = "alloc"))]
pub extern "Rust" fn main(_argc: i32, _argv: *const *const u8) {
    use core::fmt::Write;

    use crate::libc_write::Stdout;

    #[inline]
    fn solve<const YEAR: u16, const DAY: u8>()
    where
        Day<YEAR, DAY>: DaySolution,
    {
        writeln!(
            &mut Stdout,
            "{}/{}-1: {}",
            YEAR,
            DAY,
            Day::<YEAR, DAY>::part_1()
        )
        .unwrap();
        writeln!(
            &mut Stdout,
            "{}/{}-2: {}",
            YEAR,
            DAY,
            Day::<YEAR, DAY>::part_2()
        )
        .unwrap();
    }

    macro_rules! run_solution {
        ($YEAR:literal, $DAY:literal) => {
            solve::<$YEAR, $DAY>();
        };
    }

    for_each_day! {
        run_solution
    };
}

#[no_mangle]
#[cfg(all(
    target_os = "linux",
    target_arch = "x86_64",
    not(test),
    not(feature = "alloc"),
    not(feature = "libc")
))]
pub extern "C" fn main(_argc: i32, _argv: *const *const u8) {
    use crate::const_helpers::itoa;

    macro_rules! run_solution {
        ($YEAR:literal, $DAY:literal) => {{
            const YEAR: &[u8] = &itoa!($YEAR as u32);
            const DAY: &[u8] = &itoa!($DAY as u32);

            sys::write(YEAR);
            sys::write(b"/");
            sys::write(DAY);
            sys::write(b"-1: ");
            sys::write(Day::<$YEAR, $DAY>::PART_1.as_bytes());
            sys::write(b"\n");

            sys::write(YEAR);
            sys::write(b"/");
            sys::write(DAY);
            sys::write(b"-2: ");
            sys::write(Day::<$YEAR, $DAY>::PART_2.as_bytes());
        }};
    }

    for_each_day! {
        run_solution
    };
}

struct Day<const YEAR: u16, const DAY: u8>;

/// An abstraction over [`Day`] allowing it to be used in generic contexts.
trait DaySolution: Input {
    fn part_1() -> impl Display;
    fn part_2() -> impl Display;
}

trait ConstDaySolution: Input {
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

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub(crate) mod sys {
    use core::arch::asm;

    const WRITE: u64 = 1;

    /// Wrapper around a Linux syscall with three arguments. It returns
    /// the syscall result (or error code) that gets stored in rax.
    unsafe fn syscall_3(num: u64, arg1: u64, arg2: u64, arg3: u64) -> i64 {
        let res;
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
        res
    }

    pub(crate) fn write(data: &[u8]) {
        let written = unsafe { syscall_3(WRITE, 1, data.as_ptr() as u64, data.len() as u64) };

        // without this, the output is missing the year in the second line. no clue why
        assert!(written == data.len() as i64);
    }
}
