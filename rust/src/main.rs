// #![cfg_attr(not(any(test, target_arch = "aarch64")), no_main)]
#![cfg_attr(not(test), no_std, no_main)]
// #![cfg_attr(target_arch = "aarch64", feature(start))]
// #![cfg_attr(not(any(test, debug_assertions)), no_std)]
#![feature(
    naked_functions,
    start,

    // iterator hackery
    iter_array_chunks,
    iter_next_chunk,
    iter_intersperse,

    // arrays
    array_windows,
    as_array_of_cells,
    const_maybe_uninit_write,
    maybe_uninit_slice,
    maybe_uninit_uninit_array,
    const_maybe_uninit_uninit_array,
    const_maybe_uninit_assume_init,

    // numbers
    int_roundings,
    isqrt,

    // misc
    let_chains,

    // cool and handy
    trace_macros,

    // const stuff
    const_slice_flatten,
    const_option,
    const_swap,
    const_replace,

    stmt_expr_attributes,
    proc_macro_hygiene,
    cfg_match,
)]

#[cfg(windows)]
compile_error!("windows is not supported");

#[path = "2022/mod.rs"]
mod year_2022;
#[path = "2023/mod.rs"]
mod year_2023;

#[cfg(not(feature = "const"))]
pub mod allocator;
pub mod const_displayable;
pub mod const_helpers;

#[cfg(not(feature = "const"))]
extern crate alloc;

#[cfg(not(feature = "const"))]
use alloc::vec;
use core::{
    self,
    ffi::{c_char, c_int, CStr},
    hint::unreachable_unchecked,
    ptr,
};

use syscalls::{syscall0, syscall2, syscall3, Sysno};

use crate::{
    const_displayable::Displayable,
    const_helpers::{arr, Num},
};

struct Day<const YEAR: u16, const DAY: u8>;

#[cfg(not(test))]
#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo<'_>) -> ! {
    loop {}
}

const NEWLINE: [u8; 1] = *b"\n";

fn print(data: &[u8]) {
    let len = data.len();
    let mut total_written = 0;

    while total_written < len {
        let written = unsafe {
            syscall3(
                Sysno::write,
                1,
                data[total_written..].as_ptr() as usize,
                data.len() - total_written,
            )
            .unwrap()
        };
        total_written += written;
    }
}

/// glibc passes argc, argv, and envp to functions in .init_array, as a
/// non-standard extension. This allows `std::env::args` to work even in a
/// `cdylib`, as it does on macOS and Windows.
// #[cfg(all(target_os = "linux", target_env = "gnu"))]
#[used]
#[link_section = ".init_array"]
static ARGV_INIT_ARRAY: extern "C" fn(core::ffi::c_int, *const *const c_char) = {
    extern "C" fn init_wrapper(argc: core::ffi::c_int, argv: *const *const c_char) {
        unsafe {
            // print(Num(argc).to_str().as_str().as_bytes());
            // panic!();

            ARGC = argc;
            ARGV = argv;
        }
    }
    init_wrapper
};

static mut ARGC: c_int = 0;
static mut ARGV: *const *const c_char = ptr::null();
// // static LOCK: StaticMutex = StaticMutex::new();

// #[no_mangle]
// pub unsafe extern "C" fn _start(argc: core::ffi::c_int, argv: *const *const
// u8) -> ! {     // let mut argc: core::ffi::c_int;

//     // core::arch::asm!("ldr x0, {:x}", out(reg) argc);

//     // print(Num(argc).to_str().as_str().as_bytes());

//     print(Num(ARGC).to_str().as_str().as_bytes());

//     // print(b"\n");

//     // print(Num(argc).to_str().as_str().as_bytes());

//     // print(b"\n");

//     // print(Num(argc + 1).to_str().as_str().as_bytes());

//     exit()
// }

// // #[naked]
// extern "C" fn _start() {
//     unsafe { core::arch::naked_asm!("call main") }
// }

#[no_mangle]
// #[start]
extern "C" fn _start(argc: c_int, argv: *const *const c_char) -> c_int {
    print(Num(argc).to_str().as_str().as_bytes());

    let mut total: u32 = 0;
    let mut argv = argv;

    loop {
        if argv.is_null() {
            break;
        }

        unsafe { argv = argv.add(1) };

        total += 1;
    }

    print(b"\n");

    print(Num(total).to_str().as_str().as_bytes());

    exit()
}

/// # Safety
///
/// This is the entry point, I don't recommend calling this
// #[cfg(not(test))]
// #[no_mangle]
pub unsafe extern "C" fn __start(argc: usize, argv: *const *const u8) -> ! {
    use core::slice;

    use crate::const_helpers::{concat_array_const, itoa};

    let argv = unsafe { slice::from_raw_parts(argv, argc) };

    for arg in argv {
        print(unsafe { CStr::from_ptr(arg.cast()) }.to_bytes());
        print(b"\n");
    }

    let 3 = argc else {
        print(b"usage: ");
        print(unsafe { CStr::from_ptr(argv[0].cast()) }.to_bytes());
        print(b" <day> <path-to-input>\navailable days:\n");

        // let mut output = alloc::vec![];

        macro_rules! solution_name {
            ($YEAR:literal, $DAY:literal) => {{
                concat_array_const! {
                    const PREFIX: [u8; _] = arr!(itoa!($YEAR as u32).as_str().as_bytes()), *b"/", arr!(itoa!($DAY as u32).as_str().as_bytes()), NEWLINE;
                }

                print(&PREFIX);
            }};
        }

        for_each_day! { solution_name };

        // print(&output);

        exit()
    };

    let day = unsafe { CStr::from_ptr(argv[1].cast()) };
    let path = unsafe { CStr::from_ptr(argv[2].cast()) };

    #[cfg(feature = "const")]
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
                const PART_1: [u8; _] = *b"1: ", arr!(SOLUTION_PART_1.as_bytes()), NEWLINE;
                const PART_2: [u8; _] = *b"2: ", arr!(SOLUTION_PART_2.as_bytes()), NEWLINE;
                const OUTPUT: [u8; _] = PREFIX, PART_1, PREFIX, PART_2;
            }

            print(OUTPUT.as_slice());
        }};
    }

    #[cfg(not(feature = "const"))]
    macro_rules! run_solution {
        ($YEAR:literal, $DAY:literal) => {{
            concat_array_const! {
                const ID: [u8; _] = arr!(itoa!($YEAR as u32).as_str().as_bytes()), *b"/", arr!(itoa!($DAY as u32).as_str().as_bytes());
                const PREFIX: [u8; _] = ID, *b"-";
            }

            if &day.to_bytes() == &ID.as_slice() {
                let mut input = read_all(path);

                let part_1 = Day::<$YEAR, $DAY>::parse(&mut input.clone());

                print(PREFIX.as_slice());
                print(b"1: ");
                print(Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part1>::new(part_1).as_str().as_bytes());
                print(NEWLINE.as_slice());

                let part_2 = Day::<$YEAR, $DAY>::parse2(&mut input);

                print(PREFIX.as_slice());
                print(b"2: ");
                print(Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part2>::new(part_2).as_str().as_bytes());
                print(NEWLINE.as_slice());

                exit();
            }
        }};
    }

    for_each_day! {
        run_solution
    };

    #[cfg(not(feature = "const"))]
    {
        print(b"day `");
        print(day.to_bytes());
        print(b"` not found\n");
    }

    exit();
}

fn exit() -> ! {
    // SAFETY: exit will terminate the program
    unsafe {
        syscall0(syscalls::Sysno::exit).unwrap();
        unreachable_unchecked()
    }
}

#[cfg(not(feature = "const"))]
fn read_all(path: &CStr) -> alloc::vec::Vec<u8> {
    // let Ok(stat) = syscall1(Sysno::stat path) else {
    //     print(b"file not found: ");
    //     print(path.to_bytes());
    //     print(&NEWLINE);
    //     exit();
    // };

    let fd = match unsafe { syscall2(Sysno::openat, path.as_ptr() as usize, 0) } {
        Ok(fd) => fd,
        Err(err) => {
            panic!("error opening file: {err}")
        }
    };

    let mut buf = vec![];

    loop {
        buf.reserve(100);

        let read = unsafe { syscall3(Sysno::read, fd, buf.as_mut_ptr() as usize, 100).unwrap() };

        if read < 100 {
            break;
        }
    }

    buf
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
    };

    // non-const impls
    (
        $(#[cfg($meta:meta)])*
        impl $Ty:ty {
            pub fn parse($($p1_args:tt)*) -> $P1Ret:ty $p1_body:block
            pub fn parse2($($p2_args:tt)*) -> $P2Ret:ty $p2_body:block
        }
    ) => {
        $(#[cfg($meta)])*
        impl $Ty {
            pub fn parse($($p1_args)*) -> $P1Ret $p1_body
            pub fn parse2($($p2_args)*) -> $P2Ret $p2_body
        }

        $(#[cfg($meta)])*
        impl $crate::SolutionTypes for $Ty {
            type Part1 = $P1Ret;
            type Part2 = $P2Ret;
        }
    };
}
