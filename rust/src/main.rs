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
    const_maybe_uninit_write,
    maybe_uninit_slice,
    maybe_uninit_uninit_array,
    const_maybe_uninit_uninit_array,
    const_maybe_uninit_assume_init,

    // numbers
    int_roundings,
    isqrt,

    // misc
    control_flow_enum,
    byte_slice_trim_ascii,
    let_chains,

    // cool and handy
    lint_reasons,
    trace_macros,
    slice_flatten,

    // const stuff
    const_mut_refs,
    const_slice_split_at_mut,
    const_option,
    const_swap,
    const_replace,
    inline_const,

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

pub mod allocator;
pub mod const_helpers;

#[cfg(not(feature = "const"))]
extern crate alloc;

#[cfg(not(feature = "const"))]
use alloc::{vec, vec::Vec};
use core::{self, ffi::CStr};

use crate::const_helpers::{arr, bytes_to_array, slice};

struct Day<const YEAR: u16, const DAY: u8>;

#[cfg(not(any(test, debug_assertions)))]
#[panic_handler]
fn panic_handler<'a, 'b>(_: &'a core::panic::PanicInfo<'b>) -> ! {
    loop {}
}

const NEWLINE: [u8; 1] = *b"\n";

fn print(data: &[u8]) {
    let len = data.len();
    let mut total_written = 0;

    while total_written < len {
        let written = sys::write(1, &data[total_written..]).unwrap();
        total_written += written;
    }
}

/// # Safety
///
/// This is the entry point, I don't recommend calling this
#[no_mangle]
pub unsafe extern "C" fn _start(argc: usize, argv: *const *const u8) -> ! {
    use core::slice;

    use crate::const_helpers::{concat_array_const, itoa};

    let argv = unsafe { slice::from_raw_parts(argv, argc) };

    let 3 = argc else {
        print(b"usage: ");
        print(unsafe { CStr::from_ptr(argv[0].cast()) }.to_bytes());
        print(b" <day> <path-to-input>\navailable days:\n");

        let mut output = vec![];

        macro_rules! solution_name {
            ($YEAR:literal, $DAY:literal) => {{
                concat_array_const! {
                    const PREFIX: [u8; _] = arr!(itoa!($YEAR as u32).as_str().as_bytes()), *b"/", arr!(itoa!($DAY as u32).as_str().as_bytes()), NEWLINE;
                }

                output.extend(PREFIX)
            }};
        }

        for_each_day! { solution_name };

        print(&output);

        sys::exit();
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
                print(crate::const_displayable::Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part1>::new(part_1).as_str().as_bytes());
                print(NEWLINE.as_slice());

                let part_2 = Day::<$YEAR, $DAY>::parse2(&mut input);

                print(PREFIX.as_slice());
                print(b"2: ");
                print(crate::const_displayable::Displayable::<<Day<$YEAR, $DAY> as SolutionTypes>::Part2>::new(part_2).as_str().as_bytes());
                print(NEWLINE.as_slice());

                sys::exit();
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

    sys::exit();
}

fn read_all(path: &CStr) -> Vec<u8> {
    let Ok(stat) = sys::stat(path) else {
        print(b"file not found: ");
        print(path.to_bytes());
        print(&NEWLINE);
        sys::exit();
    };

    let fd = sys::open(path, 0).unwrap();

    let mut buf = vec![0; stat.size()];

    let read = sys::read(fd, &mut buf, stat.size()).unwrap();
    assert_eq!(read, stat.size());

    buf
}

#[derive(Debug)]
#[repr(C)]
struct Stat(#[cfg(all(target_os = "macos", target_arch = "aarch64"))] [u8; 144]);

impl Stat {
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    pub fn size(&self) -> usize {
        // trust me bro
        i64::from_ne_bytes(bytes_to_array(slice(&self.0, 72, 80)))
            .try_into()
            .unwrap()
    }
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

pub(crate) mod sys {
    use core::{
        arch::asm,
        ffi::{c_void, CStr},
        hint::unreachable_unchecked,
        ptr::addr_of_mut,
    };

    cfg_match! {
        cfg(all(target_os = "linux", target_arch = "aarch64")) => {
            const WRITE: usize = 1;
            const EXIT: usize = 60;
        }
        cfg(all(target_os = "macos", target_arch = "aarch64")) => {
            const WRITE: usize = 4;
            const EXIT: usize = 1;
            const READ: usize = 3;
            const STAT: usize = 188;
            const OPEN: usize = 5;
            const CLOSE: usize = 6;
        }
    }

    /// Wrapper around a Linux syscall with three arguments. It returns
    /// the syscall result (or error code) that gets stored in rax.
    unsafe fn syscall_1(num: usize, arg1: usize) -> isize {
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

    /// Wrapper around a Linux syscall with three arguments. It returns
    /// the syscall result (or error code) that gets stored in rax.
    unsafe fn syscall_2(num: usize, arg1: usize, arg2: usize) -> isize {
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
            in("x1") arg2,
            options(nostack),
        );
        res
    }

    /// Wrapper around a Linux syscall with three arguments. It returns
    /// the syscall result (or error code) that gets stored in rax.
    unsafe fn syscall_3(num: usize, arg1: usize, arg2: usize, arg3: usize) -> isize {
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

    pub fn write(fd: usize, data: &[u8]) -> Result<usize, isize> {
        let written = unsafe { syscall_3(WRITE, fd, data.as_ptr() as usize, data.len()) };

        written.try_into().map_err(|_| written)
    }

    pub fn read(fd: usize, buf: &mut [u8], count: usize) -> Result<usize, isize> {
        let ret = unsafe { syscall_3(READ, fd, buf.as_mut_ptr() as usize, count) };

        ret.try_into().map_err(|_| ret)
    }

    pub fn exit() -> ! {
        unsafe {
            syscall_1(EXIT, 0);
            unreachable_unchecked();
        };
    }

    pub fn stat(path: &CStr) -> Result<crate::Stat, isize> {
        let mut stat: crate::Stat = unsafe { core::mem::zeroed() };

        let ret = unsafe { syscall_2(STAT, path.as_ptr() as usize, addr_of_mut!(stat) as usize) };

        (ret == 0).then_some(stat).ok_or(ret)
    }

    pub fn open(path: &CStr, flags: usize) -> Result<usize, isize> {
        let ret = unsafe { syscall_2(OPEN, path.as_ptr() as usize, flags) };

        ret.try_into().map_err(|_| ret)
    }

    pub fn close(fd: usize) {
        unsafe { syscall_1(CLOSE, fd) };
    }

    /// # Safety
    ///
    /// The caller must ensure that `s` is at least `n` bytes long, `n`
    /// bytes of `s` are valid to be set to zero.
    #[no_mangle]
    // llvm is emitting this symbol for some reason
    unsafe extern "C" fn bzero(s: *mut c_void, n: usize) {
        extern "C" {
            fn memset(dest: *mut c_void, c: i32, n: usize);
        }
        memset(s, 0, n);
    }

    #[test]
    fn bzero_test() {
        let mut bz = vec![1_u8; 20];

        unsafe { bzero(bz.as_mut_ptr().cast(), 10) }

        assert_eq!(
            bz,
            vec![0; 10]
                .into_iter()
                .chain(vec![1; 10])
                .collect::<Vec<_>>()
        );

        let mut bz = vec![u32::MAX; 4];

        unsafe { bzero(bz.as_mut_ptr().cast(), 6) }

        assert_eq!(
            bz,
            [0, u32::from_ne_bytes([0, 0, 255, 255]), u32::MAX, u32::MAX]
        );
    }
}

pub mod const_displayable {
    pub struct Displayable<T: ConstDisplayable> {
        t: T,
        output: T::Output,
    }

    pub trait ConstDisplayable {
        type Output;
    }

    macro_rules! displayable_num {
        ($($T:ty)+) => {
            $(
                impl Displayable<$T> {
                    #[must_use]
                    pub const fn new(t: $T) -> Self {
                        Self {
                            t,
                            output: $crate::const_helpers::Num(t).to_str(),
                        }
                    }

                    #[must_use]
                    pub const fn as_str(&self) -> &str {
                        self.output.as_str()
                    }
                }

                impl ConstDisplayable for $T {
                    type Output = $crate::const_helpers::array::ArrayVec<u8, { $crate::const_helpers::Num::<$T>::STR_LEN }>;
                }
            )+
        };
    }
    displayable_num!(u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize);

    impl ConstDisplayable for alloc::string::String {
        type Output = alloc::string::String;
    }

    impl Displayable<alloc::string::String> {
        #[must_use]
        pub const fn new(t: alloc::string::String) -> Self {
            Self {
                t,
                // we borrow from self.t in as_str, so this can be anything really
                output: alloc::string::String::new(),
            }
        }

        #[must_use]
        pub fn as_str(&self) -> &str {
            &self.t
        }
    }

    impl<'a> ConstDisplayable for &'a str {
        type Output = &'a str;
    }

    impl<'a> Displayable<&'a str> {
        #[must_use]
        pub const fn new(t: &'a str) -> Self {
            Self { t, output: t }
        }

        #[must_use]
        pub fn as_str(&self) -> &str {
            self.t
        }
    }
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
