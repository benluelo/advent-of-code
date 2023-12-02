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

extern crate alloc;

use core::{
    alloc::{GlobalAlloc, Layout},
    cmp,
    fmt::{Display, Write},
    ptr,
};

use libc::c_void;

#[link(name = "c")]
extern "C" {}

#[cfg(not(any(test, debug_assertions)))]
#[panic_handler]
fn panic_handler<'a, 'b>(_: &'a core::panic::PanicInfo<'b>) -> ! {
    loop {}
}

#[global_allocator]
static A: LibcAlloc = LibcAlloc;

/// Mostly pulled from here: <https://github.com/daniel5151/libc_alloc/blob/master/src/lib.rs>
pub struct LibcAlloc;

unsafe impl GlobalAlloc for LibcAlloc {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        libc::memalign(
            layout.align().max(core::mem::size_of::<usize>()),
            layout.size(),
        )
        .cast::<u8>()
    }

    #[inline]
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        // Unfortunately, calloc doesn't make any alignment guarantees, so the memory
        // has to be manually zeroed-out.
        let ptr = self.alloc(layout);
        if !ptr.is_null() {
            ptr::write_bytes(ptr, 0, layout.size());
        }
        ptr
    }

    #[inline]
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        libc::free(ptr.cast::<c_void>());
    }

    #[inline]
    unsafe fn realloc(&self, old_ptr: *mut u8, old_layout: Layout, new_size: usize) -> *mut u8 {
        let new_layout = Layout::from_size_align_unchecked(new_size, old_layout.align());
        let new_ptr = self.alloc(new_layout);
        if !new_ptr.is_null() {
            let size = cmp::min(old_layout.size(), new_size);
            ptr::copy_nonoverlapping(old_ptr, new_ptr, size);
            self.dealloc(old_ptr, old_layout);
        }
        new_ptr
    }
}

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

#[no_mangle]
#[cfg(not(test))]
pub extern "Rust" fn main(_argc: i32, _argv: *const *const u8) {
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
