use core::{
    self,
    alloc::{GlobalAlloc, Layout},
    cmp, ptr,
};

use libc::c_void;

// #[link(name = "c", cfg(feature = "alloc"))]
// extern "C" {}

#[global_allocator]
pub(crate) static A: LibcAlloc = LibcAlloc;

/// Mostly pulled from here: <https://github.com/daniel5151/libc_alloc/blob/master/src/lib.rs>
pub struct LibcAlloc;

unsafe impl GlobalAlloc for LibcAlloc {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        libc::malloc(
            // layout.align().max(core::mem::size_of::<usize>()),
            layout.size(),
        )
        .cast::<u8>()
    }

    #[inline]
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        libc::free(ptr.cast::<c_void>());
    }
}
