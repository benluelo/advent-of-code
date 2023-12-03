use core::{
    self,
    alloc::{GlobalAlloc, Layout},
    cmp, ptr,
};

use libc::c_void;

#[link(name = "c")]
extern "C" {}

#[global_allocator]
pub(crate) static A: LibcAlloc = LibcAlloc;

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
