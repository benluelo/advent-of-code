//! Pulled from <https://doc.rust-lang.org/std/alloc/trait.GlobalAlloc.html>

use alloc::alloc::handle_alloc_error;
use core::{
    self,
    alloc::{GlobalAlloc, Layout},
    cell::UnsafeCell,
    slice,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    const_helpers::{arr, concat_array_const, itoa, parse_int},
    sys, NEWLINE,
};

pub const ARENA_SIZE: usize = match option_env!("ARENA_SIZE") {
    Some(env) => parse_int(env.as_bytes()) as usize,
    None => 1024 * 128,
};

const MAX_SUPPORTED_ALIGN: usize = 4096;
#[repr(C, align(4096))] // 4096 == MAX_SUPPORTED_ALIGN
pub struct StackArenaAllocator {
    arena: UnsafeCell<[u8; ARENA_SIZE]>,
    remaining: AtomicUsize, // we allocate from the top, counting down
}

#[allow(dead_code)]
impl StackArenaAllocator {
    pub fn remaining(&self) -> usize {
        self.remaining.load(Ordering::Relaxed)
    }

    /// # Safety
    ///
    /// The caller must ensure that no allocations occur while the returned
    /// reference is held.
    pub unsafe fn memory(&self) -> &[u8] {
        let remaining = self.remaining.load(Ordering::Relaxed);

        // .add(remaining)
        slice::from_raw_parts(
            self.arena.get().cast::<u8>().cast_const().add(remaining),
            ARENA_SIZE - remaining,
        )
    }
}

#[global_allocator]
pub static ALLOCATOR: StackArenaAllocator = StackArenaAllocator {
    arena: UnsafeCell::new([0; ARENA_SIZE]),
    remaining: AtomicUsize::new(ARENA_SIZE),
};

unsafe impl Sync for StackArenaAllocator {}

unsafe impl GlobalAlloc for StackArenaAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let align = layout.align();

        // `Layout` contract forbids making a `Layout` with align=0, or align not power
        // of 2. So we can safely use a mask to ensure alignment without
        // worrying about UB.
        let align_mask_to_round_down = !(align - 1);

        if align > MAX_SUPPORTED_ALIGN {
            handle_alloc_error(layout);
        }

        let mut allocated = 0;
        let update_res =
            self.remaining
                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |mut remaining| {
                    if size > remaining {
                        return None;
                    }
                    remaining -= size;
                    remaining &= align_mask_to_round_down;
                    allocated = remaining;
                    Some(remaining)
                });

        if update_res.is_err() {
            sys::write({
                concat_array_const! {
                    const MSG: [u8; _] = *b"out of memory, try compiling with ARENA_SIZE > ", arr!(itoa!(ARENA_SIZE).as_slice()), NEWLINE;
                };
                &MSG
            });

            sys::exit();
        };
        self.arena.get().cast::<u8>().add(allocated)
    }

    // this is the os' problem
    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {}
}
