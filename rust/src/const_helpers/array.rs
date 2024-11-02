use core::mem::{self, MaybeUninit};

use cfg_proc::apply;

use crate::const_helpers::{iter, slice, slice_mut, utf8};

pub struct ArrayVec<T, const N: usize> {
    len: usize,
    arr: [MaybeUninit<T>; N],
}

impl<T: core::fmt::Debug, const N: usize> core::fmt::Debug for ArrayVec<T, N> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl<T, const N: usize> ArrayVec<T, N> {
    #[must_use]
    #[allow(clippy::new_without_default)]
    pub const fn new() -> Self {
        Self {
            len: 0,
            arr: MaybeUninit::uninit_array(),
        }
    }

    pub const fn append(&mut self, t: T) {
        self.arr[self.len].write(t);
        self.len += 1;
    }

    pub const fn push(&mut self, t: T) {
        assert!(self.len < self.arr.len());
        if self.len == 0 {
            self.append(t);
            return;
        }

        if self.len == 1 {
            self.arr[1].write(t);
            self.arr.swap(0, 1);
            self.len += 1;
            return;
        }

        self.arr[self.len].write(t);
        #[apply(iter)]
        for i in range(0, self.len) {
            self.arr.swap(self.len - i, self.len - (i + 1));
        }

        self.len += 1;
    }

    pub const fn get(&mut self, idx: usize) -> &T {
        assert!(idx < self.len);
        unsafe { self.arr[idx].assume_init_ref() }
    }

    pub const fn get_mut(&mut self, idx: usize) -> &mut T {
        assert!(idx < self.len);
        unsafe { self.arr[idx].assume_init_mut() }
    }

    pub const fn as_slice(&self) -> &[T] {
        unsafe { MaybeUninit::slice_assume_init_ref(slice(self.arr.as_slice(), 0, self.len)) }
    }

    pub const fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { MaybeUninit::slice_assume_init_mut(slice_mut(&mut self.arr, 0, self.len)) }
    }

    pub const fn remove(&mut self, idx: usize) -> T {
        assert!(idx < self.len);

        // move the item to be removed to the end of the array
        match self.len - idx {
            1 => {
                // no extra work required, tail of the list is the item to be
                // removed
            }
            2 => self.arr.swap(idx, self.len - 1),
            _ =>
            {
                #[apply(iter)]
                for i in range(idx, self.len - 1) {
                    self.arr.swap(i, i + 1);
                }
            }
        }

        let t = mem::replace(&mut self.arr[self.len - 1], MaybeUninit::uninit());

        self.len -= 1;

        unsafe { t.assume_init() }
    }
}

impl<const N: usize> ArrayVec<u8, N> {
    #[must_use]
    pub const fn as_str(&self) -> &str {
        utf8(self.as_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn append_works() {
        let mut av = ArrayVec::<u8, 5>::new();

        assert_eq!(av.as_slice(), &[]);

        av.append(1);
        assert_eq!(av.as_slice(), &[1]);

        av.append(2);
        assert_eq!(av.as_slice(), &[1, 2]);

        av.append(3);
        assert_eq!(av.as_slice(), &[1, 2, 3]);

        av.append(4);
        assert_eq!(av.as_slice(), &[1, 2, 3, 4]);

        av.append(5);
        assert_eq!(av.as_slice(), &[1, 2, 3, 4, 5]);
    }

    #[test]
    fn push_works() {
        let mut av = ArrayVec::<u8, 5>::new();

        assert_eq!(av.as_slice(), &[]);

        av.push(1);
        assert_eq!(av.as_slice(), &[1]);

        av.push(2);
        assert_eq!(av.as_slice(), &[2, 1]);

        av.push(3);
        assert_eq!(av.as_slice(), &[3, 2, 1]);

        av.push(4);
        assert_eq!(av.as_slice(), &[4, 3, 2, 1]);

        av.push(5);
        assert_eq!(av.as_slice(), &[5, 4, 3, 2, 1]);
    }

    #[test]
    fn remove() {
        {
            let mut av = ArrayVec::<char, 5>::new();

            av.append('a');
            av.append('b');
            av.append('c');

            assert_eq!(av.remove(1), 'b');
            assert_eq!(av.as_slice(), &['a', 'c']);

            av.append('d');
            av.append('e');
            av.append('f');

            assert_eq!(av.remove(4), 'f');
            assert_eq!(av.as_slice(), &['a', 'c', 'd', 'e']);
        }

        {
            let mut av = ArrayVec::<char, 5>::new();

            av.append('a');
            av.append('b');
            av.append('c');

            assert_eq!(av.remove(0), 'a');
            assert_eq!(av.as_slice(), &['b', 'c']);
        }
    }

    #[test]
    #[should_panic = "index out of bounds: the len is 1 but the index is 1"]
    fn append_overflow_panics() {
        let mut av = ArrayVec::<u8, 1>::new();

        av.append(1);
        av.append(2);
    }
}
