use core::mem::{self, MaybeUninit};

use crate::{
    const_helpers::{iter, itoa, slice, slice_eq, slice_mut, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 15> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
    // const PART_1: &'static str = "";
    // const PART_2: &'static str = "";
}

#[allow(long_running_const_eval)]
const SOLUTION_PART_1: u32 = parse(Day::<2023, 15>::INPUT.as_bytes());
#[allow(long_running_const_eval)]
const SOLUTION_PART_2: usize = parse2(Day::<2023, 15>::INPUT.as_bytes());

#[test]
fn parse_test() {
    let mut input = b"rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

    let mut input = Day::<2023, 15>::INPUT.as_bytes();

    let score = parse2(input);

    println!("{score}");
}

const fn parse(input: &[u8]) -> u32 {
    let mut res = 0;

    iter! {
        for bz in split(input, b",") {
            res += hash(bz) as u32;
        }
    }

    res
}

const fn parse2(input: &[u8]) -> usize {
    let mut hashmap: HashMap = [const { ArrayVec::new() }; 256];

    iter! {
        for bz in split(input, b",") {
            let (op, h) = hash2(bz);

            let label = bz.split_at(bz.len() - match op {
                Op::Add(_) => 2,
                Op::Remove => 1,
            }).0;

            let slice = hashmap[h as usize].as_slice();

            let mut item = None;
            iter! {
                for (idx, ref v) in enumerate(&slice) {
                    if slice_eq(v.0, label) {
                        item = Some(idx);
                        break;
                    }
                }
            }

            match (op, item) {
                (Op::Add(value), None) => hashmap[h as usize].append((label, value)),
                (Op::Add(value), Some(existing_idx)) => {
                    *hashmap[h as usize].get_mut(existing_idx) = (label, value);
                }
                (Op::Remove, None) => {},
                (Op::Remove, Some(existing_idx)) => {
                    hashmap[h as usize].remove(existing_idx);
                }
            }
        }
    }

    let mut res = 0;

    // dbg_array_vec(&hashmap);

    iter! {
        for (box_idx, ref item) in enumerate(hashmap) {
            let slice = item.as_slice();

            if !slice.is_empty() {

                iter! {
                    for (slot, (_label, focal_length)) in enumerate(slice) {
                        res += (box_idx + 1) * (slot + 1) * focal_length as usize;
                    }
                }
            }
        }
    }

    res
}

const fn hash(bz: &[u8]) -> u8 {
    let mut res: u8 = 0;

    iter! {
        for char in bz {
            res = res.wrapping_add(char);
            res = res.wrapping_mul(17);
        }
    }

    res
}

const fn hash2(bz: &[u8]) -> (Op, u8) {
    let mut res: u8 = 0;

    iter! {
        for char in bz {
            match char {
                b'-' => return (Op::Remove, res),
                b'=' => return (Op::Add(*bz.last().unwrap() - 48), res),
                _ => {
                    res = res.wrapping_add(char);
                    res = res.wrapping_mul(17);
                }
            }
        }
    }

    panic!()
}

// 6 might have to be increased for different inputs, but this worked for mine
type HashMap<'a> = [ArrayVec<(&'a [u8], u8), 6>; 256];

struct ArrayVec<T, const N: usize> {
    len: usize,
    arr: [MaybeUninit<T>; N],
}

impl<T: core::fmt::Debug, const N: usize> core::fmt::Debug for ArrayVec<T, N> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl<T, const N: usize> ArrayVec<T, N> {
    const fn new() -> Self {
        Self {
            len: 0,
            arr: MaybeUninit::uninit_array(),
        }
    }

    const fn append(&mut self, t: T) {
        self.arr[self.len].write(t);
        self.len += 1;
    }

    const fn get(&mut self, idx: usize) -> &T {
        assert!(idx < self.len);
        unsafe { self.arr[idx].assume_init_ref() }
    }

    const fn get_mut(&mut self, idx: usize) -> &mut T {
        assert!(idx < self.len);
        unsafe { self.arr[idx].assume_init_mut() }
    }

    const fn as_slice(&self) -> &[T] {
        unsafe { MaybeUninit::slice_assume_init_ref(slice(self.arr.as_slice(), 0, self.len)) }
    }

    const fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { MaybeUninit::slice_assume_init_mut(slice_mut(&mut self.arr, 0, self.len)) }
    }

    const fn remove(&mut self, idx: usize) -> T {
        assert!(idx < self.len);

        // move the item to be removed to the end of the array
        match self.len - idx {
            1 => {
                // no extra work required, tail of the list is the item to be
                // removed
            }
            2 => self.arr.swap(idx, self.len - 1),
            _ => {
                iter! {
                    for i in range(idx, self.len - 1) {
                        self.arr.swap(i, i + 1)
                    }
                }
            }
        }

        let t = mem::replace(&mut self.arr[self.len - 1], MaybeUninit::uninit());

        self.len -= 1;

        unsafe { t.assume_init() }
    }
}

enum Op {
    Add(u8),
    Remove,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works() {
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
    #[should_panic]
    fn append_overflow_panics() {
        let mut av = ArrayVec::<u8, 1>::new();

        av.append(1);
        av.append(2);
    }
}

#[cfg(test)]
fn dbg_array_vec(hashmap: &HashMap) {
    let formatted = hashmap
        .iter()
        .enumerate()
        .map(|(box_idx, item)| {
            let slice = item.as_slice();

            if !slice.is_empty() {
                format!(
                    "Box {box_idx}: {}",
                    slice
                        .iter()
                        .enumerate()
                        .map(|(slot, (label, focal_length))| {
                            format!("[{} {focal_length}]", utf8(label))
                        })
                        .collect::<Vec<_>>()
                        .join(" "),
                )
            } else {
                String::new()
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    println!("{formatted}");
}
