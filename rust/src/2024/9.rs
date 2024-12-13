use cfg_proc::apply;

use crate::{
    day,
    utils::{array::ArrayVec, iter, min_usize},
    Day,
};

#[apply(day)]
impl Day<2024, 9> {
    pub const fn parse(input: &[u8]) -> usize {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> usize {
        parse2(input)
    }
}

#[allow(clippy::collapsible_else_if)]
const fn parse(input: &[u8]) -> usize {
    let input = input.trim_ascii();

    let mut tail = input.len() - 1;
    // index into the (potentially partially moved) tail file
    let mut tail_partial_idx = 0;

    let mut head = 0;
    // index into the (potentially partially filled) head empty space
    let mut head_partial_idx = 0;

    let mut curr = 0;

    let mut len = 0;
    #[apply(iter)]
    for (idx, c) in enumerate(input) {
        if idx % 2 == 0 {
            len += (c - 48) as usize;
        }
    }

    let mut checksum = 0;

    'outer: while curr < len {
        // if the current head pointer is pointing to a file, then handle that file
        if head % 2 == 0 {
            let file_size = (input[head] - 48) as usize;

            #[apply(iter)]
            for _ in range(0, file_size) {
                checksum += (head / 2) * curr;
                curr += 1;
                if curr >= len {
                    break 'outer;
                }
            }

            head += 1;
        } else {
            // otherwise, the head pointer is pointing to an empty space. if the tail
            // pointer is pointing to a file, then attempt to move that file to the head
            if tail % 2 == 0 {
                let empty_space_size = (input[head] - 48) as usize;

                let file_size = (input[tail] - 48) as usize;

                let to_write = file_size - tail_partial_idx;
                let remaining_space = empty_space_size - head_partial_idx;

                // attempt to move the remaining portion of this file
                #[apply(iter)]
                for _ in range(0, min_usize(to_write, remaining_space)) {
                    checksum += (tail / 2) * curr;
                    curr += 1;
                }

                // if there's still space left in this empty space, move the head partial idx
                // pointer accordingly
                if to_write < remaining_space {
                    head_partial_idx += to_write;
                } else {
                    head_partial_idx = 0;
                    head += 1;
                }

                if to_write <= remaining_space {
                    // the entire tail file fit, reset the tail partial idx pointer
                    tail_partial_idx = 0;

                    // this file has been moved entirely, bump the tail pointer back one
                    tail -= 1;
                } else {
                    let written = to_write - remaining_space;
                    tail_partial_idx = file_size - written;
                }
            } else {
                // always decrease the the tail pointer if it is pointing to an empty space
                tail -= 1;
            }
        }
    }

    checksum
}

const DISK_SIZE: usize = 100_000;

const fn parse2(input: &[u8]) -> usize {
    let input = input.trim_ascii();

    let mut disk = <ArrayVec<Option<u16>, DISK_SIZE>>::new();

    #[apply(iter)]
    for (idx, c) in enumerate(input) {
        let size = c - 48;
        if idx % 2 == 0 {
            #[apply(iter)]
            for _ in range(0, size) {
                #[allow(clippy::cast_possible_truncation)]
                disk.append(Some((idx / 2) as u16));
            }
        } else {
            #[apply(iter)]
            for _ in range(0, size) {
                disk.append(None);
            }
        }
    }

    let mut idx = disk.len();
    let mut in_chunk = None::<(u16, usize)>;
    let mut highest_chunk_moved = u16::MAX;
    let mut first_empty_space_idx: usize = 0;

    while idx > 0 && idx >= first_empty_space_idx {
        idx -= 1;

        match (in_chunk, disk.get(idx)) {
            (Some((in_chunk_, size)), Some(chunk)) => {
                if *chunk == in_chunk_ {
                    // in a chunk previously and still in the same chunk
                    in_chunk = Some((*chunk, size + 1));
                } else {
                    // in a chunk previously and now not in the same chunk
                    in_chunk = Some((*chunk, 1));
                    highest_chunk_moved = in_chunk_;
                    // dbg!(first_empty_space_idx);
                    move_chunk(
                        &mut disk,
                        &mut first_empty_space_idx,
                        idx + 1,
                        size,
                        in_chunk_,
                    );
                }
            }
            // in a chunk previously, now not in a chunk
            (Some((in_chunk_, size)), None) => {
                in_chunk = None;
                highest_chunk_moved = in_chunk_;
                // dbg!(first_empty_space_idx);
                move_chunk(
                    &mut disk,
                    &mut first_empty_space_idx,
                    idx + 1,
                    size,
                    in_chunk_,
                );
            }
            // not in a chunk previously, in a chunk now
            (None, Some(chunk)) => {
                // only move chunks that haven't been moved yet
                if *chunk < highest_chunk_moved {
                    in_chunk = Some((*chunk, 1));
                }
            }
            // not in a chunk previously, not in a chunk now
            (None, None) => {}
        }
    }

    let mut checksum = 0;
    #[apply(iter)]
    for (idx, f) in enumerate(disk.as_slice()) {
        if let Some(f) = f {
            checksum += (f as usize) * idx
        }
    }

    checksum
}

const fn move_chunk(
    disk: &mut ArrayVec<Option<u16>, DISK_SIZE>,
    first_empty_space_idx: &mut usize,
    chunk_start: usize,
    chunk_len: usize,
    chunk_id: u16,
) {
    assert!(chunk_len <= 9);
    assert!(chunk_len > 0);

    let arr = disk.as_slice_mut();

    let mut idx = *first_empty_space_idx;

    let mut found_empty = false;

    while idx < (chunk_start + chunk_len) {
        if !found_empty && arr[idx].is_none() {
            found_empty = true;
            *first_empty_space_idx = idx;
        }

        'block: {
            #[apply(iter)]
            for i in range(0, chunk_len) {
                if arr[idx + i].is_some() {
                    break 'block;
                }
            }

            #[apply(iter)]
            for i in range(0, chunk_len) {
                debug_assert!(arr[chunk_start + i].unwrap() == chunk_id);
                arr.swap(idx + i, chunk_start + i);
            }

            return;
        }

        idx += 1;
    }
}

#[cfg(test)]
#[test]
fn test() {
    let input = "2333133121414131402";

    dbg!(parse2(input.as_bytes()));
    dbg!(parse2(Today::INPUT.as_bytes()));
}
