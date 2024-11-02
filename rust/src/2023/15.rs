use cfg_proc::apply;

use crate::{
    const_helpers::{array::ArrayVec, iter, slice_eq},
    day, Day,
};

#[apply(day)]
impl Day<2023, 15> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> usize {
        parse2(input)
    }
}

#[test]
#[cfg(test)]
fn parse_test() {
    // let input = b"rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

    let input = Day::<2023, 15>::INPUT.as_bytes();

    let score = parse2(input);

    println!("{score}");
}

const fn parse(input: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for bz in split(input, b",") {
        res += hash(bz) as u32;
    }

    res
}

const fn parse2(input: &[u8]) -> usize {
    let mut hashmap: HashMap = [const { ArrayVec::new() }; 256];

    #[apply(iter)]
    for bz in split(input, b",") {
        let (op, h) = hash2(bz);

        let label = bz
            .split_at(
                bz.len()
                    - match op {
                        Op::Add(_) => 2,
                        Op::Remove => 1,
                    },
            )
            .0;

        let slice = hashmap[h as usize].as_slice();

        let mut item = None;
        #[apply(iter)]
        for (idx, ref v) in enumerate(&slice) {
            if slice_eq(v.0, label) {
                item = Some(idx);
                break;
            }
        }

        match (op, item) {
            (Op::Add(value), None) => hashmap[h as usize].append((label, value)),
            (Op::Add(value), Some(existing_idx)) => {
                *hashmap[h as usize].get_mut(existing_idx) = (label, value);
            }
            (Op::Remove, None) => {}
            (Op::Remove, Some(existing_idx)) => {
                hashmap[h as usize].remove(existing_idx);
            }
        }
    }

    let mut res = 0;

    // dbg_array_vec(&hashmap);

    #[apply(iter)]
    for (box_idx, ref item) in enumerate(hashmap) {
        let slice = item.as_slice();

        if !slice.is_empty() {
            #[apply(iter)]
            for (slot, (_label, focal_length)) in enumerate(slice) {
                res += (box_idx + 1) * (slot + 1) * focal_length as usize;
            }
        }
    }

    res
}

const fn hash(bz: &[u8]) -> u8 {
    let mut res: u8 = 0;

    #[apply(iter)]
    for char in bz {
        res = res.wrapping_add(char);
        res = res.wrapping_mul(17);
    }

    res
}

const fn hash2(bz: &[u8]) -> (Op, u8) {
    let mut res: u8 = 0;

    #[apply(iter)]
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

    panic!()
}

// 6 might have to be increased for different inputs, but this worked for mine
type HashMap<'a> = [ArrayVec<(&'a [u8], u8), 6>; 256];

enum Op {
    Add(u8),
    Remove,
}

#[cfg(test)]
#[allow(dead_code)]
fn dbg_array_vec(hashmap: &HashMap) {
    use crate::const_helpers::utf8;

    let formatted = hashmap
        .iter()
        .enumerate()
        .map(|(box_idx, item)| {
            let slice = item.as_slice();

            if slice.is_empty() {
                String::new()
            } else {
                format!(
                    "Box {box_idx}: {}",
                    slice
                        .iter()
                        .map(|(label, focal_length)| {
                            format!("[{} {focal_length}]", utf8(label))
                        })
                        .collect::<Vec<_>>()
                        .join(" "),
                )
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    println!("{formatted}");
}
