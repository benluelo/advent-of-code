use cfg_proc::apply;

use crate::{
    day,
    utils::{iter, read_until, slice, slice_eq, utf8},
    Day,
};

#[apply(day)]
impl Day<2023, 8> {
    pub const fn parse(input: &[u8]) -> usize {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> u128 {
        parse2(input)
    }
}

#[test]
#[cfg(test)]
fn parse_test() {
    //     let input = b"\
    // RL

    // AAA = (BBB, CCC)
    // BBB = (DDD, EEE)
    // CCC = (ZZZ, GGG)
    // DDD = (DDD, DDD)
    // EEE = (EEE, EEE)
    // GGG = (GGG, GGG)
    // ZZZ = (ZZZ, ZZZ)
    // ";

    //     let input2 = b"\
    // LLR

    // AAA = (BBB, BBB)
    // BBB = (AAA, ZZZ)
    // ZZZ = (ZZZ, ZZZ)
    // ";

    dbg!(parse2(Day::<2023, 8>::INPUT.as_bytes()));
    // dbg!(parse(input2));
    // dbg!(parse2(input));
}

#[derive(PartialEq, Clone)]
struct MapInstruction {
    key: [u8; 3],
    left: [u8; 3],
    right: [u8; 3],
}

impl core::fmt::Debug for MapInstruction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("MapInstruction")
            .field("key", &utf8(&self.key))
            .field("left", &utf8(&self.left))
            .field("right", &utf8(&self.right))
            .finish()
    }
}

const fn parse_line(bz: &[u8]) -> MapInstruction {
    let [k, e, y, b' ', b'=', b' ', b'(', le, f, t, b',', b' ', r, ig, ht, b')'] = bz else {
        panic!()
    };

    MapInstruction {
        key: [*k, *e, *y],
        left: [*le, *f, *t],
        right: [*r, *ig, *ht],
    }
}

const fn parse(bytes: &[u8]) -> usize {
    let directions = read_until(bytes, 0, b"\n\n");

    let mappings = bytes.split_at(directions.len() + 2).1;

    let start = find_node(mappings, *b"AAA");
    let mut curr = start;

    let mut direction_counter = 0;
    loop {
        let dir = directions[direction_counter % directions.len()];
        let next_instr = match dir {
            b'L' => curr.left,
            b'R' => curr.right,
            _ => panic!(),
        };

        if matches!(&next_instr, b"ZZZ") {
            // count the first step as well
            break direction_counter + 1;
        }

        curr = find_node(mappings, next_instr);

        direction_counter += 1;
    }
}

const fn parse2(bytes: &[u8]) -> u128 {
    let directions = read_until(bytes, 0, b"\n\n");

    let mappings = bytes.split_at(directions.len() + 2).1;

    let mut res = 0;

    #[apply(iter)]
    for line in lines(mappings) {
        if matches!(&line, [_, _, b'A', ..]) {
            let start = find_node(mappings, [line[0], line[1], line[2]]);
            let mut curr = start;

            let mut direction_counter = 0;
            let steps = loop {
                let dir = directions[direction_counter % directions.len()];
                let next_instr = match dir {
                    b'L' => curr.left,
                    b'R' => curr.right,
                    _ => panic!(),
                };

                if matches!(&next_instr, [_, _, b'Z']) {
                    // count the first step as well
                    break direction_counter + 1;
                }

                curr = find_node(mappings, next_instr);

                direction_counter += 1;
            };

            res = if res == 0 {
                steps as u128
            } else {
                lcm(steps as u128, res)
            };
        }
    }

    res
}

// https://docs.rs/num-integer/0.1.45/src/num_integer/lib.rs.html#855
const fn lcm(a: u128, b: u128) -> u128 {
    a * (b / gcd(a, b))
}

const fn gcd(mut a: u128, mut b: u128) -> u128 {
    // Use Stein's algorithm
    if a == 0 || b == 0 {
        return a | b;
    }

    // find common factors of 2
    let shift = (a | b).trailing_zeros();

    // divide n and m by 2 ntil odd
    a >>= a.trailing_zeros();
    b >>= b.trailing_zeros();

    while a != b {
        if a > b {
            a -= b;
            a >>= a.trailing_zeros();
        } else {
            b -= a;
            b >>= b.trailing_zeros();
        }
    }
    a << shift
}

const fn find_node(mappings: &[u8], key: [u8; 3]) -> MapInstruction {
    #[apply(iter)]
    for line in lines(mappings) {
        if slice_eq(slice(line, 0, 3), &key) {
            return parse_line(line);
        }
    }

    panic!()
}
