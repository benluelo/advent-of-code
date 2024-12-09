use cfg_proc::apply;

use crate::{
    day,
    utils::{array::ArrayVec, iter, parse_u8, read_until, slice, split_once},
    Day,
};

#[apply(day)]
impl Day<2022, 5> {
    pub const fn parse(input: &[u8]) -> ArrayVec<u8, MAX_CRATES_LEN> {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> ArrayVec<u8, MAX_CRATES_LEN> {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> ArrayVec<u8, MAX_CRATES_LEN> {
    let (crates, actions) = split_once(input, b"\n\n").unwrap();
    let mut crates = parse_crate_stack(crates);

    let mut output = ArrayVec::<u8, MAX_CRATES_LEN>::new();

    #[apply(iter)]
    for line in lines(actions) {
        let action = Action::parse(line);
        #[apply(iter)]
        for _ in range(0, action.mov) {
            let moved = crates.get_mut(action.from as usize - 1).pop();
            crates.get_mut(action.to as usize - 1).append(moved);
        }
    }

    #[apply(iter)]
    for v in iter_mut(crates.as_slice_mut()) {
        output.append(v.pop());
    }

    output
}

const fn parse2(input: &[u8]) -> ArrayVec<u8, MAX_CRATES_LEN> {
    let (crates, actions) = split_once(input, b"\n\n").unwrap();
    let mut crates = parse_crate_stack(crates);

    let mut output = ArrayVec::<u8, MAX_CRATES_LEN>::new();

    #[apply(iter)]
    for line in lines(actions) {
        let action = Action::parse(line);

        // 31 is the largest move action in my input
        let mut stack = ArrayVec::<u8, 31>::new();

        #[apply(iter)]
        for _ in range(0, action.mov) {
            let moved = crates.get_mut(action.from as usize - 1).pop();
            stack.push(moved);
        }

        #[apply(iter)]
        for c in iter(stack.as_slice()) {
            crates.get_mut(action.to as usize - 1).append(*c);
        }
    }

    #[apply(iter)]
    for v in iter_mut(crates.as_slice_mut()) {
        output.append(v.pop());
    }

    output
}

#[derive(Debug)]
struct Action {
    mov: u8,
    from: u8,
    to: u8,
}

impl Action {
    const fn parse(action: &[u8]) -> Self {
        let (b"move", rest) = split_once(action, b" ").unwrap() else {
            panic!()
        };
        let (mov, rest) = split_once(rest, b" ").unwrap();

        let (b"from", rest) = split_once(rest, b" ").unwrap() else {
            panic!()
        };
        let (from, rest) = split_once(rest, b" ").unwrap();

        let (b"to", to) = split_once(rest, b" ").unwrap() else {
            panic!()
        };

        Action {
            mov: parse_u8(mov),
            from: parse_u8(from),
            to: parse_u8(to),
        }
    }
}

const MAX_CRATES_LEN: usize = 9;

// 48 is the maximum crate stack size given my input
type CrateStacks = ArrayVec<ArrayVec<u8, 48>, MAX_CRATES_LEN>;

const fn parse_crate_stack(crates: &[u8]) -> CrateStacks {
    let mut output = ArrayVec::new();

    let mut cursor = 0;

    let crates_len = (read_until(crates, 0, b"\n").len() + 1) / 4;

    #[apply(iter)]
    for _ in range(0, crates_len) {
        output.push(ArrayVec::new())
    }

    loop {
        match slice(crates, cursor, cursor + 4) {
            [b'[', c, b']', _] => {
                output.get_mut((cursor % (crates_len * 4)) / 4).append(*c);
                cursor += 4;
            }
            [b' ', b' ', b' ', _] => {
                cursor += 4;
            }
            // the last row has been hit, reverse all of the stacks and return
            [b' ', b'1', b' ', _] => {
                #[apply(iter)]
                for v in iter_mut(output.as_slice_mut()) {
                    v.reverse();
                }
                return output;
            }
            _ => panic!(),
        }
    }
}

#[test]
fn test() {
    let input = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
";

    dbg!(parse2(input.as_bytes()).as_str());
    dbg!(parse2(Today::INPUT.as_bytes()).as_str());
}
