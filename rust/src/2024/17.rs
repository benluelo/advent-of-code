use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{array::ArrayVec, iter, parse_u8, parse_u64, split_once},
};

#[apply(day)]
impl Day<2024, 17> {
    pub const fn parse(input: &[u8]) -> ArrayVec<u8, 32> {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u64 {
        // parse2(input)
        0
    }
}

const fn parse(input: &[u8]) -> ArrayVec<u8, 32> {
    let mut program = Program::<32>::from_input(input);

    while program.step() {
        // dbg!(&program);
    }

    let mut out = ArrayVec::<_, 32>::new();

    #[apply(iter)]
    for (idx, op) in enumerate(program.out.as_slice()) {
        out.append(op + 48);
        if idx < program.out.len() - 1 {
            out.append(b',');
        }
    }

    out
}

fn parse2(input: &[u8]) -> u64 {
    let program = Program::<32>::from_input(input);

    let mut a: u64 = 0;

    loop {
        if a.is_multiple_of(1_000_000) {
            dbg!(a);
        }

        let mut program = Program::<32>::new(a, program.b, program.c, program.program.copied());

        while program.step() {
            if program.out.len() > program.program.len() {
                break;
            }

            if !program
                .program
                .as_slice()
                .starts_with(program.out.as_slice())
            {
                break;
            }
        }

        if program.out.as_slice() == program.program.as_slice() {
            return a;
        }

        a += 1;
    }
}

#[derive(Debug)]
struct Program<const N: usize> {
    a: u64,
    b: u64,
    c: u64,

    ip: usize,

    program: ArrayVec<u8, N>,

    out: ArrayVec<u8, N>,
}

impl<const N: usize> Program<N> {
    const fn new(a: u64, b: u64, c: u64, program: ArrayVec<u8, N>) -> Self {
        Self {
            a,
            b,
            c,
            ip: 0,
            program,
            out: ArrayVec::new(),
        }
    }

    const fn from_input(input: &[u8]) -> Self {
        let Some((a, input)) = split_once(input, b"\n") else {
            panic!()
        };

        let Some((b, input)) = split_once(input, b"\n") else {
            panic!()
        };

        let Some((c, input)) = split_once(input, b"\n") else {
            panic!()
        };

        let (Some(([], a)), Some(([], b)), Some(([], c))) = (
            split_once(a, b"Register A: "),
            split_once(b, b"Register B: "),
            split_once(c, b"Register C: "),
        ) else {
            panic!()
        };

        let a = parse_u64(a);
        let b = parse_u64(b);
        let c = parse_u64(c);

        let Some(([], program_bz)) = split_once(input, b"\nProgram: ") else {
            panic!()
        };

        let mut program = ArrayVec::new();

        #[apply(iter)]
        for op in split(program_bz.trim_ascii(), b",") {
            program.append(parse_u8(op));
        }

        Program::<N>::new(a, b, c, program)
    }

    const fn eat(&mut self) -> Option<u8> {
        if self.ip >= self.program.len() {
            None
        } else {
            let res = self.program.get(self.ip);
            self.ip += 1;
            Some(*res)
        }
    }

    const fn read_combo_operand(&self, raw: u8) -> u64 {
        match raw {
            n @ 0..=3 => n as u64,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            _ => unreachable!(),
        }
    }

    const fn step(&mut self) -> bool {
        match (self.eat(), self.eat()) {
            (Some(i), Some(op)) => {
                match i {
                    ADV => {
                        let op = self.read_combo_operand(op);
                        assert!(op <= u32::MAX as u64);
                        self.a /= 2_u64.strict_pow(op as u32);
                    }
                    BXL => {
                        self.b ^= op as u64;
                    }
                    BST => {
                        self.b = self.read_combo_operand(op) % 8;
                    }
                    JNZ => {
                        if self.a != 0 {
                            self.ip = (self.read_combo_operand(op) % 8) as usize;
                        }
                    }
                    BXC => {
                        self.b ^= self.c;
                    }
                    OUT => {
                        self.out.append((self.read_combo_operand(op) % 8) as u8);
                    }
                    BDV => {
                        let op = self.read_combo_operand(op);
                        assert!(op <= u32::MAX as u64);
                        self.b = self.a / 2_u64.strict_pow(op as u32);
                    }
                    CDV => {
                        let op = self.read_combo_operand(op);
                        assert!(op <= u32::MAX as u64);
                        self.c = self.a / 2_u64.strict_pow(op as u32);
                    }
                    _ => unreachable!(),
                }

                true
            }
            _ => false,
        }
    }
}

const ADV: u8 = 0;
const BXL: u8 = 1;
const BST: u8 = 2;
const JNZ: u8 = 3;
const BXC: u8 = 4;
const OUT: u8 = 5;
const BDV: u8 = 6;
const CDV: u8 = 7;

#[cfg(test)]
#[test]
fn test() {
    use crate::utils::utf8;

    let input = "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0";

    let input2 = "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0";

    // dbg!(utf8(parse(Today::INPUT.as_bytes()).as_slice()));
    // dbg!(parse2(input2.as_bytes()));
    dbg!(parse2(Today::INPUT.as_bytes()));

    // let mut program = Program::<32>::new(0, 29, 0, b"1,7");

    // while program.step() {
    //     // dbg!(&program);
    // }

    // dbg!(&program);

    // dbg!(utf8(program.out.as_slice()));
}
