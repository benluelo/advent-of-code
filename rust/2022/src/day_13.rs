use std::{
    convert::Infallible,
    fmt::Debug,
    iter::{self, Peekable},
    ops::ControlFlow,
    str::FromStr,
};

pub fn solution(input: &str) -> u32 {
    input
        .split("\n\n")
        .map(|packets| {
            packets
                .split('\n')
                .map(|packet| packet.parse::<PacketData>().unwrap())
                .next_chunk()
                .unwrap()
        })
        .zip(1..)
        .filter_map(|([lhs, rhs], idx)| {
            lhs.check_if_in_order(&rhs)
                .break_value()
                .unwrap()
                .then_some(idx)
        })
        .sum()
}

pub fn solution_part_2(_input: &str) -> ! {
    todo!()
    //
}

#[derive(Debug, PartialEq)]
enum PacketData {
    Int(u8),
    List(Vec<PacketData>),
}

impl PacketData {
    fn check_if_in_order(&self, other: &Self) -> ControlFlow<bool, ()> {
        use std::cmp::Ordering::{Equal, Greater, Less};
        use std::ops::ControlFlow::{Break, Continue};
        use PacketData::{Int, List};

        match (self, other) {
            (Int(lhs), Int(rhs)) => match lhs.cmp(rhs) {
                Less => Break(true),
                Equal => Continue(()),
                Greater => Break(false),
            },
            (Int(int), list @ List(_)) => List(vec![Int(*int)]).check_if_in_order(list),
            (list @ List(_), Int(int)) => list.check_if_in_order(&List(vec![Int(*int)])),
            (List(lhs), List(rhs)) => {
                for idx in 0.. {
                    match (lhs.get(idx), rhs.get(idx)) {
                        (None, None) => return Continue(()),
                        (None, Some(_)) => return Break(true),
                        (Some(_), None) => return Break(false),
                        (Some(l), Some(r)) => match l.check_if_in_order(r) {
                            Continue(()) => continue,
                            Break(is_in_order) => return Break(is_in_order),
                        },
                    }
                }

                unreachable!()
            }
        }
    }
}

impl FromStr for PacketData {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn list(chars: &mut Peekable<impl Iterator<Item = char> + Debug>) -> Vec<PacketData> {
            iter::from_fn(|| {
                let item = if chars.next_if_eq(&'[').is_some() {
                    PacketData::List(list(chars))
                } else if let Some(']') | None = chars.peek() {
                    chars.next();
                    return None;
                } else {
                    PacketData::Int(int(chars))
                };

                chars.next_if_eq(&',');

                Some(item)
            })
            .collect()
        }

        fn int(chars: &mut Peekable<impl Iterator<Item = char>>) -> u8 {
            let mut digits: Vec<u8> = vec![];
            while let Some(digit) = chars
                .next_if(char::is_ascii_digit)
                .map(|c| c.to_digit(10).unwrap().try_into().unwrap())
            {
                digits.push(digit);
            }

            digits
                .into_iter()
                .rev()
                .zip(0..)
                .fold(0, |acc, (int, idx)| (int * (10_u8).pow(idx)) + acc)
        }

        let mut chars = s.chars().peekable();

        let '[' = chars.next().unwrap() else { panic!("bad input") };

        Ok(PacketData::List(list(&mut chars)))
    }
}

#[cfg(test)]
mod tests {
    use crate::day_13::PacketData;

    use super::solution;

    const TEST_INPUT: &str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

    #[test]
    fn test() {
        assert_eq!(solution(TEST_INPUT), 13);
    }

    #[test]
    fn multiple_digit_characters() {
        use super::PacketData::{Int, List};

        assert_eq!(
            "[10,10,[33,1,[]]]".parse::<PacketData>().unwrap(),
            List(vec![
                Int(10),
                Int(10),
                List(vec![Int(33), Int(1), List(vec![])])
            ])
        );
    }
}
