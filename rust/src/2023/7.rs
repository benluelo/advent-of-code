use core::cmp::Ordering;

use cfg_proc::apply;

use crate::{
    const_helpers::{cmp, count_segments, iter, parse_int, utf8},
    day, Day,
};

#[apply(day)]
impl Day<2023, 7> {
    pub const fn parse(input: &[u8]) -> u128 {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> u128 {
        parse2(input)
    }
}

#[test]
#[cfg(test)]
fn parse_test() {
    let input = b"\
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
";

    dbg!(parse(input));
    dbg!(parse2(input));
}

const fn parse(input: &[u8]) -> u128 {
    parse_generic::<false>(input)
}

const fn parse2(input: &[u8]) -> u128 {
    parse_generic::<true>(input)
}

const fn parse_generic<const WITH_JOKERS: bool>(input: &[u8]) -> u128 {
    // find the max in the list
    // find the next hightest in the list, less than previous max
    // repeat

    let len = count_segments::<b'\n', true>(input);

    let mut max = None::<Hand<WITH_JOKERS>>;
    let mut res: u128 = 0;

    #[apply(iter)]
    for line in lines(input) {
        let bet = parse_int(line.split_at(5).1.trim_ascii_start()) as u128;
        let hand = parse_hand_from_line(line);

        if let Some(m) = &max {
            match hand.cmp(*m) {
                Ordering::Less | Ordering::Equal => {}
                Ordering::Greater => {
                    res = bet * len as u128;
                    max = Some(hand);
                }
            }
        } else {
            res = bet * len as u128;
            max = Some(hand);
        }
    }

    let Some(mut max) = max else { panic!() };

    let mut rank = len as u128 - 1; // already found max
    while rank != 0 {
        let mut curr_max = None::<Hand<WITH_JOKERS>>;
        let mut curr_res: u128 = 0;

        #[apply(iter)]
        for line in lines(input) {
            let bet = parse_int(line.split_at(5).1.trim_ascii_start()) as u128;
            let hand = parse_hand_from_line(line);

            match hand.cmp(max) {
                Ordering::Less => {}
                Ordering::Equal | Ordering::Greater => continue,
            }

            // hand is < the previous max here
            if let Some(m) = &curr_max {
                match hand.cmp(*m) {
                    Ordering::Less | Ordering::Equal => {}
                    Ordering::Greater => {
                        curr_res = bet;
                        curr_max = Some(hand);
                    }
                }
            } else {
                curr_res = bet;
                curr_max = Some(hand);
            }
        }

        res += curr_res * rank;
        let Some(new_max) = curr_max else { panic!() };
        max = new_max;

        rank -= 1;
    }

    res
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum HandKind {
    FiveOfAKind = 6,
    FourOfAKind = 5,
    FullHouse = 4,
    ThreeOfAKind = 3,
    TwoPair = 2,
    OnePair = 1,
    HighCard = 0,
}

impl<const WITH_JOKERS: bool> core::fmt::Display for Hand<WITH_JOKERS> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let bz = self.as_bytes();
        f.write_fmt(format_args!(
            "{:?}({}{}{}{}{})",
            self.hand_kind(),
            utf8(&[bz[0]]),
            utf8(&[bz[1]]),
            utf8(&[bz[2]]),
            utf8(&[bz[3]]),
            utf8(&[bz[4]]),
        ))
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Label {
    A = 14,
    K = 13,
    Q = 12,
    J = 11,
    T = 10,
    _9 = 9,
    _8 = 8,
    _7 = 7,
    _6 = 6,
    _5 = 5,
    _4 = 4,
    _3 = 3,
    _2 = 2,
}

impl Label {
    const fn cmp<const WITH_JOKERS: bool>(self, other: Self) -> Ordering {
        let l = if WITH_JOKERS && matches!(self, Self::J) {
            1
        } else {
            self as u8
        };
        let r = if WITH_JOKERS && matches!(other, Self::J) {
            1
        } else {
            other as u8
        };

        cmp!(l, r)
    }

    const fn as_byte(self) -> u8 {
        match self {
            Label::A => b'A',
            Label::K => b'K',
            Label::Q => b'Q',
            Label::J => b'J',
            Label::T => b'T',
            Label::_9 => b'9',
            Label::_8 => b'8',
            Label::_7 => b'7',
            Label::_6 => b'6',
            Label::_5 => b'5',
            Label::_4 => b'4',
            Label::_3 => b'3',
            Label::_2 => b'2',
        }
    }
}

const fn label_rank(label: u8) -> Label {
    match label {
        b'A' => Label::A,
        b'K' => Label::K,
        b'Q' => Label::Q,
        b'J' => Label::J,
        b'T' => Label::T,
        b'9' => Label::_9,
        b'8' => Label::_8,
        b'7' => Label::_7,
        b'6' => Label::_6,
        b'5' => Label::_5,
        b'4' => Label::_4,
        b'3' => Label::_3,
        b'2' => Label::_2,
        _ => panic!(),
    }
}

#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
enum Hand<const WITH_JOKERS: bool> {
    XXXXX { x: Label },
    XXXXY { x: Label, y: Label },
    XXXYY { x: Label, y: Label },
    XXYYY { x: Label, y: Label },
    XYYYY { x: Label, y: Label },
    XXYYX { x: Label, y: Label },
    XXYYZ { x: Label, y: Label, z: Label },
    XXXYX { x: Label, y: Label },
    XXXYZ { x: Label, y: Label, z: Label },
    XXYXX { x: Label, y: Label },
    XXYZZ { x: Label, y: Label, z: Label },
    XXYZW { x: Label, y: Label, z: Label, w: Label },
    XXYZY { x: Label, y: Label, z: Label },
    XXYXZ { x: Label, y: Label, z: Label },
    XXYZX { x: Label, y: Label, z: Label },
    XXYXY { x: Label, y: Label },
    XYYYX { x: Label, y: Label },
    XYYYZ { x: Label, y: Label, z: Label },
    XYYXX { x: Label, y: Label },
    XYYZZ { x: Label, y: Label, z: Label },
    XYYZY { x: Label, y: Label, z: Label },
    XYYZX { x: Label, y: Label, z: Label },
    XYYZW { x: Label, y: Label, z: Label, w: Label },
    XYYXZ { x: Label, y: Label, z: Label },
    XYYXY { x: Label, y: Label },
    XYXXX { x: Label, y: Label },
    XYZZZ { x: Label, y: Label, z: Label },
    XYZZW { x: Label, y: Label, z: Label, w: Label },
    XYZZY { x: Label, y: Label, z: Label },
    XYZZX { x: Label, y: Label, z: Label },
    XYXXY { x: Label, y: Label },
    XYXXZ { x: Label, y: Label, z: Label },
    XYXYY { x: Label, y: Label },
    XYZXX { x: Label, y: Label, z: Label },
    XYZYY { x: Label, y: Label, z: Label },
    XYXZZ { x: Label, y: Label, z: Label },
    XYZWW { x: Label, y: Label, z: Label, w: Label },
    XYXYX { x: Label, y: Label },
    XYXYZ { x: Label, y: Label, z: Label },
    XYXZX { x: Label, y: Label, z: Label },
    XYXZW { x: Label, y: Label, z: Label, w: Label },
    XYZYZ { x: Label, y: Label, z: Label },
    XYXZY { x: Label, y: Label, z: Label },
    XYZXZ { x: Label, y: Label, z: Label },
    XYZXY { x: Label, y: Label, z: Label },
    XYZYW { x: Label, y: Label, z: Label, w: Label },
    XYZWZ { x: Label, y: Label, z: Label, w: Label },
    XYZXW { x: Label, y: Label, z: Label, w: Label },
    XYZWX { x: Label, y: Label, z: Label, w: Label },
    XYZYX { x: Label, y: Label, z: Label },
    XYZWY { x: Label, y: Label, z: Label, w: Label },
    XYZWV { x: Label, y: Label, z: Label, w: Label, v: Label },
}

impl<const WITH_JOKERS: bool> Hand<WITH_JOKERS> {
    const fn cmp(self, other: Self) -> Ordering {
        let ordering = cmp!(self.hand_kind() as u8, other.hand_kind() as u8);

        match ordering {
            Ordering::Equal => cmp_cards::<WITH_JOKERS>(self.as_labels(), other.as_labels()),
            cmp => cmp,
        }
    }

    const fn hand_kind(self) -> HandKind {
        #[allow(clippy::enum_glob_use)]
        use {Hand::*, Label::J};

        match self {
            // always five of a kind, even if all jokers
            XXXXX { .. } => HandKind::FiveOfAKind,

            // if either x or y is a joker with any 2-label hand, best is five of a kind
            XXXYX { x, y } | XXYXX { x, y } | XXXXY { x, y } | XYYYY { x, y } | XYXXX { x, y } => {
                match (WITH_JOKERS, (x, y)) {
                    (true, (J, _) | (_, J)) => HandKind::FiveOfAKind,
                    _ => HandKind::FourOfAKind,
                }
            }

            XXXYY { x, y }
            | XXYYY { x, y }
            | XXYYX { x, y }
            | XXYXY { x, y }
            | XYYYX { x, y }
            | XYYXX { x, y }
            | XYXXY { x, y }
            | XYXYY { x, y }
            | XYXYX { x, y }
            | XYYXY { x, y } => match (WITH_JOKERS, (x, y)) {
                (true, (J, _) | (_, J)) => HandKind::FiveOfAKind,
                _ => HandKind::FullHouse,
            },

            // with 3 of a kind, if anything is a joker, the best is 4 of a kind
            XXXYZ { x, y, z }
            | XXYXZ { x, y, z }
            | XXYZX { x, y, z }
            | XYYYZ { x, y, z }
            | XYYZY { x, y, z }
            | XYZZZ { x, y, z }
            | XYXXZ { x, y, z }
            | XYZXX { x, y, z }
            | XYZYY { x, y, z }
            | XYXZX { x, y, z } => match (WITH_JOKERS, (x, y, z)) {
                (true, (J, _, _) | (_, J, _) | (_, _, J)) => HandKind::FourOfAKind,
                _ => HandKind::ThreeOfAKind,
            },

            // with 2 pair, if the single label is a joker, the best is a full house (joining with
            // either of the pairs), but if either of the pairs is a joker, then the best is 4 of a
            // kind (joining with the other pair)
            XXYYZ { x: p1, y: p2, z: s }
            | XXYZZ { x: p1, y: s, z: p2 }
            | XXYZY { x: p1, y: p2, z: s }
            | XYYZZ { x: s, y: p1, z: p2 }
            | XYYZX { x: p1, y: p2, z: s }
            | XYYXZ { x: p1, y: p2, z: s }
            | XYZZY { x: s, y: p1, z: p2 }
            | XYZZX { x: p1, y: s, z: p2 }
            | XYXZZ { x: p1, y: s, z: p2 }
            | XYXYZ { x: p1, y: p2, z: s }
            | XYZYZ { x: s, y: p1, z: p2 }
            | XYXZY { x: p1, y: p2, z: s }
            | XYZXZ { x: p1, y: s, z: p2 }
            | XYZXY { x: p1, y: p2, z: s }
            | XYZYX { x: p1, y: p2, z: s } => match (WITH_JOKERS, (p1, p2, s)) {
                (true, (J, _, _) | (_, J, _)) => HandKind::FourOfAKind,
                (true, (_, _, J)) => HandKind::FullHouse,
                _ => HandKind::TwoPair,
            },

            // with one pair, three of a kind is the best if anything is a joker
            XYZYW { x, y, z, w }
            | XYZWZ { x, y, z, w }
            | XYZXW { x, y, z, w }
            | XYZWX { x, y, z, w }
            | XYZWY { x, y, z, w }
            | XXYZW { x, y, z, w }
            | XYYZW { x, y, z, w }
            | XYZZW { x, y, z, w }
            | XYZWW { x, y, z, w }
            | XYXZW { x, y, z, w } => match (WITH_JOKERS, (x, y, z, w)) {
                (true, (J, _, _, _) | (_, J, _, _) | (_, _, J, _) | (_, _, _, J)) => {
                    HandKind::ThreeOfAKind
                }
                _ => HandKind::OnePair,
            },

            // with high card, one pair is the best if anything is a joker
            XYZWV { x, y, z, w, v } => match (WITH_JOKERS, (x, y, z, w, v)) {
                (
                    true,
                    (J, _, _, _, _)
                    | (_, J, _, _, _)
                    | (_, _, J, _, _)
                    | (_, _, _, J, _)
                    | (_, _, _, _, J),
                ) => HandKind::OnePair,
                _ => HandKind::HighCard,
            },
        }
    }
}

const fn cmp_cards<const WITH_JOKERS: bool>(l: [Label; 5], r: [Label; 5]) -> Ordering {
    #[apply(iter)]
    for i in range(0, 5) {
        match l[i].cmp::<WITH_JOKERS>(r[i]) {
            Ordering::Equal => continue,
            cmp => return cmp,
        }
    }

    Ordering::Equal
}

impl<const WITH_JOKERS: bool> Hand<WITH_JOKERS> {
    const fn as_labels(self) -> [Label; 5] {
        match self {
            Hand::XXXXX { x } => [x, x, x, x, x],
            Hand::XXXXY { x, y } => [x, x, x, x, y],
            Hand::XXXYY { x, y } => [x, x, x, y, y],
            Hand::XXYYY { x, y } => [x, x, y, y, y],
            Hand::XYYYY { x, y } => [x, y, y, y, y],
            Hand::XXYYX { x, y } => [x, x, y, y, x],
            Hand::XXYYZ { x, y, z } => [x, x, y, y, z],
            Hand::XXXYX { x, y } => [x, x, x, y, x],
            Hand::XXXYZ { x, y, z } => [x, x, x, y, z],
            Hand::XXYXX { x, y } => [x, x, y, x, x],
            Hand::XXYZZ { x, y, z } => [x, x, y, z, z],
            Hand::XXYZW { x, y, z, w } => [x, x, y, z, w],
            Hand::XXYZY { x, y, z } => [x, x, y, z, y],
            Hand::XXYXZ { x, y, z } => [x, x, y, x, z],
            Hand::XXYZX { x, y, z } => [x, x, y, z, x],
            Hand::XXYXY { x, y } => [x, x, y, x, y],
            Hand::XYYYX { x, y } => [x, y, y, y, x],
            Hand::XYYYZ { x, y, z } => [x, y, y, y, z],
            Hand::XYYXX { x, y } => [x, y, y, x, x],
            Hand::XYYZZ { x, y, z } => [x, y, y, z, z],
            Hand::XYYZY { x, y, z } => [x, y, y, z, y],
            Hand::XYYZX { x, y, z } => [x, y, y, z, x],
            Hand::XYYZW { x, y, z, w } => [x, y, y, z, w],
            Hand::XYYXZ { x, y, z } => [x, y, y, x, z],
            Hand::XYYXY { x, y } => [x, y, y, x, y],
            Hand::XYXXX { x, y } => [x, y, x, x, x],
            Hand::XYZZZ { x, y, z } => [x, y, z, z, z],
            Hand::XYZZW { x, y, z, w } => [x, y, z, z, w],
            Hand::XYZZY { x, y, z } => [x, y, z, z, y],
            Hand::XYZZX { x, y, z } => [x, y, z, z, x],
            Hand::XYXXY { x, y } => [x, y, x, x, y],
            Hand::XYXXZ { x, y, z } => [x, y, x, x, z],
            Hand::XYXYY { x, y } => [x, y, x, y, y],
            Hand::XYZXX { x, y, z } => [x, y, z, x, x],
            Hand::XYZYY { x, y, z } => [x, y, z, y, y],
            Hand::XYXZZ { x, y, z } => [x, y, x, z, z],
            Hand::XYZWW { x, y, z, w } => [x, y, z, w, w],
            Hand::XYXYX { x, y } => [x, y, x, y, x],
            Hand::XYXYZ { x, y, z } => [x, y, x, y, z],
            Hand::XYXZX { x, y, z } => [x, y, x, z, x],
            Hand::XYXZW { x, y, z, w } => [x, y, x, z, w],
            Hand::XYZYZ { x, y, z } => [x, y, z, y, z],
            Hand::XYXZY { x, y, z } => [x, y, x, z, y],
            Hand::XYZXZ { x, y, z } => [x, y, z, x, z],
            Hand::XYZXY { x, y, z } => [x, y, z, x, y],
            Hand::XYZYW { x, y, z, w } => [x, y, z, y, w],
            Hand::XYZWZ { x, y, z, w } => [x, y, z, w, z],
            Hand::XYZXW { x, y, z, w } => [x, y, z, x, w],
            Hand::XYZWX { x, y, z, w } => [x, y, z, w, x],
            Hand::XYZYX { x, y, z } => [x, y, z, y, x],
            Hand::XYZWY { x, y, z, w } => [x, y, z, w, y],
            Hand::XYZWV { x, y, z, w, v } => [x, y, z, w, v],
        }
    }

    const fn as_bytes(self) -> [u8; 5] {
        let [a, b, c, d, e] = self.as_labels();
        [
            a.as_byte(),
            b.as_byte(),
            c.as_byte(),
            d.as_byte(),
            e.as_byte(),
        ]
    }
}

#[deny(unreachable_patterns)]
#[allow(clippy::too_many_lines)]
const fn parse_hand_from_line<const WITH_JOKERS: bool>(hand_bytes: &[u8]) -> Hand<WITH_JOKERS> {
    const EQ: bool = true;
    const NE: bool = false;

    let [a, b, c, d, e, ..] = hand_bytes else {
        panic!()
    };

    let a = label_rank(*a);
    let b = label_rank(*b);
    let c = label_rank(*c);
    let d = label_rank(*d);
    let e = label_rank(*e);

    match (
        a.as_byte() == b.as_byte(),
        b.as_byte() == c.as_byte(),
        c.as_byte() == d.as_byte(),
        d.as_byte() == e.as_byte(),
        (
            a.as_byte() == c.as_byte(),
            a.as_byte() == d.as_byte(),
            a.as_byte() == e.as_byte(),
            b.as_byte() == d.as_byte(),
            b.as_byte() == e.as_byte(),
            c.as_byte() == e.as_byte(),
        ),
    ) {
        // X X X X X
        (EQ, EQ, EQ, EQ, _) => Hand::XXXXX { x: a },
        // X X X X Y
        (EQ, EQ, EQ, NE, _) => Hand::XXXXY { x: a, y: e },
        // X X X Y Y
        (EQ, EQ, NE, EQ, _) => Hand::XXXYY { x: a, y: d },
        // X X Y Y Y
        (EQ, NE, EQ, EQ, _) => Hand::XXYYY { x: a, y: c },
        // X Y Y Y Y
        (NE, EQ, EQ, EQ, _) => Hand::XYYYY { x: a, y: b },

        // X X Y Y ?
        // X X Y Y X      ac  ad  ae  bd  be  ce
        (EQ, NE, EQ, NE, (NE, NE, EQ, NE, EQ, NE)) => Hand::XXYYX { x: a, y: c },
        // X X Y Y Z
        (EQ, NE, EQ, NE, (NE, NE, NE, NE, NE, NE)) => Hand::XXYYZ { x: a, y: c, z: e },

        // X X X ? ?
        // X X X Y X
        (EQ, EQ, NE, NE, (EQ, NE, EQ, NE, EQ, EQ)) => Hand::XXXYX { x: a, y: d },
        // X X X Y Z
        (EQ, EQ, NE, NE, (EQ, NE, NE, NE, NE, NE)) => Hand::XXXYZ { x: a, y: d, z: e },

        // X X Y X X
        (EQ, NE, NE, EQ, (NE, EQ, EQ, EQ, EQ, NE)) => Hand::XXYXX { x: a, y: c },
        // X X Y Z Z
        (EQ, NE, NE, EQ, (NE, NE, NE, NE, NE, NE)) => Hand::XXYZZ { x: a, y: c, z: d },

        // X X ? ? ?
        // X X Y Z W
        (EQ, NE, NE, NE, (_, _, _, NE, NE, NE)) => Hand::XXYZW {
            x: a,
            y: c,
            z: d,
            w: e,
        },
        // X X Y Z Y
        (EQ, NE, NE, NE, (_, _, _, NE, NE, EQ)) => Hand::XXYZY { x: a, y: c, z: d },
        // X X Y X Z
        (EQ, NE, NE, NE, (_, _, _, EQ, NE, NE)) => Hand::XXYXZ { x: a, y: c, z: e },
        // X X Y Z X
        (EQ, NE, NE, NE, (NE, NE, EQ, NE, EQ, NE)) => Hand::XXYZX { x: a, y: c, z: d },
        // X X Y X Y
        (EQ, NE, NE, NE, (NE, EQ, NE, EQ, NE, EQ)) => Hand::XXYXY { x: a, y: c },

        // ? X X X ?
        // X Y Y Y X
        (NE, EQ, EQ, NE, (NE, NE, EQ, EQ, NE, NE)) => Hand::XYYYX { x: a, y: b },
        // X Y Y Y Z
        (NE, EQ, EQ, NE, (NE, NE, NE, EQ, NE, NE)) => Hand::XYYYZ { x: a, y: b, z: e },

        // X Y Y [? ?]
        // X Y Y X X
        (NE, EQ, NE, EQ, (NE, EQ, EQ, NE, NE, NE)) => Hand::XYYXX { x: a, y: b },
        // X Y Y Z Z
        (NE, EQ, NE, EQ, (NE, NE, NE, NE, NE, NE)) => Hand::XYYZZ { x: a, y: b, z: d },

        // X Y Y ? ?
        // X Y Y Z Y
        (NE, EQ, NE, NE, (NE, NE, NE, NE, EQ, EQ)) => Hand::XYYZY { x: a, y: b, z: d },
        // X Y Y Z X
        (NE, EQ, NE, NE, (NE, NE, EQ, NE, NE, NE)) => Hand::XYYZX { x: a, y: b, z: d },
        // X Y Y Z W
        (NE, EQ, NE, NE, (NE, NE, NE, NE, NE, NE)) => Hand::XYYZW {
            x: a,
            y: b,
            z: d,
            w: e,
        },
        // X Y Y X Z
        (NE, EQ, NE, NE, (NE, EQ, NE, NE, NE, NE)) => Hand::XYYXZ { x: a, y: b, z: e },
        // X Y Y X Y
        (NE, EQ, NE, NE, (NE, EQ, NE, NE, EQ, EQ)) => Hand::XYYXY { x: a, y: b },

        // X Y X X X
        (NE, NE, EQ, EQ, (EQ, EQ, EQ, NE, NE, EQ)) => Hand::XYXXX { x: a, y: b },
        // X Y Z Z Z
        (NE, NE, EQ, EQ, (NE, NE, NE, NE, NE, EQ)) => Hand::XYZZZ { x: a, y: b, z: c },

        // ? ? Y Y ?
        // NOTE: a == e && b == e cannot hold since a != b
        // X Y Z Z W
        (NE, NE, EQ, NE, (NE, NE, NE, NE, NE, NE)) => Hand::XYZZW {
            x: a,
            y: b,
            z: c,
            w: e,
        },
        // X Y Z Z Y
        (NE, NE, EQ, NE, (NE, NE, NE, NE, EQ, NE)) => Hand::XYZZY { x: a, y: b, z: d },
        // X Y Z Z X
        (NE, NE, EQ, NE, (NE, NE, EQ, NE, NE, NE)) => Hand::XYZZX { x: a, y: b, z: c },
        // X Y X X Y
        (NE, NE, EQ, NE, (EQ, EQ, NE, NE, EQ, NE)) => Hand::XYXXY { x: a, y: b },
        // X Y X X Z
        (NE, NE, EQ, NE, (EQ, EQ, NE, NE, NE, NE)) => Hand::XYXXZ { x: a, y: b, z: e },

        // compare ac ae be
        // NOTE: c == e cannot hold since c != d
        // ? ? ? Y Y
        // X Y X Y Y
        (NE, NE, NE, EQ, (EQ, NE, NE, EQ, EQ, NE)) => Hand::XYXYY { x: a, y: b },
        // X Y Z X X
        (NE, NE, NE, EQ, (NE, EQ, EQ, NE, NE, NE)) => Hand::XYZXX { x: a, y: b, z: c },
        // X Y Z Y Y
        (NE, NE, NE, EQ, (NE, NE, NE, EQ, EQ, NE)) => Hand::XYZYY { x: a, y: b, z: c },
        // X Y X Z Z
        (NE, NE, NE, EQ, (EQ, NE, NE, NE, NE, NE)) => Hand::XYXZZ { x: a, y: b, z: d },
        // X Y Z W W
        (NE, NE, NE, EQ, (NE, NE, NE, NE, NE, NE)) => Hand::XYZWW {
            x: a,
            y: b,
            z: c,
            w: d,
        },

        // compare all
        // ? ? ? ? ?
        // X Y X Y X
        (NE, NE, NE, NE, (EQ, NE, EQ, EQ, NE, EQ)) => Hand::XYXYX { x: a, y: b },
        // X Y X Y Z
        (NE, NE, NE, NE, (EQ, NE, NE, EQ, NE, NE)) => Hand::XYXYZ { x: a, y: b, z: e },
        // X Y X Z X
        (NE, NE, NE, NE, (EQ, NE, EQ, NE, NE, EQ)) => Hand::XYXZX { x: a, y: b, z: d },
        // X Y X Z W
        (NE, NE, NE, NE, (EQ, NE, NE, NE, NE, NE)) => Hand::XYXZW {
            x: a,
            y: b,
            z: d,
            w: e,
        },

        // X Y Z Y Z
        (NE, NE, NE, NE, (NE, NE, NE, EQ, NE, EQ)) => Hand::XYZYZ { x: a, y: b, z: c },
        // X Y X Z Y
        (NE, NE, NE, NE, (EQ, NE, NE, NE, EQ, NE)) => Hand::XYXZY { x: a, y: b, z: d },
        // X Y Z X Z
        (NE, NE, NE, NE, (NE, EQ, NE, NE, NE, EQ)) => Hand::XYZXZ { x: a, y: b, z: c },
        // X Y Z X Y
        (NE, NE, NE, NE, (NE, EQ, NE, NE, EQ, NE)) => Hand::XYZXY { x: a, y: b, z: c },
        // X Y Z Y W
        (NE, NE, NE, NE, (NE, NE, NE, EQ, NE, NE)) => Hand::XYZYW {
            x: a,
            y: b,
            z: c,
            w: e,
        },
        // X Y Z W Z
        (NE, NE, NE, NE, (NE, NE, NE, NE, NE, EQ)) => Hand::XYZWZ {
            x: a,
            y: b,
            z: c,
            w: d,
        },

        // X Y Z X W
        (NE, NE, NE, NE, (NE, EQ, NE, NE, NE, NE)) => Hand::XYZXW {
            x: a,
            y: b,
            z: c,
            w: e,
        },
        // X Y Z W X
        (NE, NE, NE, NE, (NE, NE, EQ, NE, NE, NE)) => Hand::XYZWX {
            x: a,
            y: b,
            z: c,
            w: d,
        },
        // X Y Z Y X
        (NE, NE, NE, NE, (NE, NE, EQ, EQ, NE, NE)) => Hand::XYZYX { x: a, y: b, z: c },
        // X Y Z W Y
        (NE, NE, NE, NE, (NE, NE, NE, NE, EQ, NE)) => Hand::XYZWY {
            x: a,
            y: b,
            z: c,
            w: d,
        },

        // X Y Z W V
        (NE, NE, NE, NE, (NE, NE, NE, NE, NE, NE)) => Hand::XYZWV {
            x: a,
            y: b,
            z: c,
            w: d,
            v: e,
        },

        _ => unreachable!(),
    }
}
