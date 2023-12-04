use crate::{
    const_helpers::{itoa, max, parse_int, read_until, slice, split, utf8},
    ConstDaySolution, Day, Input,
};

impl ConstDaySolution for Day<2023, 2> {
    const PART_1: &'static str = utf8(&itoa!(SOLUTION_PART_1));
    const PART_2: &'static str = utf8(&itoa!(SOLUTION_PART_2));
}

const SOLUTION_PART_1: u32 =
    parse(split!(Day::<2023, 2>::INPUT.as_bytes(), b'\n', true).as_slice());

const SOLUTION_PART_2: u32 =
    parse2(split!(Day::<2023, 2>::INPUT.as_bytes(), b'\n', true).as_slice());

struct Game {
    red: u32,
    green: u32,
    blue: u32,
}

const fn parse(bytes: &'static [&'static [u8]]) -> u32 {
    let mut res = 0;

    let mut i = 0;
    while i < bytes.len() {
        let (game_number, Game { red, green, blue }) = parse_line(bytes[i]);

        if red <= 12 && green <= 13 && blue <= 14 {
            res += game_number;
        }

        i += 1;
    }

    res
}

const fn parse2(bytes: &'static [&'static [u8]]) -> u32 {
    let mut res = 0;

    let mut i = 0;
    while i < bytes.len() {
        let (_, Game { red, green, blue }) = parse_line(bytes[i]);

        res += red * green * blue;

        i += 1;
    }

    res
}

const fn parse_line(input: &'static [u8]) -> (u32, Game) {
    let mut game = Game {
        red: 0,
        green: 0,
        blue: 0,
    };

    let mut idx = 0;

    let b"Game" = read_until(input, idx, b' ') else {
        panic!()
    };

    idx += "Game".len() + 1;
    let game_number = read_until(slice(input, idx, input.len()), 0, b':');

    idx += game_number.len() + 1;
    while idx < input.len() {
        let remaining = slice(input, idx, input.len());

        let round = read_until(remaining, 0, b';');
        if round.is_empty() {
            break;
        }

        let mut round_idx = 0;
        while round_idx < round.len() {
            let remaining = slice(round, round_idx, round.len());

            let count_and_colour = read_until(remaining, 0, b',').trim_ascii_start();
            if count_and_colour.is_empty() {
                break;
            }

            let count = read_until(count_and_colour, 0, b' ');

            let colour =
                slice(count_and_colour, count.len(), count_and_colour.len()).trim_ascii_start();

            match colour {
                b"red" => {
                    game.red = max(game.red, parse_int(count));
                }
                b"green" => {
                    game.green = max(game.green, parse_int(count));
                }
                b"blue" => {
                    game.blue = max(game.blue, parse_int(count));
                }
                _ => panic!(),
            }

            // +2 to skip the space as well
            round_idx += count_and_colour.len() + 2;
        }

        idx += round.len() + 1;
    }

    (parse_int(game_number), game)
}
