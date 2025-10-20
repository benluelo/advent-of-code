use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, max_u32, parse_u32, read_until, slice},
};

#[apply(day)]
impl Day<2023, 2> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }
    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

struct Game {
    red: u32,
    green: u32,
    blue: u32,
}

const fn parse(bytes: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for line in lines(bytes) {
        let (game_number, Game { red, green, blue }) = parse_line(line);

        if red <= 12 && green <= 13 && blue <= 14 {
            res += game_number;
        }
    }

    res
}

const fn parse2(bytes: &[u8]) -> u32 {
    let mut res = 0;

    #[apply(iter)]
    for line in lines(bytes) {
        let (_, Game { red, green, blue }) = parse_line(line);

        res += red * green * blue;
    }

    res
}

const fn parse_line(input: &[u8]) -> (u32, Game) {
    let mut game = Game {
        red: 0,
        green: 0,
        blue: 0,
    };

    let mut idx = 0;

    let b"Game" = read_until(input, idx, b" ") else {
        panic!()
    };

    idx += "Game".len() + 1;
    let game_number = read_until(slice(input, idx, input.len()), 0, b":");

    idx += game_number.len() + 1;
    while idx < input.len() {
        let remaining = slice(input, idx, input.len());

        let round = read_until(remaining, 0, b";");
        if round.is_empty() {
            break;
        }

        let mut round_idx = 0;
        while round_idx < round.len() {
            let remaining = slice(round, round_idx, round.len());

            let count_and_colour = read_until(remaining, 0, b",").trim_ascii_start();
            if count_and_colour.is_empty() {
                break;
            }

            let count = read_until(count_and_colour, 0, b" ");

            let colour =
                slice(count_and_colour, count.len(), count_and_colour.len()).trim_ascii_start();

            match colour {
                b"red" => {
                    game.red = max_u32(game.red, parse_u32(count));
                }
                b"green" => {
                    game.green = max_u32(game.green, parse_u32(count));
                }
                b"blue" => {
                    game.blue = max_u32(game.blue, parse_u32(count));
                }
                _ => panic!(),
            }

            // +2 to skip the space as well
            round_idx += count_and_colour.len() + 2;
        }

        idx += round.len() + 1;
    }

    (parse_u32(game_number), game)
}
