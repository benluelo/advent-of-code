use cfg_proc::apply;

use crate::{
    Day, day,
    utils::{iter, split_once},
};

#[apply(day)]
impl Day<2022, 2> {
    pub const fn parse(input: &[u8]) -> u32 {
        parse(input)
    }

    pub const fn parse2(input: &[u8]) -> u32 {
        parse2(input)
    }
}

const fn parse(input: &[u8]) -> u32 {
    let mut total = 0;

    #[apply(iter)]
    for line in lines(input.trim_ascii()) {
        total += Round::parse(line).score();
    }

    total
}

const fn parse2(input: &[u8]) -> u32 {
    let mut total = 0;

    #[apply(iter)]
    for line in lines(input.trim_ascii()) {
        total += IncompleteRound::parse(line).into_round().score();
    }

    total
}

enum Rps {
    Rock,
    Paper,
    Scissors,
}

impl Rps {
    const fn points(&self) -> u32 {
        match self {
            Rps::Rock => 1,
            Rps::Paper => 2,
            Rps::Scissors => 3,
        }
    }

    const fn outcome(&self, other: &Self) -> RpsOutcome {
        use Rps::{Paper, Rock, Scissors};
        use RpsOutcome::{Draw, Loss, Win};

        match (self, other) {
            (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => Draw,
            (Paper, Scissors) | (Scissors, Rock) | (Rock, Paper) => Loss,
            (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => Win,
        }
    }
}

enum RpsOutcome {
    Win,
    Loss,
    Draw,
}

impl RpsOutcome {
    const fn points(&self) -> u32 {
        match self {
            RpsOutcome::Win => 6,
            RpsOutcome::Loss => 0,
            RpsOutcome::Draw => 3,
        }
    }
}

struct Round {
    opponent: Rps,
    you: Rps,
}

impl Round {
    const fn parse(s: &[u8]) -> Self {
        let (opponent, you) = split_once(s, b" ").unwrap();

        Round {
            opponent: match opponent {
                b"A" => Rps::Rock,
                b"B" => Rps::Paper,
                b"C" => Rps::Scissors,
                _ => panic!(),
            },
            you: match you {
                b"X" => Rps::Rock,
                b"Y" => Rps::Paper,
                b"Z" => Rps::Scissors,
                _ => panic!(),
            },
        }
    }

    const fn score(self) -> u32 {
        self.you.points() + self.you.outcome(&self.opponent).points()
    }
}

struct IncompleteRound {
    opponent: Rps,
    desired_outcome: RpsOutcome,
}

impl IncompleteRound {
    const fn parse(s: &[u8]) -> Self {
        let (opponent, you) = split_once(s, b" ").unwrap();

        IncompleteRound {
            opponent: match opponent {
                b"A" => Rps::Rock,
                b"B" => Rps::Paper,
                b"C" => Rps::Scissors,
                _ => panic!(),
            },
            desired_outcome: match you {
                b"X" => RpsOutcome::Loss,
                b"Y" => RpsOutcome::Draw,
                b"Z" => RpsOutcome::Win,
                _ => panic!(),
            },
        }
    }

    const fn into_round(self) -> Round {
        use Rps::{Paper, Rock, Scissors};
        use RpsOutcome::{Draw, Loss, Win};

        let you = match (&self.opponent, self.desired_outcome) {
            (Rock, Loss) | (Paper, Win) | (Scissors, Draw) => Scissors,
            (Rock, Draw) | (Paper, Loss) | (Scissors, Win) => Rock,
            (Paper, Draw) | (Rock, Win) | (Scissors, Loss) => Paper,
        };

        Round {
            opponent: self.opponent,
            you,
        }
    }
}
