use alloc::{borrow::ToOwned, string::String};
use core::str::FromStr;

use cfg_proc::apply;

use crate::{const_helpers::utf8, day, Day};

#[apply(day)]
impl Day<2022, 2> {
    pub fn parse(input: &[u8]) -> u32 {
        utf8(input)
            .trim()
            .lines()
            .map(|s| s.parse::<Round>().unwrap())
            .fold(0, |acc, curr| acc + curr.score())
    }

    pub fn parse2(input: &[u8]) -> u32 {
        utf8(input)
            .trim()
            .lines()
            .map::<Round, _>(|s| s.parse::<IncompleteRound>().unwrap().into())
            .fold(0, |acc, curr| acc + curr.score())
    }
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
    const fn score(self) -> u32 {
        self.you.points() + self.you.outcome(&self.opponent).points()
    }
}

impl FromStr for Round {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let [opponent, you] = s
            .split(' ')
            .next_chunk()
            .map_err(Iterator::collect::<String>)?;

        Ok(Round {
            opponent: match opponent {
                "A" => Rps::Rock,
                "B" => Rps::Paper,
                "C" => Rps::Scissors,
                c => return Err(c.to_owned()),
            },
            you: match you {
                "X" => Rps::Rock,
                "Y" => Rps::Paper,
                "Z" => Rps::Scissors,
                c => return Err(c.to_owned()),
            },
        })
    }
}

struct IncompleteRound {
    opponent: Rps,
    desired_outcome: RpsOutcome,
}

impl From<IncompleteRound> for Round {
    fn from(value: IncompleteRound) -> Self {
        use Rps::{Paper, Rock, Scissors};
        use RpsOutcome::{Draw, Loss, Win};

        let you = match (&value.opponent, value.desired_outcome) {
            (Rock, Loss) | (Paper, Win) | (Scissors, Draw) => Scissors,
            (Rock, Draw) | (Paper, Loss) | (Scissors, Win) => Rock,
            (Paper, Draw) | (Rock, Win) | (Scissors, Loss) => Paper,
        };

        Round {
            opponent: value.opponent,
            you,
        }
    }
}

impl FromStr for IncompleteRound {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let [opponent, you] = s
            .split(' ')
            .next_chunk()
            .map_err(Iterator::collect::<String>)?;

        Ok(IncompleteRound {
            opponent: match opponent {
                "A" => Rps::Rock,
                "B" => Rps::Paper,
                "C" => Rps::Scissors,
                c => return Err(c.to_owned()),
            },
            desired_outcome: match you {
                "X" => RpsOutcome::Loss,
                "Y" => RpsOutcome::Draw,
                "Z" => RpsOutcome::Win,
                c => return Err(c.to_owned()),
            },
        })
    }
}
