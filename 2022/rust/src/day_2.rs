use std::str::FromStr;

enum Rps {
    Rock,
    Paper,
    Scissors,
}

impl Rps {
    fn points(&self) -> u32 {
        match self {
            Rps::Rock => 1,
            Rps::Paper => 2,
            Rps::Scissors => 3,
        }
    }

    fn outcome(&self, other: &Self) -> RpsOutcome {
        use Rps::*;
        use RpsOutcome::*;

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
    fn points(&self) -> u32 {
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
    fn score(self) -> u32 {
        self.you.points() + self.you.outcome(&self.opponent).points()
    }
}

impl FromStr for Round {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let [opponent, you] = s
            .split(' ')
            .next_chunk()
            .map_err(|iter| iter.collect::<String>())?;

        Ok(Round {
            opponent: match opponent {
                "A" => Rps::Rock,
                "B" => Rps::Paper,
                "C" => Rps::Scissors,
                c => return Err(c.to_string()),
            },
            you: match you {
                "X" => Rps::Rock,
                "Y" => Rps::Paper,
                "Z" => Rps::Scissors,
                c => return Err(c.to_string()),
            },
        })
    }
}

pub fn solution(input: String) -> u32 {
    input
        .trim()
        .lines()
        .map(|s| s.parse::<Round>().unwrap())
        .fold(0, |acc, curr| acc + curr.score())
}
