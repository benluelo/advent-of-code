pub fn solution(input: String) -> u32 {
    parse_input(input)
        .into_iter()
        .max()
        .expect("input contained no numbers")
}

pub fn solution_part_2(input: String) -> u32 {
    let mut parsed_input = parse_input(input);

    parsed_input.sort_by(|a, b| b.cmp(a));

    parsed_input[0..3].iter().sum()
}

fn parse_input(input: String) -> Vec<u32> {
    input
        .split("\n\n")
        .map(|s| s.parse::<u32>().ok())
        .fold(vec![], |acc: Vec<u32>, curr| match (&*acc, curr) {
            ([], Some(curr)) => vec![curr],
            ([], None) => acc,
            ([finished @ .., acc], Some(curr)) => finished
                .iter()
                .copied()
                .chain(std::iter::once(acc + curr))
                .collect(),
            (accumulated, None) => accumulated
                .iter()
                .copied()
                .chain(std::iter::once(0))
                .collect(),
        })
}
