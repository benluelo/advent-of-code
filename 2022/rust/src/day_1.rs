pub fn solution(input: String) -> u32 {
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
        .into_iter()
        .max()
        .expect("input contained no numbers")
}
