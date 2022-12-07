pub fn solution(input: String) -> usize {
    const ARRAY_SIZE: usize = 4;

    input
        .trim()
        .as_bytes()
        .array_windows()
        .zip(ARRAY_SIZE..)
        .find_map(|([a, b, c, d], i)| {
            (a != b && a != c && a != d && b != c && b != d && c != d).then_some(i)
        })
        .unwrap()
}
