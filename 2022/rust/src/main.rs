#![feature(iter_next_chunk)]

mod day_1;
mod day_2;

macro_rules! print_solutions {
    ($([$day:literal-1 $(, $part_2_day:literal-2)?])*) => {
        $(
            paste::paste! {
                {
                    use [< day_ $day >]::*;

                    let input = std::fs::read_to_string(
                        std::path::PathBuf::from_iter([
                            "..",
                            "inputs",
                            &format!("day-{}.txt", $day)
                        ])
                    ).unwrap();

                    $(
                        const _: [(); $day] = [(); $part_2_day];
                    )?

                    println!(
                        "{}-1 solution: {}",
                        $day,
                        solution(input.clone())
                    );

                    $(
                        println!(
                            "{}-2 solution: {}",
                            $part_2_day,
                            solution_part_2(input)
                        );
                    )?
                }
            }
        )*
    }
}

fn main() {
    print_solutions! {
        [1-1, 1-2]
        [2-1]
    }
}
