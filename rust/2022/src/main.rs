#![feature(iter_next_chunk, array_windows, iter_intersperse, iter_repeat_n)]
#![warn(clippy::pedantic)]

mod day_1;
mod day_2;
mod day_4;
mod day_5;
mod day_6;
mod day_7;
mod day_8;
mod day_9;

fn main() {
    print_solutions! {
        [1-1, 1-2]
        [2-1, 2-2]
        [4-1, 4-2]
        [5-1, 5-2]
        [6-1, 6-2]
        [7-1, 7-2]
        [8-1, 8-2]
        [9-1]
    }
}

#[macro_export]
macro_rules! print_solutions {
    ($([$day:literal-1 $(, $part_2_day:literal-2)?])*) => {
        $(
            paste::paste! {
                {
                    $(
                        const _: [(); $day] = [(); $part_2_day];
                    )?

                    use [< day_ $day >]::*;

                    let input = std::fs::read_to_string(
                        std::path::PathBuf::from_iter([
                            "..",
                            "..",
                            "inputs",
                            "2022",
                            &format!("day-{}.txt", $day)
                        ])
                    ).unwrap();

                    println!(
                        "{}-1 solution: {}",
                        $day,
                        solution(&input)
                    );

                    $(
                        println!(
                            "{}-2 solution: {}",
                            $part_2_day,
                            solution_part_2(&input)
                        );
                    )?
                }
            }
        )*
    }
}
