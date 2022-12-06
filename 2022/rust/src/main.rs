#![feature(iter_next_chunk)]
#![cfg_attr(feature = "type-level-solutions", allow(incomplete_features))]
#![cfg_attr(feature = "type-level-solutions", feature(generic_const_exprs))]
#![cfg_attr(feature = "type-level-solutions", recursion_limit = "1000")]

mod day_1;
#[cfg(feature = "type-level-solutions")]
mod day_1_alternate;
mod day_2;
mod day_4;
mod day_5;

fn main() {
    print_solutions! {
        [1-1, 1-2]
        [2-1, 2-2]
        [4-1, 4-2]
        [5-1, 5-2]
    }
}

#[macro_export]
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
