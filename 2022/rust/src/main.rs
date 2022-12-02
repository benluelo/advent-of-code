#![feature(concat_idents)]

mod day_1;

macro_rules! print_solutions {
    ($([$day:ident $(, $part_2:ident)?]),*) => {
        $(
            {
                use $day::*;

                let input = std::fs::read_to_string(
                    std::path::PathBuf::from_iter([
                        "..",
                        "inputs",
                        &stringify!($day).replace('_', "-")
                    ])
                ).unwrap();

                let day_number = &stringify!($day)
                    .split("-")
                    .nth(2)
                    .unwrap();

                println!(
                    "{}-1 solution: {}",
                    day_number,
                    solution(input.clone())
                );

                $(
                    println!(
                        "{}-2 solution: {}",
                        day_number,
                        concat_idents!(solution, _, $part_2) (input)
                    );
                )?
            }
        )*
    }
}

fn main() {
    print_solutions! {
        [day_1]
    }
}
