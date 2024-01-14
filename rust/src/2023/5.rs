use cfg_proc::apply;

use crate::{
    const_helpers::{iter, min_u64, parse_int, read_until, split_with_len, strip_prefix},
    day, Day,
};

#[apply(day)]
impl Day<2023, 5> {
    pub const fn parse(input: &[u8]) -> u64 {
        parse(input)
    }
    // technically possible but i can't recommend doing this at compile time
    pub const fn parse2(input: &[u8]) -> u64 {
        parse2(input)
    }
}

#[test]
#[cfg(test)]
fn parse_test() {
    let input = b"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";

    dbg!(parse2(input));
    // dbg!(parse2(Day::<2023, 5>::INPUT.as_bytes()));
}

const DOUBLE_NEWLINE: &[u8; 2] = b"\n\n";

const fn parse(input: &[u8]) -> u64 {
    let mut res = 0;

    let mut idx = 0;
    let seeds_line = read_until(input, idx, DOUBLE_NEWLINE);
    idx += seeds_line.len() + DOUBLE_NEWLINE.len();

    let seed_to_soil_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += seed_to_soil_map.len() + DOUBLE_NEWLINE.len();

    let soil_to_fertilizer_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += soil_to_fertilizer_map.len() + DOUBLE_NEWLINE.len();

    let fertilizer_to_water_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += fertilizer_to_water_map.len() + DOUBLE_NEWLINE.len();

    let water_to_light_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += water_to_light_map.len() + DOUBLE_NEWLINE.len();

    let light_to_temperature_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += light_to_temperature_map.len() + DOUBLE_NEWLINE.len();

    let temperature_to_humidity_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += temperature_to_humidity_map.len() + DOUBLE_NEWLINE.len();

    let humidity_to_location_map = read_until(input, idx, DOUBLE_NEWLINE);

    let seeds = strip_prefix(seeds_line, b"seeds: ");
    let seed_to_soil_map = strip_prefix(seed_to_soil_map, b"seed-to-soil map:\n");
    let soil_to_fertilizer_map = strip_prefix(soil_to_fertilizer_map, b"soil-to-fertilizer map:\n");
    let fertilizer_to_water_map =
        strip_prefix(fertilizer_to_water_map, b"fertilizer-to-water map:\n");
    let water_to_light_map = strip_prefix(water_to_light_map, b"water-to-light map:\n");
    let light_to_temperature_map =
        strip_prefix(light_to_temperature_map, b"light-to-temperature map:\n");
    let temperature_to_humidity_map = strip_prefix(
        temperature_to_humidity_map,
        b"temperature-to-humidity map:\n",
    );
    let humidity_to_location_map =
        strip_prefix(humidity_to_location_map, b"humidity-to-location map:\n");

    #[apply(iter)]
    for num in split(seeds, b" ") {
        let soil_id = map_id::<false>(parse_int(num) as u64, seed_to_soil_map);
        // dbg!(soil_id);
        let fertilizer_id = map_id::<false>(soil_id, soil_to_fertilizer_map);
        // dbg!(fertilizer_id);
        let water_id = map_id::<false>(fertilizer_id, fertilizer_to_water_map);
        // dbg!(water_id);
        let light_id = map_id::<false>(water_id, water_to_light_map);
        // dbg!(light_id);
        let temperature_id = map_id::<false>(light_id, light_to_temperature_map);
        // dbg!(temperature_id);
        let humidity_id = map_id::<false>(temperature_id, temperature_to_humidity_map);
        // dbg!(humidity_id);
        let location_id = map_id::<false>(humidity_id, humidity_to_location_map);
        // dbg!(location_id);

        res = if res == 0 {
            location_id
        } else {
            min_u64(res, location_id)
        };
    }

    res
}

const fn parse2(input: &[u8]) -> u64 {
    let ParseMapsResult {
        seeds,
        seed_to_soil_map,
        soil_to_fertilizer_map,
        fertilizer_to_water_map,
        water_to_light_map,
        light_to_temperature_map,
        temperature_to_humidity_map,
        humidity_to_location_map,
    } = parse_maps(input);

    // let mut counter = 0;

    #[apply(iter)]
    for location_id in range(0, u64::MAX) {
        // counter += 1;
        // if counter == 100_000 {
        //     dbg!(counter);
        //     counter = 0;
        // };

        let humidity_id = map_id::<true>(location_id, humidity_to_location_map);
        let temperature_id = map_id::<true>(humidity_id, temperature_to_humidity_map);
        let light_id = map_id::<true>(temperature_id, light_to_temperature_map);
        let water_id = map_id::<true>(light_id, water_to_light_map);
        let fertilizer_id = map_id::<true>(water_id, fertilizer_to_water_map);
        let soil_id = map_id::<true>(fertilizer_id, soil_to_fertilizer_map);
        let found_seed_id = map_id::<true>(soil_id, seed_to_soil_map);

        let mut seed = None;

        #[apply(iter)]
        for num in split(seeds, b" ") {
            let (seed_id, range) = match seed {
                None => {
                    seed = Some(parse_int(num) as u64);
                    continue;
                }
                Some(id) => {
                    seed = None;
                    (id, parse_int(num))
                }
            };

            if found_seed_id >= seed_id && found_seed_id < (seed_id + range as u64) {
                return location_id;
            }
        }
    }

    panic!("no solution found");
}

struct ParseMapsResult<'a> {
    seeds: &'a [u8],
    seed_to_soil_map: &'a [u8],
    soil_to_fertilizer_map: &'a [u8],
    fertilizer_to_water_map: &'a [u8],
    water_to_light_map: &'a [u8],
    light_to_temperature_map: &'a [u8],
    temperature_to_humidity_map: &'a [u8],
    humidity_to_location_map: &'a [u8],
}

const fn parse_maps(input: &[u8]) -> ParseMapsResult {
    let mut idx = 0;
    let seeds_line = read_until(input, idx, DOUBLE_NEWLINE);
    idx += seeds_line.len() + DOUBLE_NEWLINE.len();

    let seed_to_soil_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += seed_to_soil_map.len() + DOUBLE_NEWLINE.len();

    let soil_to_fertilizer_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += soil_to_fertilizer_map.len() + DOUBLE_NEWLINE.len();

    let fertilizer_to_water_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += fertilizer_to_water_map.len() + DOUBLE_NEWLINE.len();

    let water_to_light_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += water_to_light_map.len() + DOUBLE_NEWLINE.len();

    let light_to_temperature_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += light_to_temperature_map.len() + DOUBLE_NEWLINE.len();

    let temperature_to_humidity_map = read_until(input, idx, DOUBLE_NEWLINE);
    idx += temperature_to_humidity_map.len() + DOUBLE_NEWLINE.len();

    let humidity_to_location_map = read_until(input, idx, DOUBLE_NEWLINE);

    let seeds = strip_prefix(seeds_line, b"seeds: ");
    let seed_to_soil_map = strip_prefix(seed_to_soil_map, b"seed-to-soil map:\n");
    let soil_to_fertilizer_map = strip_prefix(soil_to_fertilizer_map, b"soil-to-fertilizer map:\n");
    let fertilizer_to_water_map =
        strip_prefix(fertilizer_to_water_map, b"fertilizer-to-water map:\n");
    let water_to_light_map = strip_prefix(water_to_light_map, b"water-to-light map:\n");
    let light_to_temperature_map =
        strip_prefix(light_to_temperature_map, b"light-to-temperature map:\n");
    let temperature_to_humidity_map = strip_prefix(
        temperature_to_humidity_map,
        b"temperature-to-humidity map:\n",
    );
    let humidity_to_location_map =
        strip_prefix(humidity_to_location_map, b"humidity-to-location map:\n");

    ParseMapsResult {
        seeds,
        seed_to_soil_map,
        soil_to_fertilizer_map,
        fertilizer_to_water_map,
        water_to_light_map,
        light_to_temperature_map,
        temperature_to_humidity_map,
        humidity_to_location_map,
    }
}

const fn map_id<const REVERSE: bool>(id: u64, map: &[u8]) -> u64 {
    #[apply(iter)]
    for line in lines(map) {
        let [dst_range_start_bz, src_range_start_bz, range_len] =
            split_with_len::<3, b' ', false>(line);
        let dst_range_start = if REVERSE {
            parse_int(src_range_start_bz) as u64
        } else {
            parse_int(dst_range_start_bz) as u64
        };
        let src_range_start = if REVERSE {
            parse_int(dst_range_start_bz) as u64
        } else {
            parse_int(src_range_start_bz) as u64
        };
        let range_len = parse_int(range_len) as u64;

        if id >= src_range_start && id < src_range_start + range_len {
            return if src_range_start > dst_range_start {
                id - (src_range_start - dst_range_start)
            } else if dst_range_start > src_range_start {
                id + (dst_range_start - src_range_start)
            } else {
                panic!("source and destination ranges are the same?")
            };
        }
    }

    // not mapped, corresponds to the same dst id
    id
}
