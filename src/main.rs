use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

fn load_rotations() -> Lines<BufReader<File>> {
    let file_handle = File::open("day1_input.txt").unwrap();
    let buffer = BufReader::new(file_handle);
    buffer.lines()
}

fn get_direction(rotation: &str) -> i32 {
    let rotation = rotation.chars().nth(0).unwrap();
    match rotation {
        'L' => -1,
        'R' => 1,
        _ => 0,
    }
}

fn get_magnitude(rotation: &str) -> i32 {
    rotation[1..].parse().unwrap()
}

fn spin_dial(position: i32, direction: i32, magnitude: i32, slots: i32) -> i32 {
    (position + (direction * magnitude)) % slots
}

fn get_password() -> i32 {
    let mut password = 0;
    let mut position = 50;
    let rotations = load_rotations();

    for rotation in rotations {
        match rotation {
            Ok(rotation) => {
                let direction = get_direction(&rotation);
                let magnitude = get_magnitude(&rotation);

                position = spin_dial(position, direction, magnitude, 100);

                if position == 0 {
                    password += 1;
                }
            }
            Err(err) => {
                eprintln!("Error parsing rotation: {}", err);
            }
        }
    }

    password
}

// Day 2

fn read_ranges_from_file() -> String {
    std::fs::read_to_string("day2_input.txt")
        .unwrap()
        .trim()
        .to_owned()
}

fn parse_range(string_range: &str) -> (i64, i64) {
    let (min, max) = string_range.split_once('-').unwrap();
    (min.parse().unwrap(), max.parse().unwrap())
}

fn check_id(id: &i64) -> bool {
    let string_repr: String = id.to_string();
    let half = string_repr.len() / 2;
    let (front, back) = string_repr.split_at(half);

    front == back
}

fn check_range(range: (i64, i64)) -> i64 {
    let (min, max) = range;
    (min..=max).filter(check_id).sum()
}

fn parse_ranges(string_ranges: &str) -> i64 {
    string_ranges
        .split(',')
        .map(parse_range)
        .map(check_range)
        .sum()
}

fn get_invalid_ids() -> i64 {
    let file_input = read_ranges_from_file();
    parse_ranges(&file_input)
}

fn main() {
    println!("Day 1: Secret Entrance - Password = {}", get_password());
    println!("Day 2: Invalid IDs - Ranges = {}", get_invalid_ids());
}
