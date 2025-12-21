use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

fn read_lines_from_file<P: AsRef<Path>>(filename: P) -> io::Result<Vec<String>> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    reader.lines().collect()
}

fn load_rotations() -> Vec<String> {
    read_lines_from_file("day1_input.txt").unwrap()
}

fn get_direction(rotation: &str) -> i32 {
    let rotation = rotation.chars().next().unwrap();
    match rotation {
        'L' => -1,
        'R' => 1,
        _ => 0,
    }
}

fn get_magnitude(rotation: &str) -> i32 {
    let (index, _) = rotation.char_indices().nth(1).unwrap();
    let rotation: i32 = rotation[index..].to_string().parse().unwrap();
    rotation
}

fn spin_dial(position: i32, direction: i32, magnitude: i32, slots: i32) -> i32 {
    (position + (direction * magnitude)) % slots
}

fn get_password() -> i32 {
    let mut password = 0;
    let mut position = 50;
    let rotations = load_rotations();

    for rotation in rotations.iter() {
        let direction = get_direction(rotation);
        let magnitude = get_magnitude(rotation);

        position = spin_dial(position, direction, magnitude, 100);

        if position == 0 {
            password += 1;
        }
    }

    password
}

// Day 2

fn read_ranges_from_file<P: AsRef<Path>>(filename: P) -> String {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);

    // Only need to read the single line at the top of the file
    reader.lines().flatten().next().unwrap()
}

fn read_csv_line(input: &str) -> Vec<&str> {
    input.split(',').collect()
}

fn parse_range(string_range: &str) -> (i64, i64) {
    let (min, max) = string_range.split_once('-').unwrap();
    (min.parse().unwrap(), max.parse().unwrap())
}

fn parse_ranges(string_ranges: Vec<&str>) -> Vec<(i64, i64)> {
    string_ranges.into_iter().map(parse_range).collect()
}

fn check_id(id: i64) -> bool {
    let string_repr: String = id.to_string();
    let half = string_repr.len() / 2;
    let (front, back) = string_repr.split_at(half);

    front == back
}

fn check_range(range: (i64, i64)) -> i64 {
    let (min, max) = range;
    (min..=max).filter(|&id| check_id(id)).sum()
}

fn check_ranges(ranges: Vec<(i64, i64)>) -> i64 {
    ranges.into_iter().map(check_range).sum()
}

fn get_invalid_ids() -> i64 {
    let file_input = read_ranges_from_file("day2_input.txt");
    let ranges = parse_ranges(read_csv_line(&file_input));
    check_ranges(ranges)
}

fn main() {
    println!("Day 1: Secret Entrance - Password = {}", get_password());
    println!("Day 2: Invalid IDs - Ranges = {}", get_invalid_ids());
}
