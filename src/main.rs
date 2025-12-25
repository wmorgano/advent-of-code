use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

fn main() {
    println!("Day 1: Secret Entrance - Password = {}", get_password());
    println!("Day 2: Invalid IDs - Ranges = {}", get_invalid_ids());
    println!("Day 3: (Part 1) Total Joltage = {}", get_total_joltage(2));
    println!("Day 3: (Part 2) Total Joltage = {}", get_total_joltage(12));
    println!(
        "Day 4: (Part 1) Total removable rolls with one pass = {}",
        get_removable_rolls_first_pass()
    );
    println!(
        "Day 4: (Part 2) Total rolls removed = {}",
        get_total_rolls_removed()
    );
    println!("Day 5: (Part 1) Valid IDs - Count = {}", count_valid_ids());
    println!(
        "Day 5: (Part 2) Total available IDs = {}",
        count_all_intervals()
    );
}

fn read_lines(fname: &str) -> Lines<BufReader<File>> {
    let file_handle = File::open(fname).unwrap();
    BufReader::new(file_handle).lines()
}

fn load_rotations() -> Lines<BufReader<File>> {
    read_lines("day1_input.txt")
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

// Day 3

fn load_banks() -> Lines<BufReader<File>> {
    read_lines("day3_input.txt")
}

fn get_total_joltage(digits: usize) -> i64 {
    load_banks()
        .map(|bank_result| get_max_joltage(bank_result, digits))
        .sum()
}

fn get_max_joltage(bank_result: Result<String, std::io::Error>, digits: usize) -> i64 {
    let line: Vec<char> = bank_result.unwrap().chars().collect();

    let mut num_digits = digits;
    let mut begin_search_range: usize = 0;

    let mut joltage = String::new();

    while num_digits > 0 {
        let slice = &line[begin_search_range..=line.len() - num_digits];
        let mut max = '0';
        let mut position = 0;
        for (i, c) in slice.iter().enumerate() {
            if *c > max {
                max = *c;
                position = i;
            }
        }

        begin_search_range += position + 1;
        num_digits -= 1;

        joltage.push(max);
    }

    joltage.trim().parse().unwrap()
}

fn load_rolls() -> Vec<Vec<char>> {
    let lines = read_lines("day4_input.txt");

    let mut rolls = Vec::new();

    for line in lines {
        let chars = line.unwrap().chars().collect();
        rolls.push(chars)
    }

    rolls
}

struct Position(usize, usize);

struct Grid {
    rolls: Vec<Vec<char>>,
    row_max: usize,
    col_max: usize,
}

impl Grid {
    fn new(data: Vec<Vec<char>>) -> Self {
        let row_max = data.len() - 1;
        let col_max = data[0].len() - 1;

        Grid {
            rolls: data,
            row_max,
            col_max,
        }
    }

    fn is_valid_position(&self, position: &Position) -> bool {
        let Position(row, column) = *position;

        row <= self.row_max && column <= self.col_max
    }

    fn get_adjacent_position_status(&self, position: Position) -> u8 {
        let Position(row, column) = position;

        if !self.is_valid_position(&position) {
            return 0;
        }

        match self.rolls[row][column] {
            '@' => 1,
            _ => 0,
        }
    }

    fn get_adjacent_positions(&self, current: &Position) -> Vec<Position> {
        let Position(row, col) = *current;

        #[rustfmt::skip]
        let offsets = [
            (-1, -1), (-1, 0), (-1, 1),
            ( 0, -1),          ( 0, 1),
            ( 1, -1), ( 1, 0), ( 1, 1),
        ];

        offsets
            .iter()
            .filter_map(|&(row_delta, column_delta)| {
                let new_row = row.checked_add_signed(row_delta)?;
                let new_col = col.checked_add_signed(column_delta)?;

                if new_row <= self.row_max && new_col <= self.col_max {
                    Some(Position(new_row, new_col))
                } else {
                    None
                }
            })
            .collect()
    }

    fn count_adjacent_rolls(&self, current: &Position) -> u8 {
        self.get_adjacent_positions(current)
            .into_iter()
            .map(|pos| self.get_adjacent_position_status(pos))
            .sum()
    }

    fn is_removable(&self, current: &Position) -> bool {
        let Position(row, column) = *current;
        let spot = self.rolls[row][column];

        spot == '@' && self.count_adjacent_rolls(current) < 4
    }

    fn try_remove(&mut self, current: &Position, remove: bool) -> i32 {
        let Position(row, column) = *current;

        if self.is_removable(current) {
            if remove {
                self.rolls[row][column] = 'x';
            }
            1
        } else {
            0
        }
    }

    fn process_row(&mut self, row: usize, remove: bool) -> i32 {
        let mut removed: i32 = 0;

        for column in 0..=self.col_max {
            let position = Position(row, column);
            removed += self.try_remove(&position, remove);
        }

        removed
    }

    fn process_grid(&mut self) -> i32 {
        let mut removed = 0;

        for row in 0..=self.row_max {
            removed += self.process_row(row, false);
        }

        removed
    }

    fn process_grid_full(&mut self) -> i32 {
        let mut total_removed = 0;

        loop {
            let mut removed = 0;

            for row in 0..=self.row_max {
                removed += self.process_row(row, true);
            }

            if removed == 0 {
                break;
            } else {
                total_removed += removed;
            }
        }

        total_removed
    }
}

fn get_removable_rolls_first_pass() -> i32 {
    let mut grid = Grid::new(load_rolls());
    grid.process_grid()
}

fn get_total_rolls_removed() -> i32 {
    let mut grid = Grid::new(load_rolls());
    grid.process_grid_full()
}

// Day 5
// Part 1

#[derive(Copy, Clone)]
struct Interval {
    min: i64,
    max: i64,
}

impl Interval {
    fn new(min: i64, max: i64) -> Self {
        Interval { min, max }
    }

    fn contains(&self, value: i64) -> bool {
        self.min <= value && value <= self.max
    }

    fn size(&self) -> i64 {
        self.max - self.min + 1
    }

    fn merge(a: &Interval, b: &Interval) -> Interval {
        Interval::new(std::cmp::min(a.min, b.min), std::cmp::max(a.max, b.max))
    }

    fn is_overlap(&self, other: &Interval) -> bool {
        self.max >= other.min - 1
    }
}

fn load_id_ranges() -> Vec<Interval> {
    let buffer = read_lines("day5_input_ranges.txt");

    let mut ranges = Vec::new();

    for line in buffer {
        let content = line.unwrap();
        let split = content.split_once('-').unwrap();
        ranges.push(Interval::new(
            split.0.parse().unwrap(),
            split.1.parse().unwrap(),
        ));
    }

    ranges
}

fn load_id_values() -> Vec<i64> {
    let buffer = read_lines("day5_input_values.txt");

    buffer.map(|line| line.unwrap().parse().unwrap()).collect()
}

fn is_valid_id(id: i64) -> bool {
    let ranges = load_id_ranges();
    ranges.iter().any(|range| range.contains(id))
}

fn count_valid_ids() -> usize {
    let values = load_id_values();
    values.iter().filter(|&&id| is_valid_id(id)).count()
}

fn sort_ranges(ranges: &mut Vec<Interval>) {
    ranges.sort_by_key(|range| range.min);
}

fn merge_ranges(ranges: &mut Vec<Interval>) {
    if ranges.is_empty() {
        return;
    }

    sort_ranges(ranges);

    let mut merged = Vec::new();
    merged.push(ranges[0]);

    let merged = ranges.iter().skip(1).fold(merged, |mut merged, range| {
        if merged.last().unwrap().is_overlap(range) {
            let last = merged.pop().unwrap();
            merged.push(Interval::merge(&last, range));
        } else {
            merged.push(*range);
        }
        merged
    });

    *ranges = merged;
}

fn count_all_intervals() -> i64 {
    let mut ranges = load_id_ranges();
    merge_ranges(&mut ranges);
    ranges.iter().map(|range| range.size()).sum()
}
