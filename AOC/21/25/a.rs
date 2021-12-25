// Gavin Gray AOC 2021

// TODO FIXME I could be faster

#![feature(stdin_forwarders)]
use std::io;

type Floor = Vec<Vec<char>>;

#[allow(non_snake_case)]
fn move_sea_cucumber(mut floor: Floor) -> u64 {
    let (H, W) = (floor.len(), floor[0].len());
    let mut count = 0;
    let mut south = true;
    let mut east = true;

    while south || east {
        let mut next = floor.clone();

        if count % 2 == 0 {
            east = false;
            for h in 0..H {
                for wj in 0..W {
                    let w = ((wj + 1) % W) as usize;
                    if floor[h][wj] == '>' && floor[h][w] == '.' {
                        east = true;
                        next[h][wj] = '.';
                        next[h][w] = '>';
                    }
                }
            }
        } else {
            south = false;
            for hi in 0..H {
                for w in 0..W {
                    let h = ((hi + 1) % H) as usize;
                    if floor[hi][w] == 'v' && floor[h][w] == '.' {
                        south = true;
                        next[h][w] = 'v';
                        next[hi][w] = '.';
                    }
                }
            }
        }

        floor = next;
        count += 1;
    }
    count / 2 + 1
}

fn main() {
    let floor: Floor = io::stdin()
        .lines()
        .map(|l| l.unwrap().chars().collect())
        .collect();

    println!("{}", move_sea_cucumber(floor));
}
