// Gavin Gray AOC 2021

#![feature(stdin_forwarders)]
use std::convert::TryInto;
use std::io;

fn neighbors(row: i32, col: i32, ml: i32) -> Vec<(i32, i32)> {
    vec![
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1),
    ]
    .into_iter()
    .filter(|&(r, c)| r >= 0 && c >= 0 && r < ml && c < ml)
    .collect()
}

// NOTE I really dislike the side mutation that is happening here.
fn step(board: &mut Vec<Vec<i32>>, max_len: i32) -> bool {
    let add1 = |num: &mut i32| {
        *num += 1;
        *num == max_len
    };
    let mut flashed = Vec::<(i32, i32)>::default();
    let mut num_flashed: i32 = 0;
    board.iter_mut().enumerate().for_each(|(row, l)| {
        l.iter_mut().enumerate().for_each(|(col, v)| {
            if add1(v) {
                flashed.push((row.try_into().unwrap(), col.try_into().unwrap()));
                num_flashed += 1;
            }
        })
    });
    while let Some((row, col)) = flashed.pop() {
        neighbors(row, col, max_len).iter().for_each(|&(r, c)| {
            if add1(&mut board[r as usize][c as usize]) {
                num_flashed += 1;
                flashed.push((r, c));
            }
        })
    }
    board.iter_mut().for_each(|l| {
        l.iter_mut().for_each(|v| {
            if *v >= 10 {
                *v = 0;
            }
        })
    });
    num_flashed != max_len * max_len
}

fn main() {
    let mut init_board: Vec<Vec<i32>> = io::stdin()
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .flat_map(|c| c.to_digit(10).unwrap().try_into())
                .collect()
        })
        .collect();
    println!(
        "{}",
        // not good: *could* potentially loop for a long time
        (0..).take_while(|_| step(&mut init_board, 10)).count() + 1
    );
}
