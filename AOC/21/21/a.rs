// Gavin Gray AOC 2021

#![feature(stdin_forwarders)]
use std::cmp;
use std::io;

static TO_WIN: u64 = 1000;

fn md(n: u64, p: u64) -> u64 {
    ((n - 1) % p) + 1
}

fn play(pos: &mut Vec<u64>, scores: &mut Vec<u64>) -> u64 {
    let mut rb: u64 = 1;
    let mut round: usize = 0;
    while scores[0] < TO_WIN && scores[1] < TO_WIN {
        let roll = rb + md(rb + 1, 100) + md(rb + 2, 100);
        pos[round % 2] = md(pos[round % 2] + roll, 10);
        scores[round % 2] += pos[round % 2];
        rb = md(rb + 3, 100);
        round += 1;
    }
    cmp::min(scores[0], scores[1]) * 3 * (round as u64)
}

fn main() {
    let mut starts: Vec<u64> = io::stdin()
        .lines()
        .map(|l| l.unwrap())
        .map(|s| s.split_whitespace().last().unwrap().parse::<u64>().unwrap())
        .collect();
    let mut scores = vec![0, 0];
    println!("{}", play(&mut starts, &mut scores));
}
