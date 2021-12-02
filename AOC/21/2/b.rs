// Gavin Gray AOC 2021

#![feature(stdin_forwarders)]
use std::io;

fn main() {
    let f = |(a, b, _)| a * b;
    println!(
        "{}",
        f(io::stdin()
            .lines()
            .map(|l| {
                l.unwrap()
                    .split_whitespace()
                    .map(String::from)
                    .collect::<Vec<String>>()
            })
            .fold((0, 0, 0), |(h, d, a), l: Vec<String>| match l.as_slice() {
                [cmd, units] => {
                    let v = str::parse::<i64>(&units).unwrap();
                    match cmd.as_ref() {
                        "forward" => (h + v, d + a * v, a),
                        "down" => (h, d, a + v),
                        "up" => (h, d, a - v),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }))
    );
}
