// Gavin Gray AOC 2021

#![feature(stdin_forwarders)]
use std::io;

fn main() {
    let f = |(a, b)| a * b;
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
            .fold((0, 0), |(h, d), l: Vec<String>| match l.as_slice() {
                [cmd, units] => {
                    let v = str::parse::<i64>(&units).unwrap();
                    match cmd.as_ref() {
                        "forward" => (h + v, d),
                        "down" => (h, d + v),
                        "up" => (h, d - v),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }))
    );
}
