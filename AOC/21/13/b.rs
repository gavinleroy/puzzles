// Gavin Gray AOC 2021

#![feature(btree_drain_filter)]
#![feature(stdin_forwarders)]
use std::collections::BTreeSet;
use std::io;

type Posn = (usize, usize);

enum Fold {
    X(usize),
    Y(usize),
}

impl Fold {
    pub fn parse(s: &String) -> Option<Self> {
        let (t, v) = s
            .split_whitespace()
            .last()
            .unwrap()
            .split_once("=")
            .unwrap();
        let iv = v.parse::<usize>().unwrap();
        match t {
            "x" => Some(Fold::X(iv)),
            "y" => Some(Fold::Y(iv)),
            _ => unreachable!(),
        }
    }
}

fn iinsert(paper: &mut BTreeSet<Posn>, p: Posn) {
    let _ = paper.insert(p);
}

fn parse_posn(s: &String) -> Option<Posn> {
    let (xs, ys) = s.split_once(",")?;
    Some((xs.parse::<usize>().unwrap(), ys.parse::<usize>().unwrap()))
}

fn fold(mut paper: &mut BTreeSet<Posn>, fold: &Fold) {
    let under: BTreeSet<Posn>;
    match fold {
        Fold::X(l) => {
            under = paper
                .drain_filter(|(x, _)| x > l)
                .map(|(x, y)| (l - (x - l), y))
                .collect();
        }
        Fold::Y(l) => {
            under = paper
                .drain_filter(|(_, y)| y > l)
                .map(|(x, y)| (x, l - (y - l)))
                .collect();
        }
    }
    under.iter().for_each(|&p| iinsert(&mut paper, p));
}

fn main() {
    let (sdots, sfolds): (Vec<String>, Vec<String>) = io::stdin()
        .lines()
        .map(|l| l.unwrap())
        .partition(|l| l.len() < 14);
    let mut paper = BTreeSet::<Posn>::default();
    sdots
        .iter()
        .filter_map(parse_posn)
        .for_each(|p| iinsert(&mut paper, p));
    let folds: Vec<Fold> = sfolds.iter().flat_map(Fold::parse).collect();
    folds.iter().for_each(|f| fold(&mut paper, f));
    // NOTE make sure you have enough terminal space
    //      to see the answer :)
    print!("\x1bc"); // clearscreen
    paper
        .iter()
        .for_each(|(x, y)| print!("\x1b[{};{}H#", y + 2, x + 1));
    println!("\x1b[8;1H");
}
