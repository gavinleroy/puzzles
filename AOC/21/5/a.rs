// Gavin Gray AOC 21

#![feature(stdin_forwarders)]
use std::io;

static MAX: usize = 1000;

#[derive(Copy, Clone)]
struct Line {
    fx: usize,
    fy: usize,
    tx: usize,
    ty: usize,
}

impl Line {
    fn parse(s: String) -> Self {
        let (f, t) = s.split_once(" -> ").unwrap();
        let ((fx, fy), (tx, ty)) = (f.split_once(",").unwrap(), t.split_once(",").unwrap());
        let (x1, y1, x2, y2) = (
            fx.parse::<usize>().unwrap(),
            fy.parse::<usize>().unwrap(),
            tx.parse::<usize>().unwrap(),
            ty.parse::<usize>().unwrap(),
        );
        Line {
            fx: x1.min(x2),
            fy: y1.min(y2),
            tx: x1.max(x2),
            ty: y1.max(y2),
        }
    }

    fn is_vertical(self) -> bool {
        self.fx == self.tx
    }

    fn is_horizontal(self) -> bool {
        self.fy == self.ty
    }

    fn not_diagonal<'r>(&'r self) -> bool {
        self.is_vertical() || self.is_horizontal()
    }
}

fn add_to_board(mut b: Vec<Vec<usize>>, l: Line) -> Vec<Vec<usize>> {
    if l.is_vertical() {
        for y in l.fy..=l.ty {
            b[y][l.fx] += 1;
        }
    } else if l.is_horizontal() {
        for x in l.fx..=l.tx {
            b[l.fy][x] += 1;
        }
    } else {
        unreachable!();
    }
    b
}

fn main() {
    println!(
        "{}",
        io::stdin()
            .lines()
            .map(|v| v.unwrap())
            .map(Line::parse)
            .filter(Line::not_diagonal)
            .fold(vec![vec![0; MAX]; MAX], add_to_board)
            .into_iter()
            .flatten()
            .filter(|i| *i > 1)
            .count()
    );
}
