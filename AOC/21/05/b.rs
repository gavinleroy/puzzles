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
        let (x1, y1, x2, y2) = {
            if x2 < x1 || (y2 < y1 && x1 == x2) {
                (x2, y2, x1, y1)
            } else {
                (x1, y1, x2, y2)
            }
        };
        Line {
            fx: x1,
            fy: y1,
            tx: x2,
            ty: y2,
        }
    }

    fn is_vertical(self) -> bool {
        self.fx == self.tx
    }

    fn is_horizontal(self) -> bool {
        self.fy == self.ty
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
        // HACK: this is bad but I'm tired
        let (mut x, mut y) = (l.fx, l.fy);
        let rev = l.fy > l.ty;
        while x != l.tx && y != l.ty {
            b[y][x] += 1;
            x += 1;
            if rev {
                y -= 1;
            } else {
                y += 1;
            }
        }
        b[y][x] += 1;
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
            .fold(vec![vec![0; MAX]; MAX], add_to_board)
            .into_iter()
            .flatten()
            .filter(|i| *i > 1)
            .count()
    );
}
