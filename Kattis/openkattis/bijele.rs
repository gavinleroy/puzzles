#[allow(unused_imports)]
use std::cmp::{min,max};
use std::io::{BufWriter, stdin, stdout, Write};
const BITS: usize = 19;
 
#[derive(Default)]
struct Scanner {
    buffer: Vec<String>
}
impl Scanner {
    fn next<T: std::str::FromStr>(&mut self) -> T {
        loop {
            if let Some(token) = self.buffer.pop() {
                return token.parse().ok().expect("Failed parse");
            }
            let mut input = String::new();
            stdin().read_line(&mut input).expect("Failed read");
            self.buffer = input.split_whitespace().rev().map(String::from).collect();
        }
    }
}
 
fn main() {
    let mut scan = Scanner::default();
    let out = &mut BufWriter::new(stdout());

    const GG: [i32; 6] = [ 1, 1, 2, 2, 2, 8 ];
    let mut a: Vec<i32> = (0..6).map(|i| GG[i]-scan.next::<i32>()).collect();
    println!("{} {} {} {} {} {}", a[0], a[1], a[2], a[3], a[4], a[5]);
}
