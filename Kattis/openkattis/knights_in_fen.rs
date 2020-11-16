// Attribute IO code to AxlLind @ https://github.com/AxlLind/easy_io
// Testing code for a simple problem on Kattis :)

#![allow(dead_code)]
use std::io::{self, Read, Stdin, Write, Stdout, Result};
use std::fs::{File, OpenOptions};
use std::fmt::{Display};
use std::char;
use std::cmp::min;
const EOF: &str = "InputReader: Reached end of input!";
pub struct InputReader<R: Read> {
  reader: R,
  buf: Vec<u8>,
  bytes_read: usize,
  current_index: usize,
}
impl InputReader<Stdin> {
  pub fn new() -> Self {
    Self::from_reader(io::stdin())
  }
}
impl InputReader<File> {
  pub fn from_file(path: &str) -> Self {
    Self::from_reader(File::open(path).unwrap())
  }
}
impl<R: Read> InputReader<R> {
  pub fn from_reader(reader: R) -> Self {
    Self {
      reader,
      buf: vec![0; 1 << 16],
      bytes_read: 0,
      current_index: 0,
    }
  }
  pub fn next<T: InputReadable>(&mut self) -> T {
    T::from_input(self)
  }
  pub fn next_line(&mut self) -> String {
    assert!(self.has_more(), EOF);
    let mut line = String::new();
    while self.peek() != '\n' {
      line.push(self.peek());
      self.consume();
      if !self.has_more() { break; }
    }
    self.consume(); // consume '\n'
    line
  }
  pub fn has_more(&mut self) -> bool {
    if self.current_index >= self.bytes_read {
      self.bytes_read = self.reader.read(&mut self.buf[..]).unwrap();
      self.current_index = 0
    }
    self.bytes_read > 0
  }
  pub fn set_buf_size(&mut self, buf_size: usize) {
    self.buf.resize(buf_size, 0);
  }
  fn peek(&self) -> char { self.buf[self.current_index] as char }
  fn consume(&mut self) { self.current_index += 1; }
  fn pop_digit(&mut self) -> u64 {
    let c = self.peek();
    self.consume();
    c as u64 - '0' as u64
  }
  fn consume_until<F: Fn(char) -> bool>(&mut self, test: F) {
    loop {
      assert!(self.has_more(), EOF);
      if test(self.peek()) { return; }
      self.consume();
    }
  }
  fn consume_until_sign(&mut self) -> i64 {
    loop {
      self.consume_until(|c| c.is_ascii_digit() || c == '-');
      if self.peek() != '-' { return 1; }

      self.consume();
      assert!(self.has_more(), EOF);
      if self.peek().is_ascii_digit() { return -1; }
    }
  }
}
pub trait InputReadable {
  fn from_input<R: Read>(input: &mut InputReader<R>) -> Self;
}
impl InputReadable for u64 {
  fn from_input<R: Read>(input: &mut InputReader<R>) -> Self {
    input.consume_until(|c| c.is_ascii_digit());
    let mut num = 0;
    while input.peek().is_ascii_digit() {
      num = num * 10 + input.pop_digit();
      if !input.has_more() { break; }
    }
    num
  }
}
impl InputReadable for i64 {
  fn from_input<R: Read>(input: &mut InputReader<R>) -> Self {
    let sign = input.consume_until_sign();
    u64::from_input(input) as i64 * sign
  }
}
impl InputReadable for f64 {
  fn from_input<R: Read>(input: &mut InputReader<R>) -> Self {
    let sign = input.consume_until_sign() as f64;
    let mut num = 0.0;
    while input.peek().is_ascii_digit() {
      num = num * 10.0 + input.pop_digit() as f64;
      if !input.has_more() { break; }
    }

    let mut factor = 1.0;
    if input.peek() == '.' {
      input.consume();
      while input.has_more() && input.peek().is_ascii_digit() {
        num = num * 10.0 + input.pop_digit() as f64;
        factor *= 10.0;
      }
    }
    sign * num / factor
  }
}
impl InputReadable for String {
  fn from_input<R: Read>(input: &mut InputReader<R>) -> Self {
    input.consume_until(|c| c.is_ascii_graphic());
    let mut word = String::new();
    while input.peek().is_ascii_graphic() {
      word.push(input.peek());
      input.consume();
      if !input.has_more() { break; }
    }
    word
  }
}
impl InputReadable for char {
  fn from_input<R: Read>(input: &mut InputReader<R>) -> Self {
    input.consume_until(|c| c.is_ascii_graphic());
    let c = input.peek();
    input.consume();
    c
  }
}
macro_rules! impl_readable_from {
  ($A:ty, [$($T:ty),+]) => {
    $(impl InputReadable for $T {
      fn from_input<R: Read>(input: &mut InputReader<R>) -> Self {
        <$A>::from_input(input) as $T
      }
    })+
  };
}
impl_readable_from!{ u64, [u32, u16, u8, usize] }
impl_readable_from!{ i64, [i32, i16, i8, isize] }
impl_readable_from!{ f64, [f32] }
pub struct OutputWriter<W: Write> {
  writer: W,
  buf: Vec<u8>,
}
impl OutputWriter<Stdout> {
  pub fn new() -> Self { Self::from_writer(io::stdout()) }
}

impl OutputWriter<File> {
  pub fn from_file(path: &str) -> Self {
    let file = OpenOptions::new()
      .write(true)
      .create(true)
      .open(path);
    Self::from_writer(file.unwrap())
  }
}
impl<W: Write> OutputWriter<W> {
  pub fn from_writer(writer: W) -> Self {
    let buf = Vec::with_capacity(1 << 16);
    Self { writer, buf }
  }
  pub fn print<T: Display>(&mut self, t: T) {
    write!(self, "{}", t).unwrap();
  }
  pub fn prints<T: Display>(&mut self, t: T) {
    write!(self, "{} ", t).unwrap();
  }
  pub fn println<T: Display>(&mut self, t: T) {
    writeln!(self, "{}", t).unwrap();
  }
}
impl<W: Write> Write for OutputWriter<W> {
  fn write(&mut self, bytes: &[u8]) -> Result<usize> {
    self.buf.extend(bytes);
    Ok(bytes.len())
  }
  fn flush(&mut self) -> Result<()> {
    self.writer.write_all(&self.buf)?;
    self.writer.flush()?;
    self.buf.clear();
    Ok(())
  }
}
impl<W: Write> Drop for OutputWriter<W> {
  fn drop(&mut self) { self.flush().unwrap(); }
}
// ------------ DONE IO ------------

fn checkboard(b: &[[char; 5]; 5]) -> bool {
    let orig = [[ '1', '1', '1', '1', '1' ],
                [ '0', '1', '1', '1', '1' ],
                [ '0', '0', ' ', '1', '1' ],
                [ '0', '0', '0', '0', '1' ],
                [ '0', '0', '0', '0', '0' ]];
    for i in 0..5 {
        for j in 0..5 {
            if orig[i as usize][j as usize] != b[i as usize][j as usize] 
                { return false; }
        }
    }
    true
}

fn solve(level: i32, er: i32, ec: i32, pr: i32, pc: i32, 
         rr: [i32; 8], cc: [i32; 8], b: &mut [[char; 5]; 5]) -> i32{
    if level > 10 { return 2000; }
    if checkboard(&b) { return level; }
    let mut ans = 2000;
    for (rri, cci) in rr.iter().zip(cc.iter()) {
        let nr = er + rri; 
        let nc = ec + cci;
        if ((nr >= 0 && nr < 5 && nc >= 0 && nc < 5)) && (nr != pr || nc != pc) {
            let mut nb = b.clone();
            let newv1 = &b[er as usize][ec as usize];
            let newv2 = &b[nr as usize][nc as usize]; 
            nb[er as usize][ec as usize] = *newv2;
            nb[nr as usize][nc as usize] = *newv1; 

            let t = solve(level + 1, nr as i32, nc as i32, er, ec, rr, cc, &mut nb);
            ans = min(ans, t);
        }
    }
    ans
}

fn main(){
    // Create a reader from stdin
    let mut input = InputReader::new();

    // Create a writer from stdout
    let mut output = OutputWriter::new();

    let rr = [-2, -2,  2, 2, -1,  1, -1, 1];
    let cc = [-1,  1, -1, 1, -2, -2,  2, 2];
    let mut b: [[char; 5]; 5]  = [['0'; 5]; 5];
    let t = input.next::<i32>();
    let _ : String = input.next_line();
    
    for _ in 0..t {
        let mut r : i32 = 0;
        let mut c : i32 = 0;
        for i in 0..5 {
            let s : String = input.next_line();
            for j in 0..5 {
                b[i as usize][j as usize] = s.chars().nth(j).unwrap();
                if b[i as usize][j as usize] == ' ' { r=i as i32; c=j as i32; }
            }
        }
        let ans = solve(0, r, c, -1, -1, rr, cc, &mut b);
        if ans > 11 {
            output.println("Unsolvable in less than 11 move(s).");
        } else {
            output.println(format!("Solvable in {} move(s).", ans));
        }
    }
}
