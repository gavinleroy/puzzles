use std::io;

fn main() {

    let mut n = String::new();
    io::stdin().read_line(&mut n)
        .expect("fail");
//    let _n: u32 = match n.trim().parse() {
//        Ok(num) => num,
//        Err(_) => 42,
//    };
    let mut vals = String::new();
    io::stdin().read_line(&mut vals)
        .expect("fail");
    let vals: std::vec::Vec<i32> = vals.trim().split_whitespace()
        .map(|st| match st.parse() { Ok(num) => num, Err(_) => 0 })
        .collect::<Vec<i32>>();
    let ans = vals.iter()
        .fold(0, |acc, x| acc + match x { std::i32::MIN..=-1 => 1, _ => 0, } );
    println!("{}", ans);
}

