// Gavin Gray AOC 2021

#![feature(fn_traits)]
#![feature(stdin_forwarders)]
#![feature(unboxed_closures)]
use std::cmp;
use std::collections::HashMap;
use std::hash::Hash;
use std::io;

static TO_WIN: u64 = 21;

#[derive(Clone, Copy, Hash)]
struct Player {
    pos: u64,
    score: u64,
}

impl PartialEq for Player {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.score == other.score
    }
}

impl Eq for Player {}

// start memoization code
// credit to: https://medium.com/swlh/on-memoization-291fd1dd924
struct HashCache<A, R> {
    data: HashMap<A, R>,
    func: fn(&mut HashCache<A, R>, A) -> R,
}

impl<A, R> HashCache<A, R>
where
    A: Eq + Hash,
{
    fn from_func(func: fn(&mut Self, A) -> R) -> Self {
        HashCache {
            data: HashMap::new(),
            func: func,
        }
    }
}

impl<A, R> FnMut<(A,)> for HashCache<A, R>
where
    A: Eq + Hash + Clone,
    R: Clone,
{
    extern "rust-call" fn call_mut(&mut self, args: (A,)) -> Self::Output {
        let arg = args.0;
        match self.data.get(&arg).map(|x| x.clone()) {
            Some(result) => result,
            None => {
                let result = (self.func.clone())(self, arg.clone());
                self.data.insert(arg, result.clone());
                result
            }
        }
    }
}

// We need to implement `FnOnce` to implement `FnMut`.
impl<A, R> FnOnce<(A,)> for HashCache<A, R>
where
    A: Eq + Hash + Clone,
    R: Clone,
{
    type Output = R;
    extern "rust-call" fn call_once(mut self, args: (A,)) -> Self::Output {
        self.call_mut(args)
    }
}
// end memoization code

fn md(n: u64, p: u64) -> u64 {
    ((n - 1) % p) + 1
}

fn flipadd(t1: (u64, u64), t2: (u64, u64)) -> (u64, u64) {
    (t1.0 + t2.1, t1.1 + t2.0)
}

fn forward() -> impl std::iter::Iterator<Item = u64> {
    vec![
        3, 4, 5, 4, 5, 6, 5, 6, 7, 4, 5, 6, 5, 6, 7, 6, 7, 8, 5, 6, 7, 6, 7, 8, 7, 8, 9,
    ]
    .into_iter()
}
fn play<F>(recurse: &mut F, player: (Player, Player)) -> (u64, u64)
where
    F: FnMut((Player, Player)) -> (u64, u64),
{
    if player.0.score >= TO_WIN {
        return (1, 0);
    } else if player.1.score >= TO_WIN {
        return (0, 1);
    }

    forward()
        .map(|f| {
            let nv = md(player.0.pos + f, 10);
            Player {
                pos: nv,
                score: player.0.score + nv,
            }
        })
        .map(|np1| recurse((player.1, np1)))
        .fold((0, 0), flipadd)
}

fn main() {
    let starts: Vec<u64> = io::stdin()
        .lines()
        .map(|l| l.unwrap())
        .map(|s| s.split_whitespace().last().unwrap().parse::<u64>().unwrap())
        .collect();
    let mut cache = HashCache::from_func(play);
    let (w1, w2) = play(
        &mut cache,
        (
            Player {
                pos: starts[0],
                score: 0,
            },
            Player {
                pos: starts[1],
                score: 0,
            },
        ),
    );
    println!("{}", cmp::max(w1, w2));
}
