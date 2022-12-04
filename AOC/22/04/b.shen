// Gavin Gray, AOC 2022 day 4

(define overlap?
  A B X _ -> (and (<= A X) (<= X B)))

(define count-them
  [] N -> N
  [A B X Y | REST] N -> (count-them REST (+ N 1)) where (overlap? A B X Y)
  [A B X Y | REST] N -> (count-them REST (+ N 1)) where (overlap? X Y A B)
  [_ _ _ _ | REST] N -> (count-them REST N))

(count-them
 (map abs (filter number? (read-file "input.text")))
 0)
