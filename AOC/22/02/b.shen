// Gavin Gray, AOC 2022 day 2
// Shen, currently untyped because I'm lazy ...

(define parse
  X -> (- (string->n X) (+ 23 65)) where (element? X ["X" "Y" "Z"])
  X -> (- (string->n X) 65))

(define adj X -> (+ 1 (mod X 3)))

(define outcome
  X 0 -> (adj (- X 1))
  X 1 -> (+ (adj X) 3)
  X 2 -> (+ (adj (+ X 1)) 6))

(define play-matches
  [] -> 0
  [X Y | REST] -> (+ (outcome X Y) (play-matches REST)))

(play-matches (map (/. S (parse (str S))) (read-file "input.text")))
