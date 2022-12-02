// Gavin Gray, AOC 2022 day 2
// Shen, currently untyped because I'm lazy ...

(define correct
  X -> (n->string (- (string->n X) 23)) where (element? X ["X" "Y" "Z"])
  X -> X)

(define outcome
  "A" "B" -> 6
  "B" "C" -> 6
  "C" "A" -> 6
  X X     -> 3
  X Y     -> 0)

(define played
  "A" -> 1
  "B" -> 2
  "C" -> 3)

(define play-matches
  [] -> 0
  [X Y | REST] -> (+ (outcome X Y) (played Y) (play-matches REST)))

(play-matches (map (/. S (correct (str S))) (read-file "input.text")))
