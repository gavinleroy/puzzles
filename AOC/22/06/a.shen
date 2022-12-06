// Gavin Gray, AOC 2022 day 6

(define find-start
  L N -> N where (set? (take 4 L))
  L N -> (find-start (tl L) (+ N 1)))

(find-start (string->list (read-file-as-string "input.text")) 4)
