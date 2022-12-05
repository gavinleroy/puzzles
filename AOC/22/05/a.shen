// Gavin Gray, AOC 2022 day 5
//
// TODO: I really dislike when my solution doesn't parse the entire
// input file. Below, you must paste the initial structure and remove
// it from the input file.

(define move
  [] State -> State
  [N From To | Rest] State ->
    (let Froml (<-vector State From)
         Tol (<-vector State To)
         V1 (vector-> State From (drop N Froml))
         V2 (vector-> State To (append (reverse (take N Froml)) Tol))
      (move Rest State)))

(list->string
 (map str
      (vector->list
       (vector.map
               hd
               (move (filter number? (read-file "input.text"))
                     (@v [L C G M Q]
                         [G H F T C L D R]
                         [R W T M N F J V]
                         [P Q V D F J]
                         [T B L S M F N]
                         [P D C H V N R]
                         [T C H]
                         [P H N Z V J S G]
                         [G H F Z] <>))))))
