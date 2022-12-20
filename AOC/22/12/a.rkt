#lang racket

(require graph)

(define (moves l)
  (for/list ([d (in-list '((0 . 1)
                           (0 . -1)
                           (1 . 0)
                           (-1 . 0)))])
    (match (cons l d)
      [(cons (cons x y) (cons a b))
       (cons (+ x a) (+ y b))])))

(define (solve start end locations)
  (define graph (unweighted-graph/directed '()))
  (for* ([(l h) (in-hash locations)]
         [lp (in-list (moves l))])
    (define new-h (hash-ref locations lp #f))
    (when (and new-h (equal? (add1 h) new-h))
      (add-directed-edge! graph l lp))
    (when (and new-h (<= new-h h))
      (add-directed-edge! graph l lp)))
  (define-values (dists p)
    (bfs graph start))
  (hash-ref dists end))

(let ([in (open-input-file "input.text")]
      [start #f]
      [end #f]
      [table (make-hash)])
  (for* ([(row r) (in-indexed (in-lines in))]
         [(h c) (in-indexed (string->list row))])
    (define (include q)
      (define v (char->integer q))
      (hash-set! table (cons r c) v))
    (cond [(char=? h #\S)
           (set! start (cons r c))
           (include #\a)]
          [(char=? h #\E)
           (set! end (cons r c))
           (include #\z)]
          [else (include h)]))
  (solve start end table))
