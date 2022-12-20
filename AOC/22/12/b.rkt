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

(define (solve starts end locations)
  (define graph (unweighted-graph/directed '()))
  (for* ([(l h) (in-hash locations)]
         [lp (in-list (moves l))])
    (define new-h (hash-ref locations lp #f))
    (when (and new-h (equal? (add1 h) new-h))
      (add-directed-edge! graph l lp))
    (when (and new-h (<= new-h h))
      (add-directed-edge! graph l lp)))
  (argmin identity
          (for/list ([start (in-list starts)])
            (define-values (dists p)
              (bfs graph start))
            (hash-ref dists end))))

(let ([in (open-input-file "input.text")]
      [starts '()]
      [end #f]
      [table (make-hash)])
  (for* ([(row r) (in-indexed (in-lines in))]
         [(h c) (in-indexed (string->list row))])
    (define (include q)
      (define v (char->integer q))
      (hash-set! table (cons r c) v))
    (cond [(or (char=? h #\S) (char=? h #\a))
           (set! starts (cons (cons r c) starts))
           (include #\a)]
          [(char=? h #\E)
           (set! end (cons r c))
           (include #\z)]
          [else
           (include h)]))
  (solve starts end table))
