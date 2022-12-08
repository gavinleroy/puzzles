#lang racket

;; Gavin Gray, AOC 22 day 8

(define-syntax-rule (row-horizontal row start end step)
  (for/fold ([h -1] [idxs '()]
                    #:result idxs)
            ([i (in-range start end step)])
    (define at (vector-ref row i))
    (if (at . > . h)
        (values at (cons i idxs))
        (values h idxs))))

(define (solve-horizontal board #:make [posn cons])
  (define res (for/list ([(row i) (in-indexed board)])
                (define t (sub1 (vector-length row)))
                (define jls (row-horizontal row 0 t 1))
                (define jrs (row-horizontal row t 0 -1))
                (set-union
                 (apply set (map (curry posn i) jls))
                 (apply set (map (curry posn i) jrs))
                 (set (posn i 0)
                      (posn i t)))))
  (apply set-union (flatten res)))

(define (solve board)
  (define (vector-transpose m)
    (apply vector-map vector (vector->list m)))
  (set-count
   (set-union
    (solve-horizontal board)
    (solve-horizontal
     (vector-transpose board)
     #:make (lambda (f s) (cons s f))))))

(let ([in (open-input-file "input.text")]
      [explode (lambda (s)
                 (map (compose (curryr - (char->integer #\0)) char->integer)
                      (string->list s)))])
  (define board
    (for/vector ([line (in-lines in)])
      (for/vector ([d (explode line)])
        d)))
  (solve board))
