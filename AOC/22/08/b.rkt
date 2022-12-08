#lang racket

(require racket/hash)

;; Gavin Gray, AOC 22 day 8

(define-syntax-rule (row-horizontal row start end step)
  (for/hash ([i (in-range start end step)])
    (define h (vector-ref row i))
    (define distance
      (for/fold ([dst 0])
                ([d (in-range (+ i step) end step)]
                 #:break (= d (vector-length row))
                 #:do ((define nh (vector-ref row d)))
                 #:final (h . <= . nh))
        (add1 dst)))
    (values i distance)))

(define-syntax-rule (combine-hash h1 h2)
  (hash-union h1 h2 #:combine *))

(define (solve-horizontal board #:make [posn cons] #:start [start (make-immutable-hash)])
  (for/fold ([hsh start])
            ([(row i) (in-indexed board)])
    (define t (vector-length row))
    (define l (row-horizontal row (sub1 t) -1 -1))
    (define r (row-horizontal row 0 t 1))
    (define vs (combine-hash l r))
    (define local (for/hash ([(j d) (in-hash vs)])
                    (values (posn i j) d)))
    (combine-hash hsh local)))

(define (solve board)
  (define (vector-transpose m)
    (apply vector-map vector (vector->list m)))
  (define h1 (solve-horizontal board))
  (define h2 (solve-horizontal
              (vector-transpose board)
              #:make (lambda (f s) (cons s f))))
  (define h3 (combine-hash h1 h2))
  (argmax identity (hash-values h3)))

(let ([in (open-input-file "input.text")]
      [explode (lambda (s)
                 (map (compose (curryr - (char->integer #\0)) char->integer)
                      (string->list s)))])
  (define board
    (for/vector ([line (in-lines in)])
      (for/vector ([d (explode line)])
        d)))
  (solve board))
