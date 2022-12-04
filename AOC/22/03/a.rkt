#lang racket

(define (points c)
  (define an (char->integer #\a))
  (define An (char->integer #\A))
  (cond
    [(and (char<=? #\a c)
          (char<=? c #\z))
     (+ 1 (- (char->integer c) an))]
    [else (+ 27 (- (char->integer c) An))]))

(define (score l)
  (define c (length l))
  (define f (take l (/ c 2)))
  (define s (drop l (/ c 2)))
  (points (findf (lambda (e) (member e s)) f)))

(let ([in (open-input-file "input.text")])
  (for/fold ([acc 0])
            ([l (in-lines in)])
    (+ acc (score (string->list l)))))
