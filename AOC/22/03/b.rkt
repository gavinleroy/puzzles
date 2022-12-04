#lang racket

(require racket/match
         racket/set)

(define (points c)
  (define an (char->integer #\a))
  (define An (char->integer #\A))
  (cond
    [(and (char<=? #\a c)
          (char<=? c #\z))
     (+ 1 (- (char->integer c) an))]
    [else (+ 27 (- (char->integer c) An))]))

(define (score l)
  (define f (car l))
  (define s (cadr l))
  (define t (caddr l))
  (define m (set-intersect f s t))
  (points (set-first m)))

(define (group-3 l)
  (define set-it
    (lambda (s) (list->set (string->list s))))
  (match l
    [(list) (list)]
    [(list a b c r ...)
     (cons
      (list
       (set-it a)
       (set-it b)
       (set-it c))
      (group-3 r))]))

(let ([in (open-input-file "input.text")])
  (foldl
   + 0
   (map
    score
    (group-3 (sequence->list (in-lines in))))))
