#lang racket

(require racket/list)

(struct neutral ())
(define N (neutral))

;; Gross ...
(define (ordered? a b)
  (define (order? a b)
    (cond [(and (integer? a) (integer? b))
           (if (= a b)
               N
               (< a b))]
          [(and (list? a) (list? b))
           (ordered? a b)]
          [(and (list? a) (integer? b))
           (order? a (list b))]
          [(and (integer? a) (list? b))
           (order? (list a) b)]))
  (let loop ([l a] [r b])
    (cond [(and (null? l) (not (null? r))) #t]
          [(and (not (null? l)) (null? r)) #f]
          [(and (null? l) (null? r)) N]
          [else
           (define o (order? (car l) (car r)))
           (if (eq? o N)
               (loop (cdr l) (cdr r))
               o)])))

(define (solve ls)
  (define-values (f s)
    (partition (let ([v #f])
                 (lambda _ (set! v (xor v #t)) v))
               ls))
  (for/fold ([acc 0])
            ([(v i) (in-indexed (map ordered? f s))])
    (if v (+ acc (add1 i)) acc)))

(let ([in (open-input-file "input.text")])
  (solve
   (filter-not
    eof-object?
    (for/list ([row (in-lines in)])
      (define sin (open-input-string (string-replace row "," " ")))
      (read sin)))))
