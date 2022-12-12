#lang racket

(struct monkey (items op t? next)
  #:transparent)

(define (parse-monkey ls)
  (define start-items
    (let* ([splt (cadr (string-split (cadr ls) ":"))]
           [sns (string-split splt ",")])
      (map (compose string->number string-trim) sns)))
  (define operation
    (let* ([rhs (cadr (string-split (caddr ls) "="))]
           [ops (string-split rhs #:trim? #t)]
           [syms (map (lambda (o)
                        (define n (string->number o))
                        (if n n (string->symbol o))) ops)]
           [expr (list (cadr syms) (car syms) (caddr syms))])
      (eval #`(lambda (old) #,expr))))
  (define test?
    (let* ([splt (string-split (cadddr ls))]
           [p (string->number (last splt))])
      (lambda (n) (zero? (modulo n p)))))
  (define get-next
    (let* ([last-n (lambda (s) (string->number (last (string-split s))))]
           [nt (last-n (car (cddddr ls)))]
           [nf (last-n (cadr (cddddr ls)))])
      (lambda (b) (if b nt nf))))
  (monkey start-items operation test? get-next))

(define (throw-to v mi monkeys)
  (define to-m (vector-ref monkeys mi))
  (define is (monkey-items to-m))
  (vector-set! monkeys mi
               (struct-copy monkey to-m
                            [items (append is (list v))])))

(define (solve monkeys)
  (define-values (incr shenanigans)
    (let ([inspected (make-vector (vector-length monkeys))])
      (values (lambda (i)
                (define v (vector-ref inspected i))
                (vector-set! inspected i (add1 v)))
              (lambda ()
                (define vp (vector-sort inspected >))
                (* (vector-ref vp 0)
                   (vector-ref vp 1))))))
  (define div-3-floor (lambda (n) (floor (/ n 3))))
  (for ([_ (in-range 20)])
    (for ([(mnk i) (in-indexed monkeys)])
      (for ([item (in-list (monkey-items mnk))])
        (incr i)
        (define value (div-3-floor
                       ((monkey-op mnk) item)))
        (define to ((monkey-next mnk)
                    ((monkey-t? mnk) value)))
        (throw-to value to monkeys))
      (vector-set! monkeys i (struct-copy monkey mnk [items '()]))))
  (shenanigans))

(let ([in (open-input-file "input.text")])
  (define monkeys
    (for/foldr ([buffer '()]
              [monkeys '()]
              #:result (list->vector (list* (parse-monkey buffer) monkeys)))
              ([l (in-lines in)])
              (if (string=? "" (string-trim l))
                  (values '() (cons (parse-monkey buffer) monkeys))
                  (values (cons l buffer) monkeys))))
  (displayln (solve monkeys)))
